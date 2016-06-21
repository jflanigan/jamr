#!/bin/bash
set -ueo pipefail

# Usage: ./EVAL.sh gold_amr_file
# Output will be in $MODEL_DIR/gold_amr_file.parsed

if [ -z "$JAMR_HOME" ]; then
    echo 'Error: please source config script'
    exit 1
fi

if [ ! -f "${MODEL_DIR}/stage1-weights" ]; then
    echo "Error: cannot find weights file ${MODEL_DIR}/stage1-weights."
    echo "Please train JAMR or download and extract weights file to ${MODEL_DIR}"
    exit 1
fi

if [ -z "$1" ]; then
    echo 'Usage: EVAL.sh gold_amr_file (iteration)'
    exit 1
fi

TEST_FILE="$(cd "$(dirname "$1")"; pwd)"/"$(basename $1)"

STAGE1_WEIGHTS="${MODEL_DIR}/stage1-weights"

if [ -z "$2" ]; then
    STAGE2_WEIGHTS="${MODEL_DIR}/stage2-weights"
    OUTPUT="${MODEL_DIR}/$(basename $1)"
else
    STAGE2_WEIGHTS="${MODEL_DIR}/stage2-weights.iter$2"
    OUTPUT="${MODEL_DIR}/$(basename $1).iter$2"
fi

INPUT="$OUTPUT.snt"

#### Parse ####

${JAMR_HOME}/run AMRParser \
  --stage1-concept-table "${MODEL_DIR}/conceptTable.train" \
  --stage1-weights "${STAGE1_WEIGHTS}" \
  --stage2-weights "${STAGE2_WEIGHTS}" \
  --dependencies "${OUTPUT}.deps" \
  --training-data "${OUTPUT}.aligned.no_opN" \
  --ner "${OUTPUT}.IllinoisNER" \
  --tok "${OUTPUT}.snt.tok" \
  -v 0 \
  --stage1-eval \
  ${PARSER_OPTIONS} \
  < "${INPUT}" \
  > "${OUTPUT}.parsed" \
  2> "${OUTPUT}.parsed.err"

${JAMR_HOME}/run AMRParser \
  --stage1-oracle \
  --stage1-concept-table "${MODEL_DIR}/conceptTable.train" \
  --stage1-weights "${STAGE1_WEIGHTS}" \
  --stage2-weights "${STAGE2_WEIGHTS}" \
  --dependencies "${OUTPUT}.deps" \
  --training-data "${OUTPUT}.aligned.no_opN" \
  --ner "${OUTPUT}.IllinoisNER" \
  --tok "${OUTPUT}.snt.tok" \
  -v 0 \
  ${PARSER_OPTIONS} \
  < "${INPUT}" \
  > "${OUTPUT}.parsed-gold-concepts" \
  2> "${OUTPUT}.parsed-gold-concepts.err"

#rm "$OUTPUT.deps" "$OUTPUT.IllinoisNER" "$OUTPUT.tok" "$OUTPUT.snt.tok" "$OUTPUT.snt" "$OUTPUT.aligned.no_opN"

echo ""
echo "  ----- Evaluation: Smatch (all stages) -----" | tee "$OUTPUT.results"
"${JAMR_HOME}/scripts/smatch_v1_0/smatch_modified.py" --pr -f "${OUTPUT}.parsed" "$TEST_FILE" 2>&1 | tee -a "$OUTPUT.results"
echo "" | tee -a "$OUTPUT.results"
echo "  ----- Evaluation: Smatch (gold concept ID) -----" | tee -a "$OUTPUT.results"
"${JAMR_HOME}/scripts/smatch_v1_0/smatch_modified.py" --pr -f "${OUTPUT}.parsed-gold-concepts" "$TEST_FILE" 2>&1 | tee -a "$OUTPUT.results"
echo "" | tee -a "$OUTPUT.results"
echo "  ----- Evaluation: Spans -----" | tee -a "$OUTPUT.results"
tail -n 3 "$OUTPUT.parsed.err" | tee -a "$OUTPUT.results"

