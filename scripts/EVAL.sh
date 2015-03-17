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
    echo 'Usage: EVAL.sh gold_amr_file'
    exit 1
fi

TEST_FILE="$(cd "$(dirname "$1")"; pwd)"/"$(basename $1)"
OUTPUT="${MODEL_DIR}/$(basename $1)"
INPUT="$OUTPUT.snt"

STAGE1_WEIGHTS="${MODEL_DIR}/stage1-weights"
STAGE2_WEIGHTS="${MODEL_DIR}/stage2-weights.iter5"

#### Tokenize ####

cat "$TEST_FILE" | grep '^# ::snt' | sed 's/^# ::snt //' > "$INPUT"
cat "$INPUT" | sed 's/   */ /g' | "${CDEC}/corpus/tokenize-anything.sh" > "$OUTPUT.snt.tok"
${JAMR_HOME}/run CorpusTool < "$TEST_FILE" --tokenized "$OUTPUT.snt.tok" > "$OUTPUT.tok"

#### NER ####

inputfile="$INPUT"
outputfile="$OUTPUT.IllinoisNER.tmp"
tmp="$OUTPUT.tmp"
configfile="$JAMR_HOME/scripts/preprocessing/IllinoisNER.config"
cpath="$ILLINOIS_NER_JAR:$ILLINOIS_NER/target/classes:$ILLINOIS_NER/target/dependency/*"
cat $inputfile | sed $'s/$/\\\n####\\\n/' > "$tmp" # see http://superuser.com/questions/307165/newlines-in-sed-on-mac-os-x
pushd "$ILLINOIS_NER"
java -classpath  ${cpath} -Xmx8g edu.illinois.cs.cogcomp.LbjNer.LbjTagger.NerTagger -annotate "$tmp" "$outputfile" "$configfile"
popd
# The awk command drops the last line, see http://askubuntu.com/questions/475694/awk-command-to-print-all-the-lines-except-the-last-three-lines
cat "$outputfile" | sed $'s/ #### /\\\n/g' | "$SCALA" "$JAMR_HOME/src/IllinoisNERConvert" | awk '{l[NR] = $0} END {for (i=1; i<=NR-1; i++) print l[i]}' > "$OUTPUT.IllinoisNER"
rm "$outputfile"
rm "$tmp"

#### Dependencies ####

"${JAMR_HOME}/run" RunStanfordParser < "$INPUT" > "$OUTPUT.deps"

#### Align gold data ###
${JAMR_HOME}/run Aligner -v 1 < "$OUTPUT.tok" 2>&1 | egrep '^#|^ |^\(|^$' | sed 's/:op[^ ]*/:op/g' > "$OUTPUT.aligned.no_opN"

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

rm "$OUTPUT.deps" "$OUTPUT.IllinoisNER" "$OUTPUT.tok" "$OUTPUT.snt.tok" "$OUTPUT.snt" "$OUTPUT.aligned.no_opN"

echo ""
echo "  ----- Evaluation: Smatch (all stages) -----" | tee "$OUTPUT.results"
"${JAMR_HOME}/scripts/smatch_v1_0/smatch_modified.py" --pr -f "${OUTPUT}.parsed" "$TEST_FILE" 2>&1 | tee -a "$OUTPUT.results"
echo "" | tee -a "$OUTPUT.results"
echo "  ----- Evaluation: Smatch (gold concept ID) -----" | tee -a "$OUTPUT.results"
"${JAMR_HOME}/scripts/smatch_v1_0/smatch_modified.py" --pr -f "${OUTPUT}.parsed-gold-concepts" "$TEST_FILE" 2>&1 | tee -a "$OUTPUT.results"
echo "" | tee -a "$OUTPUT.results"
echo "  ----- Evaluation: Spans -----" | tee -a "$OUTPUT.results"
tail -n 3 "$OUTPUT.parsed.err" | tee -a "$OUTPUT.results"

