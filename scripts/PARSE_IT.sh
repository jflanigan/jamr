#!/bin/bash
set -eo pipefail

# Usage: ./PARSE_IT.sh input_file
# Output will be in input_file.parsed

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
    echo 'Usage: PARSE_IT.sh input_file'
    exit 1
fi

INPUT="$(cd "$(dirname "$1")"; pwd)"/"$(basename $1)"
OUTPUT="$INPUT.parsed"

STAGE1_WEIGHTS="${MODEL_DIR}/stage1-weights"
STAGE2_WEIGHTS="${MODEL_DIR}/stage2-weights.iter5"

#### Tokenize ####

cat "$INPUT" | sed 's/   */ /g' | "${CDEC}/corpus/tokenize-anything.sh" > "$INPUT.tok"

#### NER ####

inputfile="$INPUT"
outputfile="$INPUT.IllinoisNER.tmp"
configfile="$JAMR_HOME/scripts/preprocessing/IllinoisNER.config"
cpath="$ILLINOIS_NER_JAR:$ILLINOIS_NER/target/classes:$ILLINOIS_NER/target/dependency/*"
temp="$inputfile.tmp"
cat "$inputfile" | sed $'s/$/\\\n####\\\n/' > "$temp" # see http://superuser.com/questions/307165/newlines-in-sed-on-mac-os-x
pushd "$ILLINOIS_NER"
java -classpath  ${cpath} -Xmx8g edu.illinois.cs.cogcomp.LbjNer.LbjTagger.NerTagger -annotate ${temp} ${outputfile} ${configfile}
popd
# The awk command drops the last line, see http://askubuntu.com/questions/475694/awk-command-to-print-all-the-lines-except-the-last-three-lines
cat "$outputfile" | sed $'s/ #### /\\\n/g' | "$SCALA" "$JAMR_HOME/src/IllinoisNERConvert" | awk '{l[NR] = $0} END {for (i=1; i<=NR-1; i++) print l[i]}' > "$INPUT.IllinoisNER"
rm "$outputfile"
rm "$temp"

#### Dependencies ####

"${JAMR_HOME}/run" RunStanfordParser < "$INPUT" > "$INPUT.deps"

#### Parse ####

${JAMR_HOME}/run AMRParser \
  --stage1-concept-table "${MODEL_DIR}/conceptTable.train" \
  --stage1-weights "${STAGE1_WEIGHTS}" \
  --stage2-weights "${STAGE2_WEIGHTS}" \
  --dependencies "${INPUT}.deps" \
  --ner "${INPUT}.IllinoisNER" \
  --tok "${INPUT}.tok" \
  -v 0 \
  ${PARSER_OPTIONS} \
  < "${INPUT}" \
  > "${OUTPUT}" \
  2> "${OUTPUT}.err"

rm "${INPUT}.deps" "${INPUT}.IllinoisNER" "${INPUT}.tok"
