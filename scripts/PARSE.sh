#!/bin/bash
set -eo pipefail

# usage: ./PARSE.sh < input_file > output_file 2> output_file.err

if [ -z "$JAMR_HOME" ]; then
    echo 'Error: please source config script'
    exit 1
fi

if [ ! -f "${MODEL_DIR}/stage1-weights" ]; then
    echo "Error: cannot find weights file ${MODEL_DIR}/stage1-weights."
    echo "Please train JAMR or download and extract weights file to ${MODEL_DIR}"
    exit 1
fi

cat > /tmp/jamr-$$.snt

INPUT=/tmp/jamr-$$.snt

STAGE1_WEIGHTS="${MODEL_DIR}/stage1-weights"
STAGE2_WEIGHTS="${MODEL_DIR}/stage2-weights.iter5"

#### Tokenize ####

echo ' ### Tokenizing input ###' >&2

cat "$INPUT" | sed 's/   */ /g' | "${CDEC}/corpus/tokenize-anything.sh" > "$INPUT.tok"

#### NER ####

echo ' ### Running NER system ###' >&2

inputfile="$INPUT"
outputfile="$INPUT.IllinoisNER.tmp"
configfile="$JAMR_HOME/scripts/preprocessing/IllinoisNER.config"
cpath="$ILLINOIS_NER_JAR:$ILLINOIS_NER/target/classes:$ILLINOIS_NER/target/dependency/*"
cat $inputfile | sed $'s/$/\\\n####\\\n/' > $inputfile.tmp  # see http://superuser.com/questions/307165/newlines-in-sed-on-mac-os-x
pushd "$ILLINOIS_NER" >&2
java -classpath  ${cpath} -Xmx8g edu.illinois.cs.cogcomp.LbjNer.LbjTagger.NerTagger -annotate $inputfile.tmp ${outputfile} ${configfile} 1>&2
popd >&2
cat "$outputfile" | sed $'s/ #### /\\\n/g' | "$SCALA" "$JAMR_HOME/src/IllinoisNERConvert" | awk 'NR > 2' > "$INPUT.IllinoisNER"
rm "$outputfile"
rm "$inputfile".tmp

#### Dependencies ####

echo ' ### Running dependency parser ###' >&2

"${JAMR_HOME}/run" RunStanfordParser < "$INPUT" > "$INPUT.deps"

#### Parse ####

echo ' ### Running JAMR ###' >&2

${JAMR_HOME}/run AMRParser \
  ${PARSER_OPTIONS} \
  --stage1-concept-table "${MODEL_DIR}/conceptTable.train" \
  --stage1-weights "${STAGE1_WEIGHTS}" \
  --stage2-weights "${STAGE2_WEIGHTS}" \
  --dependencies "${INPUT}.deps" \
  --ner "${INPUT}.IllinoisNER" \
  --tok "${INPUT}.tok" \
  --ignore-parser-errors \
  --output-format AMR,nodes,edges,root \
  -v 0 \
  < "${INPUT}"

rm /tmp/jamr-$$.snt /tmp/jamr-$$.snt.tok /tmp/jamr-$$.snt.deps /tmp/jamr-$$.snt.IllinoisNER

