#!/bin/bash -e

# usage: ./ALIGN.sh < input_file > output_file 2> output_file.err

cat > /tmp/jamr-$$

INPUT=/tmp/jamr-$$

#### Tokenize ####

echo ' ### Tokenizing ###' >&2

cat "${INPUT}" | grep '::snt ' | sed 's/^# ::snt //' | "${CDEC}/corpus/tokenize-anything.sh" > "${INPUT}.snt.tok"

${JAMR_HOME}/run CorpusTool < "${INPUT}" --tokenized "${INPUT}.snt.tok" > "${INPUT}.tok"

#### Align ####

echo ' ### Running aligner ###' >&2

# -v 1 will output spans
${JAMR_HOME}/run Aligner -v 0 < "${INPUT}.tok" 

rm /tmp/jamr-$$ /tmp/jamr-$$.tok /tmp/jamr-$$.snt.tok

