#!/bin/bash
set -eo pipefail

# usage: ./GENERATE.sh input_file

if [ -z "$JAMR_HOME" ]; then
    echo 'Error: please source config script'
    exit 1
fi

if [ ! -f "${MODEL_DIR}/rule-model" ]; then
    echo "Error: cannot find weights file ${MODEL_DIR}/rule-model."
    echo "Please train generator or download and extract weights file to ${MODEL_DIR}"
    exit 1
fi

INPUT="$(cd "$(dirname "$1")"; pwd)"/"$(basename $1)"

#### Sentence level grammars ####

echo ' ### Creating sentence level grammars ###' >&2

mkdir -p "${INPUT}_grammars"

"${JAMR_HOME}/run" Generate.SentenceLevelGrammars \
    --weights "${MODEL_DIR}/rule-model" \
    --rule-inventory "${MODEL_DIR}/extract-rules" \
    --kbest 100 \
    --no-reference \
    --output "${INPUT}_grammars" \
    -v 0 \
    ${GENERATOR_OPTIONS} \
    < "${INPUT}" \
    > "${INPUT}_grammars.snt" \
    2> "${INPUT}_grammars.err"


#### Decoding ####

echo ' ### Running cdec decoder ###' >&2

"${JAMR_HOME}/tools/cdec/decoder/cdec" \
    -c "${JAMR_HOME}/scripts/generator-training/cdec.ini" \
    --show_config \
    -w "${MODEL_DIR}/dpmert/weights" \
    -i "${INPUT}_grammars.snt" \
    > "${INPUT}.out" \
    2> "${INPUT}.err"

rm -r "${INPUT}_grammars" "${INPUT}_grammars.snt" "${INPUT}_grammars.err" 
