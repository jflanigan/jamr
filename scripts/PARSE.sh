#!/bin/bash -e

# usage: ./PARSE.sh < input_file > output_file 2> output_file.err

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
  --stage1-concept-table "${MODEL_DIR}/conceptTable.train" \
  --stage1-features "bias,length,fromNERTagger,conceptGivenPhrase" \
  --stage1-weights "${STAGE1_WEIGHTS}" \
  --stage2-decoder LR \
  --stage2-features "rootConcept,rootDependencyPathv1,bias,typeBias,self,fragHead,edgeCount,distance,logDistance,posPathv3,dependencyPathv4,conceptBigram" \
  --stage2-labelset "${JAMR_HOME}/resources/labelset" \
  --stage2-weights "${STAGE2_WEIGHTS}" \
  --dependencies "${INPUT}.deps" \
  --ner "${INPUT}.IllinoisNER" \
  --tok "${INPUT}.tok" \
  --ignore-parser-errors \
  --output-format AMR,nodes,edges,root \
  -v 0 \
  < "${INPUT}"

rm /tmp/jamr-$$.snt /tmp/jamr-$$.snt.tok /tmp/jamr-$$.snt.deps /tmp/jamr-$$.snt.IllinoisNER

