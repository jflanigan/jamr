#!/bin/bash
# If you want to reproduce the entire analysis, run this script
rm -r data
mkdir data
./cpAndSplitOutputFromQin.sh
./cpRefFiles.sh
./createPhraseHieroRef.sh

# if you are using the stanford tagger:
#cd data
#../Tokenize.py
#../tagMT.sh
#cd ..
#rm hiero phrase ref ref1 ref2 ref3 ref4
#./taggerLinks.sh

# alternatively run the stanford parser to produce tags
cd data
# create the tags
../stanfordparser_tags.sh
# change the tag separator to '_'
../converttags.sh
cd ..
# create links that point to the tagged files
./parserLinks.sh

rm -r output
mkdir output
echo "Running CountTheA.py..."
./CountTheA.py

