#!/bin/bash
# If you want to reproduce the entire analysis, run this script
mkdir data
./cpAndSplitOutputFromQin.sh
./cpRefFiles.sh
./createPhraseHieroRef.sh
cd data
../Tokenize.py
../tagMT.sh
cd ..
./createLinks.sh

mkdir output
echo "Running CountTheA.py..."
./CountTheA.py

