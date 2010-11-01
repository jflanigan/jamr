#!/bin/bash
# If you want to reproduce the entire analysis, run this script
mkdir data
./cpAndSplitOutputFromQin.sh
./cpRefFiles.sh
./createPhraseHieroRef.sh
./Tokenize.py
./tagMT.sh
./CountTheA.py

