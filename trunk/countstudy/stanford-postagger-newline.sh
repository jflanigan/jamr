#!/bin/sh
#
# usage: ./stanford-postagger.sh model textFile
#  e.g., ./stanford-postagger.sh models/left3words-wsj-0-18.tagger sample-input.txt

# This is a modification of the original script that comes with the tagger.
# By default, the tagger automatically does sentence segmentation.
# This is turned off with the '-tokenize false' option

cd ~/tools/stanford-postagger-full-2010-05-26/

java -mx300m -cp 'stanford-postagger.jar:' edu.stanford.nlp.tagger.maxent.MaxentTagger -debug -tokenize false -model $1 -textFile $2
