#!/bin/csh -f
#
# Runs the English PCFG parser on one or more files, printing trees only
# usage: ./lexparser.csh fileToparse+
#
#set scriptdir=`dirname $0`

cd ~/tools/stanford-parser-2010-08-20

java -mx500m -cp "stanford-parser.jar:" edu.stanford.nlp.parser.lexparser.LexicalizedParser -sentences newline -outputFormat "wordsAndTags" englishPCFG.ser.gz $*
