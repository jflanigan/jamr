#!/bin/bash

# Convert the tags produced the stanford parser ('/') into '_'
# (Runs convertTagDelimiter.py on all the files)

rm output.hiero/hiero.parsertagged output.phrase.based/phrase.parsertagged ref/ref1.parsertagged ref/ref2.parsertagged ref/ref3.parsertagged ref/ref4.parsertagged

cat output.hiero/hiero.parsertagged_orig | ../convertTAGDelimiter.py >> output.hiero/hiero.parsertagged
cat output.phrase.based/phrase.parsertagged_orig | ../convertTAGDelimiter.py >> output.phrase.based/phrase.parsertagged
cat ref/ref1.parsertagged_orig | ../convertTAGDelimiter.py >> ref/ref1.parsertagged
cat ref/ref2.parsertagged_orig | ../convertTAGDelimiter.py >> ref/ref2.parsertagged
cat ref/ref3.parsertagged_orig | ../convertTAGDelimiter.py >> ref/ref3.parsertagged
cat ref/ref4.parsertagged_orig | ../convertTAGDelimiter.py >> ref/ref4.parsertagged

wc -l output.hiero/hiero.parsertagged
wc -l output.phrase.based/phrase.parsertagged
wc -l ref/ref1.parsertagged
wc -l ref/ref2.parsertagged
wc -l ref/ref3.parsertagged
wc -l ref/ref4.parsertagged
wc -l ref/ref1
