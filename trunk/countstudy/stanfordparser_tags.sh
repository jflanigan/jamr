#!/bin/bash
# Tag phrase based, hiero, and ref with the Stanford parser
# This script should be run from the data directory

rm */*.parsertagged_orig

../lexparser_TAGS.csh $PWD/output.hiero/hiero.tokens > output.hiero/hiero.parsertagged_orig &

../lexparser_TAGS.csh $PWD/output.phrase.based/phrase.tokens > output.phrase.based/phrase.parsertagged_orig &

../lexparser_TAGS.csh $PWD/ref/ref1.tokens > ref/ref1.parsertagged_orig &
../lexparser_TAGS.csh $PWD/ref/ref2.tokens > ref/ref2.parsertagged_orig &
../lexparser_TAGS.csh $PWD/ref/ref3.tokens > ref/ref3.parsertagged_orig &
../lexparser_TAGS.csh $PWD/ref/ref4.tokens > ref/ref4.parsertagged_orig &

