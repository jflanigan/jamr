#!/bin/bash
# Tag phrase based, hiero, and ref with the Stanford tagger
# This script should be run from the data directory

# (first run Tokenize.py)

../stanford-postagger-newline.sh models/left3words-wsj-0-18.tagger output.hiero/hiero.tokens > output.hiero/hiero.tagged

../stanford-postagger-newline.sh models/left3words-wsj-0-18.tagger output.phrase.based/phrase.tokens > output.phrase.based/phrase.tagged

../stanford-postagger-newline.sh models/left3words-wsj-0-18.tagger ref/ref1.tokens > ref/ref1.tagged
../stanford-postagger-newline.sh models/left3words-wsj-0-18.tagger ref/ref2.tokens > ref/ref2.tagged
../stanford-postagger-newline.sh models/left3words-wsj-0-18.tagger ref/ref3.tokens > ref/ref3.tagged
../stanford-postagger-newline.sh models/left3words-wsj-0-18.tagger ref/ref4.tokens > ref/ref4.tagged


