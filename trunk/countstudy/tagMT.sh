#!/bin/bash
# Tag the output of Hiero and Phrase based MT (on the DEV07_blind test set) with the Stanford parser
cd ~/tools/stanford-postagger-full-2010-05-26/
#./stanford-postagger.sh models/left3words-wsj-0-18.tagger ~/cs/outputFromQin/dev07_blind_text_ref-sorted.plain > ~/cs/outputFromQin/dev07_blind_text_ref-sorted.tagged
#./stanford-postagger.sh models/left3words-wsj-0-18.tagger ~/cs/outputFromQin/output.phrase.based/Output.DEV07_blind > ~/cs/outputFromQin/output.phrase.based/Output.DEV07_blind.tagged
#./stanford-postagger.sh models/left3words-wsj-0-18.tagger ~/cs/outputFromQin/output.hiero/Output.DEV07_blind > ~/cs/outputFromQin/output.hiero/Output.DEV07_blind.tagged

# (first run Tokenize.py)
#rm ~/cs/outputFromQin/ref/*.tagged
#./stanford-postagger-newline.sh models/left3words-wsj-0-18.tagger ~/cs/outputFromQin/ref/ref.tokens > ~/cs/outputFromQin/ref/ref.tagged
./stanford-postagger-newline.sh models/left3words-wsj-0-18.tagger ~/cs/outputFromQin/ref/ref1.tokens > ~/cs/outputFromQin/ref/ref1.tagged
./stanford-postagger-newline.sh models/left3words-wsj-0-18.tagger ~/cs/outputFromQin/ref/ref2.tokens > ~/cs/outputFromQin/ref/ref2.tagged
./stanford-postagger-newline.sh models/left3words-wsj-0-18.tagger ~/cs/outputFromQin/ref/ref3.tokens > ~/cs/outputFromQin/ref/ref3.tagged
./stanford-postagger-newline.sh models/left3words-wsj-0-18.tagger ~/cs/outputFromQin/ref/ref4.tokens > ~/cs/outputFromQin/ref/ref4.tagged
./stanford-postagger-newline.sh models/left3words-wsj-0-18.tagger ~/cs/outputFromQin/output.phrase.based/phrase.tokens > ~/cs/outputFromQin/output.phrase.based/phrase.tagged
./stanford-postagger-newline.sh models/left3words-wsj-0-18.tagger ~/cs/outputFromQin/output.hiero/hiero.tokens > ~/cs/outputFromQin/output.hiero/hiero.tagged



