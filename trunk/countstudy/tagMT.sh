#!/bin/bash
# Tag the output of Hiero and Phrase based MT (on the DEV07_blind test set) with the Stanford parser
cd ~/tools/stanford-postagger-full-2010-05-26/
./stanford-postagger.sh models/left3words-wsj-0-18.tagger ~/cs/outputFromQin/dev07_blind_text_ref-sorted.plain > ~/cs/outputFromQin/dev07_blind_text_ref-sorted.tagged
./stanford-postagger.sh models/left3words-wsj-0-18.tagger ~/cs/outputFromQin/output.phrase.based/Output.DEV07_blind > ~/cs/outputFromQin/output.phrase.based/Output.DEV07_blind.tagged
./stanford-postagger.sh models/left3words-wsj-0-18.tagger ~/cs/outputFromQin/output.hiero/Output.DEV07_blind > ~/cs/outputFromQin/output.hiero/Output.DEV07_blind.tagged
