#!/bin/bash
# Merges the correct files into phrase, hiero and ref
cd data

cd output.hiero
cat Output.MT03 Output.MT08-nw > hiero
cd ..

cd output.phrase.based
cat Output.MT03 Output.MT08-nw > phrase
cd ..

cd ref
cat ref1.MT03 ref1.MT08 >> ref1
cat ref2.MT03 ref1.MT08 >> ref2
cat ref3.MT03 ref1.MT08 >> ref3
cat ref4.MT03 ref1.MT08 >> ref4

