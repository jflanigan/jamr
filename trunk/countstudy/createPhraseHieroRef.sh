#!/bin/bash
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

cd ..
ln -s output.hiero/hiero .
ln -s output.phrase.based/phrase .
ln -s ref/ref1 ref
ln -s ref/ref1 .
ln -s ref/ref2 .
ln -s ref/ref3 .
ln -s ref/ref4 .

