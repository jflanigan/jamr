#!/bin/bash
#
cd ~/cs/outputFromQin/
mkdir ref
cd ref
cp -v dev07_blind_text_ref-sorted.plain .
mv dev07_blind_text_ref-sorted.plain ref.DEV07_blind
src="/afs/cs.cmu.edu/user/nbach/Eval/Chinese"
ls $src
cp -v $src/may03Data/ref.1 ref.MT03
cp -v $src/mt08/mt08_c2e_nw.ref1 ref.MT08.part1
cp -v $src/mt08/mt08_c2e_web.ref1 ref.MT08.part2
cat ref.MT08.part1 ref.MT08.part2 >> ref.MT08
rm ref.MT08.part1 ref.MT08.part2
