#!/bin/bash
#
# Multiple refs

cd ~/cs/outputFromQin/
mkdir ref
cd ref
#cp -v dev07_blind_text_ref-sorted.plain .
#mv dev07_blind_text_ref-sorted.plain ref.DEV07_blind
src="/afs/cs.cmu.edu/user/nbach/Eval/Chinese"
#ls $src
cp -v $src/may03Data/ref.1 ref1.MT03
cp -v $src/may03Data/ref.2 ref2.MT03
cp -v $src/may03Data/ref.3 ref3.MT03
cp -v $src/may03Data/ref.4 ref4.MT03
cp -v $src/mt08/mt08_c2e_nw.ref1 ref1.MT08
cp -v $src/mt08/mt08_c2e_nw.ref2 ref2.MT08
cp -v $src/mt08/mt08_c2e_nw.ref3 ref3.MT08
cp -v $src/mt08/mt08_c2e_nw.ref4 ref4.MT08
#cp -v $src/mt08/mt08_c2e_web.ref1 ref.MT08.part2
#cat ref.MT08.part1 ref.MT08.part2 >> ref.MT08
#rm ref.MT08.part1 ref.MT08.part2
cat ref1.MT03 ref1.MT08 >> ref1
cat ref2.MT03 ref1.MT08 >> ref2
cat ref3.MT03 ref1.MT08 >> ref3
cat ref4.MT03 ref1.MT08 >> ref4

