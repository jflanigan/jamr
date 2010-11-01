#!/bin/bash
#
# Multiple refs

cd data
mkdir ref
cd ref
#cp -v dev07_blind_text_ref-sorted.plain .
#mv dev07_blind_text_ref-sorted.plain ref.DEV07_blind
src="/afs/cs.cmu.edu/user/nbach/Eval/Chinese"
#The data has also been copied to ~/archive/afs_cs.cmu.edu_user_nbach_Eval_Chinese on tahoe.lti

# Copy the ref files
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

