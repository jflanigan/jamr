#!/bin/bash
#

# Originally the output from Qin was located in /afs/cs.cmu.edu/user/qing/ForJeff
# I copied it to my local computer (tahoe.lti.cs.cmu) in case he deleted it
cp -rv ~/archive/afs_cs.cmu.edu_user_qing_ForJeff/* data

cd data

mkdir output.phrase.based
cd output.phrase.based/
ln -s ../Output.All_Test.Phrase-Based.txt .
../../Split_TestSet.sh Output.All_Test.Phrase-Based.txt

cd ..
mkdir output.hiero
cd output.hiero
ln -s ../Output.All_Test.Hiero.txt .
../../Split_TestSet.sh Output.All_Test.Hiero.txt

