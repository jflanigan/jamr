#!/usr/bin/python

import nltk
import os

os.chdir('/afs/cs.cmu.edu/user/jmflanig/outputFromQin')

for filename in ['ref/ref', 'output.phrase.based/phrase', 'output.hiero/hiero'] :
    infile = open(filename, 'r').readlines()
    tokens = map(nltk.word_tokenize, infile)
    str = map(' '.join, tokens)
    file = open(filename+'.tokens', 'w')
    for line in str:
        print >> file , line
    file.close()

