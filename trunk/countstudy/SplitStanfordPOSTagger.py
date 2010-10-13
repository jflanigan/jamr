#!/usr/bin/python3

import sys

input = sys.stdin
lines = input.readlines()
for words_POS in [line.split(' ') for line in lines[0:1]] :
    print(words_POS)
    wordlist = [word_POS.split('_',1)[0] for word_POS in words_POS]  # this will not work as expect if input contains _
    POSlist = [word_POS.split('_',1)[1] for word_POS in words_POS]
    print(wordlist)
    print(POSlist)
    wordstr = ' '.join(wordlist)
    POSstr = ' '.join(POSlist)
    print(wordstr)
    print(POSstr)
#for line in POSslist:
#    print(line)

