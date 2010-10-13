# -*- coding: utf-8 -*-

import random
import sys
# The following should work for python3.0
#enlist = open('en', 'r').readlines()
#chlist = open('ch', 'r').readlines()
#outlist = open('ch_output', 'w')

# The following should work for python 2.6
import codecs
enlist = codecs.open('en', 'r', encoding='utf-8').readlines()
chlist = codecs.open('ch', 'r', encoding='utf-8').readlines()
out = codecs.open('ch_output', 'w', encoding='utf-8')

if len(enlist) != len(chlist):
    print 'Error len(en) = ', len(enlist), ', len(ch) = ', len(chlist)

sys.stdout = codecs.open('officerstory', 'w', encoding='utf-8')

for line in range(12509, 12532):
    print 'Line ', line
    print chlist[line-1]
    out.write(chlist[line-1]+u'\n')
    print ''  #google translate
    print ''
    print enlist[line-1]
    print '\n'

