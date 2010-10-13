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

filtered = []
for (en,ch,line) in zip(enlist, chlist, range(1, len(enlist))):
    if ((ch.find(u'有') != -1 and (' '+en).find(' a ') != -1 ) # 1448 (182 less than 40 in ch)
      or (ch.find(u'把') != -1) # 538 (116 less than 40 in ch)
      or (ch.find(u'有') == 0) # and len(ch) < 40: # 174
      or (en.find('a ') == 0) # and len(ch) < 40: # 248
      or (ch.find(u'一') != -1 and (' '+en).find(' a ') != -1)
      and en.find(' er ') == -1 and en.find(' uh ') == -1 and en.find(' um ') == -1):  # 2917 (171 less than 30 in ch)
#    if len(en) < 80:
        filtered.append((en,ch,line))
#        print ch
#        print en

sys.stdout = codecs.open('study2nolen.txt', 'w', encoding='utf-8')

for i in range(0,30):
    x = random.choice(range(0,len(filtered)))
    (en,ch,line) = filtered[x]
    print i, ': Line ', line
    print ch
    out.write(ch+u'\n')
    print ''  #pinyin
    print ''
    print ''  #google translate
    print ''
    print en
    print '\n'

print len(enlist), ' sentences total in the corpus.'
print 'Selection process (for definiteness effects and length), filtered this down to ', len(filtered), ' sentences.'
print 'The above sentences were randomly selected from the ', len(filtered), 'filtered sentences.'


sys.stdout.close()

