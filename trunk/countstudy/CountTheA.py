#!/usr/bin/python3

import re
import sys

def findNounPhrase_returnDETandHead(str):
# Searches for all noun phrases in a string POStagged by the stanford tagger and returns a list
# of dictionaries that contain entries for the head and determiner of the noun phrases
    DET = '((?P<DET>\w*)_DT)?'
    ADJs = '(\s*\w*_JJ)*'
    Ns = '(\s*\w*_((NN)|(NNS)|(NNP)|(NNPS)))*'
    HEAD = '\s*(?P<HEAD>\w*)_((NN)|(NNS)|(NNP)|(NNPS))'
    regexp = DET + ADJs + Ns + HEAD
    return [ match.groupdict() for match in re.finditer(regexp, str) ]

def findHead_returnDET(str, head):
    DET = '((?P<DET>\w*)_DT)?'
    ADJs = '(\s*\w*_JJ)*'
    Ns = '(\s*\w*_((NN)|(NNS)|(NNP)|(NNPS)))*'
    HEAD = '\s*(' + head + ')_((NN)|(NNS)|(NNP)|(NNPS))'
    regexp = DET + ADJs + Ns + HEAD
    return [ match.groupdict()['DET'] for match in re.finditer(regexp, str, re.I) ]

def findHeads_returnDETS(str, headdict):
    return [ (entry['HEAD'], entry['DET'], findHead_returnDET(str, entry['HEAD'])) for entry in headdict ]

def strDETcorrespondence(ref, mt):
    return findHeads_returnDETS(mt, findNounPhrase_returnDETandHead(ref))

def printcorrespondences(refs, mts):
    for (ref, mt) in zip(refs, mts):
        print('ref = ' + ref)
        print('mt =  ' + mt)
        print(strDETcorrespondence(ref, mt))
    return

def histogram(refs, mts):
    total = 0
    counts = {'the' : {'the':0, 'a':0, 'NONE':0, 'other':0},
              'a' : {'the':0, 'a':0, 'NONE':0, 'other':0},
              'NONE' : {'the':0, 'a':0, 'NONE':0, 'other':0},
              'other' : {'the':0, 'a':0, 'NONE':0, 'other':0},
              'found' : 0,
              'notfound' : 0,
              'multiple' : 0}
    for (ref, mt) in zip(refs, mts):
        for (head, refDET, mtDETlist) in strDETcorrespondence(ref, mt):
            total = total + 1
            if len(mtDETlist) == 0:
                counts['notfound'] = counts['notfound'] + 1
            if len(mtDETlist) >= 2:
                counts['multiple'] = counts['multiple'] + 1
            if len(mtDETlist) == 1:
                counts['found'] = counts['found'] + 1
                if refDET == None:
                    refDET = 'NONE'
                if mtDETlist[0] == None:
                    mtDETlist[0] = 'NONE'
                refdet = refDET.lower()
                mtdet = mtDETlist[0].lower()
                if refDET == 'NONE':
                    refdet = 'NONE'
                if mtDETlist[0] == 'NONE':
                    mtdet = 'NONE'

                if refdet == 'an':
                    refdet = 'a'
                if mtdet == 'an':
                    mtdet = 'a'
                if refdet == 'NONE' or refdet == 'the' or refdet == 'a':
                    if mtdet == 'NONE' or mtdet == 'the' or mtdet == 'a':
                        counts[refdet][mtdet] = counts[refdet][mtdet] + 1
                    else:
                        counts[refdet]['other'] = counts[refdet]['other'] + 1
                else:
                    print(refdet)
                    if mtdet == 'NONE' or mtdet == 'the' or mtdet == 'a':
                        counts['other'][mtdet] = counts['other'][mtdet] + 1
                    else:
                        counts['other']['other'] = counts['other']['other'] + 1
    return (counts, total)

def printhistogram(refs, mts):
    (hist, total) = histogram(refs, mts)
    print('ref -> mt')
    dets = ['NONE', 'the', 'a', 'other']
    for r in dets:
        branchingtotal = sum([hist[r][key] for key in dets])     # (a la high energy particle physics 'Branching Fraction')
        if branchingtotal == 0:
            branchingtotal = 1
        for m in dets:
            print(r,'->',m,' \t',hist[r][m],'\t',100*hist[r][m]/hist['found'],'%')
    numcorrect = sum([ hist[r][r] for r in dets ])
    numnoncorret = sum([ hist[r][r] for r in ['the', 'a'] ])
    print()
    print(100*numcorrect/hist['found'],'% correct out of',hist['found'],'found in MT output\n')
#    print(100*numnoncorrect/hist['found'],'% correct out of',hist['found'],'found in MT output\n')
    for s in ['found', 'notfound', 'multiple']:
        print(hist[s],'\t(',100*hist[s]/total,'%)\t',s)
    print(total, 'total noun phrases in ref')

ref = open('ref', 'r').readlines()
phrase = open('phrase', 'r').readlines()
hiero = open('hiero', 'r').readlines()

def run():
    ref = open(sys.argv[1], 'r').readlines()
    #mt = open(sys.argv[2], 'r').readlines()
    
    print(ref[0])

#run()

