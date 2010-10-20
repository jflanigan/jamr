#!/usr/bin/python3

import re
import sys

mtfile = 'hiero'

def findNounPhrase_returnDETandHead(str):
# Searches for all noun phrases in a string POStagged by the stanford tagger and returns a list
# of dictionaries that contain entries for the head and determiner of the noun phrases
    DET = '((?P<DET>\w+)_DT)?'
    ADJs = '(\s*\w+_JJ)*'
    Ns = '(\s*\w+_((NN)|(NNS)|(NNP)|(NNPS)))*'
    HEAD = '\s*(?P<HEAD>\w+)_((NN)|(NNS)|(NNP)|(NNPS))'
    regexp = DET + ADJs + Ns + HEAD
    return [ match.groupdict() for match in re.finditer(regexp, str) ]

def findHead_returnDET(str, head):
    DET = '((?P<DET>\w+)_DT)?'
    ADJs = '(\s*\w+_JJ)*'
    Ns = '(\s*\w+_((NN)|(NNS)|(NNP)|(NNPS)))*'
    HEAD = '\s*(' + head + ')_((NN)|(NNS)|(NNP)|(NNPS))'
    regexp = DET + ADJs + Ns + HEAD
    return [ match.groupdict()['DET'] for match in re.finditer(regexp, str, re.I) ]

def findHeads_returnDETS(ref, mt, headdict):
    return [ (entry['HEAD'], entry['DET'], findHead_returnDET(ref, entry['HEAD']), findHead_returnDET(mt, entry['HEAD'])) for entry in headdict ]

def strDETcorrespondence(ref, mt):
    return findHeads_returnDETS(ref, mt, findNounPhrase_returnDETandHead(ref))

def printcorrespondences(refs, mts):
    for (ref, mt) in zip(refs, mts):
        print('ref = ' + ref)
        print('mt =  ' + mt)
        print(strDETcorrespondence(ref, mt),'\n')
    return

def detkey(DET):
    if DET == None:
        det = 'NONE'
    else:
        det = DET.lower()
        if det == 'an':
            det = 'a'
        if det != 'the' and det != 'a':
            det = 'other'
    return det

def starHead(str, head):
    HEAD = re.compile('\s*(' + head + ')_((NN)|(NNS)|(NNP)|(NNPS))', re.I)
    starred = re.sub(HEAD, ' ***'+head+'***_HEAD', str)
    return starred

def remove_POS(str):
    POS = '_.*?((\s+)|$)'
    return re.sub(POS, ' ', str)

def logsentence(head, ref, mt, refdet, mtdet):
    outfile = open(mtfile+'.sentences_'+refdet+'_'+mtdet, mode='at')
    refphrase = remove_POS(starHead(ref, head))
    mtphrase = remove_POS(starHead(mt, head))
    outfile.write('REF:\n'+refphrase+'\n')
    outfile.write('MT:\n'+mtphrase+'\n\n')
    outfile.close()

def findHead_returnPhrase(str, head):
    DET = '(?:(?:\w+)_DT)?'
    ADJs = '(?:\s*\w+_JJ)*'
    Ns = '(?:\s*\w+_(?:(?:NN)|(?:NNS)|(?:NNP)|(?:NNPS)))*'
    HEAD = '\s*(?:' + head + ')_(?:(?:NN)|(?:NNS)|(?:NNP)|(?:NNPS))'
    regexp = DET + ADJs + Ns + HEAD
    phrase = re.findall(regexp, str, re.I)
    if len(phrase) != 1:
        print("ERROR: findHead_returnPhrase found more than one phrase.")
    return phrase[0]

def logphrase(head, ref, mt, refdet, mtdet):
    outfile = open(mtfile+'.phrases_'+refdet+'_'+mtdet, mode='at')
    refphrase = remove_POS(findHead_returnPhrase(ref, head))
    mtphrase = remove_POS(findHead_returnPhrase(mt, head))
    outfile.write('ref = '+refphrase+'\n')
    outfile.write('mt =  '+mtphrase+'\n\n')
    outfile.close()

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
        for (head, refDET, refDETlist, mtDETlist) in strDETcorrespondence(ref, mt):
            goodheadlist = []
            total = total + 1
            if len(mtDETlist) == 0:
                counts['notfound'] = counts['notfound'] + 1
                break;
            if len(mtDETlist) >= 2:
                counts['multiple'] = counts['multiple'] + 1
                break;
            elif len(refDETlist) >= 2:
                counts['multiple'] = counts['multiple'] + 1
                break;
            if len(mtDETlist) == 1 and len(refDETlist) == 1:
                counts['found'] = counts['found'] + 1
                refdet = detkey(refDET)
                mtdet = detkey(mtDETlist[0])
                counts[detkey(refDET)][detkey(mtDETlist[0])] = counts[detkey(refDET)][detkey(mtDETlist[0])] + 1
                logphrase(head, ref, mt, refdet, mtdet)
                logsentence(head, ref, mt, refdet, mtdet)
            else:
                print("-------------ERROR------------")
    return (counts, total)

def testhistogram(refs, mts):
    for (ref, mt) in zip(refs, mts):
        (hist1, total1) = histogram([ref], [mt])
        (hist2, total2) = histogram([mt], [ref])
        if total1 != total2:
            printcorrespondences([ref], [mt])
            printcorrespondences([mt], [ref])

def printhistogram(refs, mts):
    (hist, total) = histogram(refs, mts)
    print('ref - mt\n')
    dets = ['NONE', 'the', 'a', 'other']
    for r in dets:
        branchingtotal = sum([hist[r][key] for key in dets])     # (a la high energy particle physics 'Branching Fraction')
        if branchingtotal == 0:
            branchingtotal = 1
        hist_found = hist['found']
        if hist_found == 0:
            hist_found = 1
        for m in dets:
            print(r,'-',m,' \t',hist[r][m],'\t',100*hist[r][m]/hist_found,'%')
    numcorrect = sum([ hist[r][r] for r in dets ])
    numnoncorret = sum([ hist[r][r] for r in ['the', 'a'] ])
    print()
    for r in dets:
        print(r, '- *   \t', sum([hist[r][key] for key in dets]), '\ttotal')
    for r in dets:
        print('* -', r, '  \t', sum([hist[key][r] for key in dets]), '\ttotal')
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

