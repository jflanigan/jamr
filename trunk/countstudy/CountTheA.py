#!/usr/bin/python3

import re
import sys

mtfile = 'output/hiero'

def findNounPhrase_returnDETandHead(str):
# Searches for all noun phrases in a string POStagged by the stanford tagger and returns a list
# of dictionaries that contain entries for the head and determiner of the noun phrases
    DET = r'((?P<DET>[^ _]+)_DT)?'
    ADJs = r'(\s*[^ _]+_JJ)*'
    Ns = r'(\s*[^ _]+_((NN)|(NNS)|(NNP)|(NNPS)))*'
    HEAD = r'\s*(?P<HEAD>[^ _]+)_((NN)|(NNS)|(NNP)|(NNPS))'
    regexp = DET + ADJs + Ns + HEAD
    return [ match.groupdict() for match in re.finditer(regexp, str) ]

def findHead_returnDET(str, head):
    DET = r'((?P<DET>[^ _]+)_DT)?'
    ADJs = r'(\s*[^ _]+_JJ(S|R)?)*'
    Ns = r'(\s*[^ _]+_((NN)|(NNS)|(NNP)|(NNPS)))*'
    HEAD = r'\s*(' + head + ')_((NN)|(NNS)|(NNP)|(NNPS))'
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
    HEAD = re.compile(r'\s*(' + head + ')_((NNPS)|(NNS)|(NNP)|(NN))', re.I)
    starred = re.sub(HEAD, ' ***'+head+'***_HEAD', str)
    return starred

def starPhrase(str, head):
    phrase = findHead_returnPhrase(str, head)
    return re.sub(phrase, ' ***'+phrase+'*** ', str)

def remove_POS(str):
#    POSstar = '(_.*?)(\\*)'
#    str = re.sub(POSstar, '*', str)
    POS = r'_.*?((\s+)|$)'
    return re.sub(POS, ' ', str)

def logsentence(head, refs, mt, refdet, mtdet, refnum):
    outfile = open(mtfile+'.sentences_'+refdet+'_'+mtdet, mode='at')
#    mtphrase = remove_POS(starHead(mt, head))
    mtphrase = starHead(mt, head)
    outfile.write('MT :\n'+mtphrase+'\n')
    for i in range(len(refs)):
        ref = refs[i]
#        refphrase = remove_POS(starHead(ref, head))
        refphrase = starHead(ref, head)
        if i == refnum:
            outfile.write('-->')
        outfile.write('REF '+str(i)+':\n'+refphrase+'\n')
    outfile.write('\n')
    outfile.close()

def findHead_returnPhrase(str, head):
    DET = r'(?:(?:[^ _]+)_DT)?'
    ADJs = r'(?:\s*[^ _]+_JJ)*'
    Ns = r'(?:\s*[^ _]+_(?:(?:NN)|(?:NNS)|(?:NNP)|(?:NNPS)))*'
    HEAD = r'\s*(?:' + head + ')_(?:(?:NNPS)|(?:NNS)|(?:NNP)|(?:NN))'
    regexp = DET + ADJs + Ns + HEAD
    phrase = re.findall(regexp, str, re.I)
    if len(phrase) != 1:
        print("ERROR: findHead_returnPhrase found more than one phrase.")
    return phrase[0]

def logphrase(head, ref, mt, refdet, mtdet):
    outfile = open(mtfile+'.phrases_'+refdet+'_'+mtdet, mode='at')
    refphrase = remove_POS(findHead_returnPhrase(ref, head))
    mtphrase = remove_POS(findHead_returnPhrase(mt, head))
    outfile.write('MT: '+mtphrase+'\n')
    outfile.write('REF: '+refphrase+'\n\n')
    outfile.close()

def flip(lst):
    return [ [ lst[i][j] for i in range(len(lst)) ] for j in range(len(lst[0])) ]

def histogram(refses, mts, refnum, log = False):
    total = 0
    counts = {'the' : {'the':0, 'a':0, 'NONE':0, 'other':0},
              'a' : {'the':0, 'a':0, 'NONE':0, 'other':0},
              'NONE' : {'the':0, 'a':0, 'NONE':0, 'other':0},
              'other' : {'the':0, 'a':0, 'NONE':0, 'other':0},
              'found' : 0,
              'notfound' : 0,
              'multiple' : 0}
    print('calculating histogram...')
    print('--------------------------------------------------------------------------------')
    progress = 1 # for progress bar
    for linenumber in range(len(mts)):
        if linenumber > progress*(len(mts)/80):
            print('-', end='')
            sys.stdout.flush()
            progress = progress + 1
        refs = refses[linenumber]
        mt = mts[linenumber]
        for (head, refDET, refDETlist, mtDETlist) in strDETcorrespondence(refs[refnum], mt):
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
                break
            if len(mtDETlist) == 1 and len(refDETlist) == 1:
                counts['found'] = counts['found'] + 1
                refdet = detkey(refDET)
                mtdet = detkey(mtDETlist[0])
                counts[detkey(refDET)][detkey(mtDETlist[0])] = counts[detkey(refDET)][detkey(mtDETlist[0])] + 1
                if log:
                    logphrase(head, refs[refnum], mt, refdet, mtdet)
                    logsentence(head, refs, mt, refdet, mtdet, refnum)
            else:
                print("-------------ERROR------------")
    print('-\n')
    return (counts, total)

def testhistogram(refs, mts):
    for (ref, mt) in zip(refs, mts):
        (hist1, total1) = histogram([ref], [mt])
        (hist2, total2) = histogram([mt], [ref])
        if total1 != total2:
            printcorrespondences([ref], [mt])
            printcorrespondences([mt], [ref])

def printhistogram(refses, mts, refnum, log = False):
    (hist, total) = histogram(refses, mts, refnum, log)
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

"""
ref1 = open('ref1', 'r').readlines()
ref2 = open('ref2', 'r').readlines()
ref3 = open('ref3', 'r').readlines()
ref4 = open('ref4', 'r').readlines()
refses = list(zip(ref1, ref2, ref3, ref4))
phrase = open('phrase', 'r').readlines()
hiero = open('hiero', 'r').readlines()
"""

ref1 = open('data/ref/ref1.parsertagged', 'r').readlines()
ref2 = open('data/ref/ref2.parsertagged', 'r').readlines()
ref3 = open('data/ref/ref3.parsertagged', 'r').readlines()
ref4 = open('data/ref/ref4.parsertagged', 'r').readlines()
refses = list(zip(ref2, ref4, ref1))
phrase = open('data/output.phrase.based/phrase.parsertagged', 'r').readlines()
hiero = open('data/output.hiero/hiero.parsertagged', 'r').readlines()


#if running as a script:
if __name__ == "__main__":
#    printhistogram(refses, hiero, 0, log=True)
    printhistogram(refses, ref1, 0, log=False)


def run():
    ref = open(sys.argv[1], 'r').readlines()
    #mt = open(sys.argv[2], 'r').readlines()
    
    print(ref[0])

#run()

