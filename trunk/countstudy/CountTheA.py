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
    return [ match.groupdict()['DET'] for match in re.finditer(regexp, str) ]

def findHeads_returnDETS(str, headdict):
    return [ findHead_returnDET(str, entry['HEAD']) for entry in headdict ]

def run():
    ref = open(sys.argv[1], 'r').readlines()
    #mt = open(sys.argv[2], 'r').readlines()
    
    print(ref[0])

#run()

