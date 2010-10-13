#!/usr/bin/python3

import re

def findNounPhraseDETandHead(str):
    DET = '((?P<DET>\w*)_DT)?'
    ADJs = '\s*(\w*_JJ)*'
    Ns = '\s*(\w*_((NN)|(NNS)|(NNP)|(NNPS)))*'
    HEAD = '\s*(?P<HEAD>\w*)_((NN)|(NNS)|(NNP)|(NNPS))'
    regexp = DET + ADJs + Ns + HEAD
    for match in re.finditer(regexp, str)
        match.groupdict()























