#!/usr/bin/python3

import sys
import re

def convertline(str):
    return re.sub('(\S*)(/)(\S*)', '\\1_\\3', str)

for line in sys.stdin.readlines():
    if line != '\n' and line != "Sentence skipped: no PCFG fallback.\n":
        print(convertline(line.rstrip('\n')))

