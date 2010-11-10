#!/bin/bash
# creates the symbolic links pointing to the output from the stanford parser
# (assuming it has be converted into a tag format we use)

rm hiero phrase ref ref1 ref2 ref3 ref4
ln -s data/output.hiero/hiero.parsertagged hiero
ln -s data/output.phrase.based/phrase.parsertagged phrase
ln -s data/ref/ref1.parsertagged ref
ln -s data/ref/ref1.parsertagged ref1
ln -s data/ref/ref2.parsertagged ref2
ln -s data/ref/ref3.parsertagged ref3
ln -s data/ref/ref4.parsertagged ref4

