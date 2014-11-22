#!/bin/bash

# The patch file was produced with the command: diff -u file1 file2 > patch_file

patch "${JAMR_HOME}/data/LDC2014E41_DEFT_Phase_1_AMR_Annotation_R4/data/split/training/deft-p1-amr-r4-training-proxy.txt" "$JAMR_HOME/scripts/preprocessing/LDC2014E41/deft-p1-amr-r4-training-proxy.txt.patch"
