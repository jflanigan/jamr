#!/bin/bash

# The patch file was produced with the command: diff -u file1 file2 > patch_file

patch "${JAMR_HOME}/data/LDC2013E117_DEFT_Phase_1_AMR_Annotation_R3/data/deft-amr-release-r3-proxy.txt" "$JAMR_HOME/scripts/preprocessing/LDC2013E117/deft-amr-release-r3-proxy.txt.patch"
