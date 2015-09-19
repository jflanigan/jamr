#!/bin/bash
set -ueo pipefail

# split data into train/dev/test folds

DATA_DIR="${JAMR_HOME}/data/LDC2015E86_DEFT_Phase_2_AMR_Annotation_R1/data/amrs/split"

split=training
cat "${DATA_DIR}/$split/deft-p2-amr-r1-amrs-$split-proxy.txt" > "${DATA_DIR}/$split/$split.txt"
for corpus in bolt dfa mt09sdl xinhua wb cctv guidelines; do
    cat "${DATA_DIR}/$split/deft-p2-amr-r1-amrs-$split-$corpus.txt" >> "${DATA_DIR}/$split/$split.txt"
done

split=dev
cat "${DATA_DIR}/$split/deft-p2-amr-r1-amrs-$split-proxy.txt" > "${DATA_DIR}/$split/$split.txt"
for corpus in bolt consensus dfa xinhua; do
    cat "${DATA_DIR}/$split/deft-p2-amr-r1-amrs-$split-$corpus.txt" >> "${DATA_DIR}/$split/$split.txt"
done

split=test
cat "${DATA_DIR}/$split/deft-p2-amr-r1-amrs-$split-proxy.txt" > "${DATA_DIR}/$split/$split.txt"
for corpus in bolt consensus dfa xinhua; do
    cat "${DATA_DIR}/$split/deft-p2-amr-r1-amrs-$split-$corpus.txt" >> "${DATA_DIR}/$split/$split.txt"
done
