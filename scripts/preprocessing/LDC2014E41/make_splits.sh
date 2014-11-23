#!/bin/bash
set -ueo pipefail

# split data into train/dev/test folds

DATA_DIR="${JAMR_HOME}/data/LDC2014E41_DEFT_Phase_1_AMR_Annotation_R4/data/split"

split=training
cat "${DATA_DIR}/$split/deft-p1-amr-r4-$split-proxy.txt" > "${DATA_DIR}/$split/$split.txt"
for corpus in bolt cctv dfa mt09sdl wb xinhua; do
    cat "${DATA_DIR}/$split/deft-p1-amr-r4-$split-$corpus.txt" >> "${DATA_DIR}/$split/$split.txt"
done

split=dev
cat "${DATA_DIR}/$split/deft-p1-amr-r4-$split-proxy.txt" > "${DATA_DIR}/$split/$split.txt"
for corpus in bolt consensus dfa xinhua; do
    cat "${DATA_DIR}/$split/deft-p1-amr-r4-$split-$corpus.txt" >> "${DATA_DIR}/$split/$split.txt"
done

split=test
cat "${DATA_DIR}/$split/deft-p1-amr-r4-$split-proxy.txt" > "${DATA_DIR}/$split/$split.txt"
for corpus in bolt consensus dfa xinhua; do
    cat "${DATA_DIR}/$split/deft-p1-amr-r4-$split-$corpus.txt" >> "${DATA_DIR}/$split/$split.txt"
done
