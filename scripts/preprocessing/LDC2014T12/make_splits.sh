#!/bin/bash
set -ueo pipefail

# split data into train/dev/test folds

DATA_DIR="${JAMR_HOME}/data/amr_anno_1.0/data/split"

split=training
cat "${DATA_DIR}/$split/amr-release-1.0-$split-proxy.txt" > "${DATA_DIR}/$split/$split.txt"
for corpus in bolt dfa mt09sdl xinhua; do
    cat "${DATA_DIR}/$split/amr-release-1.0-$split-$corpus.txt" >> "${DATA_DIR}/$split/$split.txt"
done

split=dev
cat "${DATA_DIR}/$split/amr-release-1.0-$split-proxy.txt" > "${DATA_DIR}/$split/$split.txt"
for corpus in bolt consensus dfa xinhua; do
    cat "${DATA_DIR}/$split/amr-release-1.0-$split-$corpus.txt" >> "${DATA_DIR}/$split/$split.txt"
done

split=test
cat "${DATA_DIR}/$split/amr-release-1.0-$split-proxy.txt" > "${DATA_DIR}/$split/$split.txt"
for corpus in bolt consensus dfa xinhua; do
    cat "${DATA_DIR}/$split/amr-release-1.0-$split-$corpus.txt" >> "${DATA_DIR}/$split/$split.txt"
done
