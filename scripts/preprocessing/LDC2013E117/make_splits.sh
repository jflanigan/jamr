#!/bin/bash
set -ueo pipefail

# split data into train/dev/test folds

DATA_DIR="${JAMR_HOME}/data/LDC2013E117_DEFT_Phase_1_AMR_Annotation_R3/data"

cat "${DATA_DIR}/deft-amr-release-r3-proxy.txt" | ./nltot | grep '::preferred' | grep -v '_200[78]' | ./ttonl > "${DATA_DIR}/deft-amr-release-r3-proxy.train"
cat "${DATA_DIR}/deft-amr-release-r3-proxy.txt" | ./nltot | grep '::preferred' | grep '_2007' | ./ttonl > "${DATA_DIR}/deft-amr-release-r3-proxy.dev"
cat "${DATA_DIR}/deft-amr-release-r3-proxy.txt" | ./nltot | grep '::preferred' | grep '_2008' | ./ttonl > "${DATA_DIR}/deft-amr-release-r3-proxy.test"
