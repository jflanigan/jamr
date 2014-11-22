#!/bin/bash
set -ueo pipefail

# split data into train/dev/test folds

DATA_DIR="${JAMR_HOME}/data/LDC2013E117_DEFT_Phase_1_AMR_Annotation_R3/data"

cat "${DATA_DIR}/deft-amr-release-r3-proxy.txt" | "${JAMR_HOME}/scripts/preprocessing/nltot" | grep '::preferred' | grep -v '_200[78]' | "${JAMR_HOME}/scripts/preprocessing/ttonl" > "${DATA_DIR}/deft-amr-release-r3-proxy.train"
cat "${DATA_DIR}/deft-amr-release-r3-proxy.txt" | "${JAMR_HOME}/scripts/preprocessing/nltot" | grep '::preferred' | grep '_2007' | "${JAMR_HOME}/scripts/preprocessing/ttonl" > "${DATA_DIR}/deft-amr-release-r3-proxy.dev"
cat "${DATA_DIR}/deft-amr-release-r3-proxy.txt" | "${JAMR_HOME}/scripts/preprocessing/nltot" | grep '::preferred' | grep '_2008' | "${JAMR_HOME}/scripts/preprocessing/ttonl" > "${DATA_DIR}/deft-amr-release-r3-proxy.test"
