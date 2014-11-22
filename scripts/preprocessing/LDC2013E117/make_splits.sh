#!/bin/bash
set -ueo pipefail

# split data into train/dev/test folds

cat "${DATA_DIR}/deft-amr-release-r3-proxy.txt" | ./nltot | grep '::preferred' | grep -v '_200[78]' | ./ttonl > "${DATA_DIR}/amr-release-proxy.train"
cat "${DATA_DIR}/deft-amr-release-r3-proxy.txt" | ./nltot | grep '::preferred' | grep '_2007' | ./ttonl > "${DATA_DIR}/amr-release-proxy.dev"
cat "${DATA_DIR}/deft-amr-release-r3-proxy.txt" | ./nltot | grep '::preferred' | grep '_2008' | ./ttonl > "${DATA_DIR}/amr-release-proxy.test"
