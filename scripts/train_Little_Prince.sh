#!/bin/bash
set -ueo pipefail

# Source config script
JAMR_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." > /dev/null && pwd )"
. "${JAMR_HOME}/scripts/config_Little_Prince.sh"

mkdir -p "$JAMR_HOME/data/AMR-Bank-v1.4"
pushd "$JAMR_HOME/data/AMR-Bank-v1.4"
wget http://amr.isi.edu/download/amr-bank-struct-v1.4-training.txt
wget http://amr.isi.edu/download/amr-bank-struct-v1.4-dev.txt
wget http://amr.isi.edu/download/amr-bank-struct-v1.4-test.txt
popd
"${JAMR_HOME}/scripts/TRAIN.sh"

