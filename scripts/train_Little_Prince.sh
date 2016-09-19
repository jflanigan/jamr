#!/bin/bash
set -ueo pipefail

# Source config script
JAMR_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." > /dev/null && pwd )"
. "${JAMR_HOME}/scripts/config_Semeval-2016_Little_Prince.sh"

mkdir -p "$JAMR_HOME/data/AMR-Bank-v1.4"
pushd "$JAMR_HOME/data/AMR-Bank-v1.4"
for split in training dev test; do
    file=amr-bank-struct-v1.4-$split.txt
    if [ ! -f $file ]; then
        wget http://amr.isi.edu/download/$file
    fi
done
popd
"${JAMR_HOME}/scripts/TRAIN.sh"

