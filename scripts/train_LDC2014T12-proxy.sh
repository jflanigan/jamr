#!/bin/bash
set -ueo pipefail

# Source config script
JAMR_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." > /dev/null && pwd )"
. "${JAMR_HOME}/scripts/config_Semeval-2016_LDC2014T12-proxy.sh"

# Extract and preprocess data
if [ ! -f "$JAMR_HOME/data/amr_anno_1.0_LDC2014T12.tgz" ]; then
    echo 'Error: Please copy amr_anno_1.0_LDC2014T12.tgz to the directory $JAMR_HOME/data before running this script.'
    exit 1
fi
mkdir -p "$JAMR_HOME/data"
pushd "$JAMR_HOME/data"
tar -xzf amr_anno_1.0_LDC2014T12.tgz
popd

"${JAMR_HOME}/scripts/TRAIN.sh"

