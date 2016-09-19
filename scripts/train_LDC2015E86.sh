#!/bin/bash
set -ueo pipefail

# Source config script
JAMR_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." > /dev/null && pwd )"
. "${JAMR_HOME}/scripts/config_Semeval-2016_LDC2015E86.sh"

# Extract and preprocess data
if [ ! -f "$JAMR_HOME/data/LDC2015E86_DEFT_Phase_2_AMR_Annotation_R1.tgz" ]; then
    echo 'Error: Please copy LDC2015E86_DEFT_Phase_2_AMR_Annotation_R1.tgz to the directory $JAMR_HOME/data before running this script.'
    exit 1
fi
mkdir -p "$JAMR_HOME/data"
pushd "$JAMR_HOME/data"
tar -xzf LDC2015E86_DEFT_Phase_2_AMR_Annotation_R1.tgz
popd

pushd "$JAMR_HOME/scripts/preprocessing"
LDC2015E86/make_splits.sh
popd
"${JAMR_HOME}/scripts/TRAIN.sh"

