#!/bin/bash
set -ueo pipefail

if [ -z "$JAMR_HOME" ]; then
    echo 'Error: please source config script'
    exit 1
fi

pushd "$JAMR_HOME/scripts/preprocessing" > /dev/null
set -x

# Preprocess the data
./cmd.snt
./cmd.snt.tok
./cmd.tok

# Run the aligner
./cmd.aligned
# Remove opN
./cmd.aligned.no_opN

# Stanford Dependency Parser
./cmd.snt.tok.deps
# Tag with IllinoisNer
./cmd.snt.IllinoisNER

