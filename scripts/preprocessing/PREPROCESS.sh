#!/bin/bash -xe
set -ueo pipefail

# Preprocess the data
#./cmd.txt.FIXED
#./cmd.train
./cmd.snt
./cmd.snt.tok
./cmd.tok

# Run the aligner
./cmd.aligned
# Remove opN
./cmd.aligned.no_opN
# Extract concept table
./cmd.aligned.concepts_no_opN

# Stanford Dependency Parser
./cmd.snt.tok.deps
# Tag with IllinoisNer
./cmd.snt.IllinoisNER

