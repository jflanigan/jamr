#!/bin/bash

# Preprocess the data
./cmd.amr-release-proxy.train
./cmd.amr-release-proxy.train.snt
./cmd.amr-release-proxy.train.snt.tok
./cmd.amr-release-proxy.train.tok

# Run the aligner
./cmd.amr-release-proxy.train.aligned
# Remove opN
./cmd.amr-release-proxy.train.aligned.no_opN

# Tag with IllinoisNer
./cmd.amr-release-proxy.train.snt.IllinoisNER

