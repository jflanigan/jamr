#!/bin/bash

# assumes this script (config.sh) lives in "${JAMR_HOME}/scripts/"

export JAMR_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." > /dev/null && pwd )"
export CLASSPATH=".:${JAMR_HOME}/target/scala-2.10/jamr-assembly-0.1-SNAPSHOT.jar"

# change the following enviroment variables for your configuration

export CDEC="${JAMR_HOME}/tools/cdec"
export ILLINOIS_NER="${JAMR_HOME}/tools/IllinoisNerExtended"
export ILLINOIS_NER_JAR="${ILLINOIS_NER}/target/IllinoisNerExtended-2.7.jar"
export WNHOME="${JAMR_HOME}/tools/WordNet-3.0"
export SCALA="${JAMR_HOME}/tools/scala-2.11.2/bin/scala"
export SMATCH="${JAMR_HOME}/scripts/smatch_v1_0/smatch_modified.py"

export TRAIN_FILE="${JAMR_HOME}/data/amr_anno_1.0/data/split/training/amr-release-1.0-training-proxy.txt"
export DEV_FILE="${JAMR_HOME}/data/amr_anno_1.0/data/split/dev/amr-release-1.0-dev-proxy.txt"
export TEST_FILE="${JAMR_HOME}/data/amr_anno_1.0/data/split/test/amr-release-1.0-test-proxy.txt"

export MODEL_DIR="${JAMR_HOME}/models/ACL2014_LDC2014T12-proxy"  # ideally keep this the same as the config_SOMETHING.sh

# The options specified below will override any options specified in the scripts
# CONCEPT_ID_TRAINING_OPTIONS and RELATION_ID_TRAINING_OPTIONS will override PARSER_OPTIONS

export STAGE1_FEATURES="bias,length,fromNERTagger,conceptGivenPhrase"

export PARSER_OPTIONS="
    --stage1-features ${STAGE1_FEATURES}
    --stage2-decoder LR
    --stage2-features rootConcept,rootDependencyPathv1,bias,typeBias,self,fragHead,edgeCount,distance,logDistance,posPathv3,dependencyPathv4,conceptBigram
    --stage2-labelset ${JAMR_HOME}/resources/labelset-r4
    --output-format AMR,nodes,edges,root
    --ignore-parser-errors
    --print-stack-trace-on-errors
"
export CONCEPT_EXTRACT_OPTIONS="
    --stage1-features ${STAGE1_FEATURES}
"

export CONCEPT_ID_TRAINING_OPTIONS="
    --training-optimizer Adagrad
    --training-passes 10
    --training-stepsize 1
    -v 1
"

export RELATION_ID_TRAINING_OPTIONS="
    --training-optimizer Adagrad
    --training-passes 7
    --training-save-interval 1
"
