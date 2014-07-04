#!/bin/bash

# assumes this script (config.sh) lives in "${JAMR_HOME}/scripts/"
export JAMR_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." > /dev/null && pwd )"

export CLASSPATH=".:${JAMR_HOME}/target/scala-2.10/jamr-assembly-0.1-SNAPSHOT.jar"

export DATA_DIR="${JAMR_HOME}/data/LDC-2013-Sep"

export MODEL_DIR="${JAMR_HOME}/experiments/current"

export CDEC="${HOME}/code/cdec" # CHANGEME

export ILLINOIS_NER="${HOME}/code/IllinoisNerExtended-v2.3" # CHANGEME
