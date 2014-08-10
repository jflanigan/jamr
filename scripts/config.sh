#!/bin/bash

# assumes this script (config.sh) lives in "${JAMR_HOME}/scripts/"

# change the following enviroment variables for your configuration

export CDEC="${HOME}/tools/cdec" # CHANGEME

export ILLINOIS_NER="${HOME}/tools/IllinoisNerExtended" # CHANGEME

export ILLINOIS_NER_JAR="${ILLINOIS_NER}/target/IllinoisNerExtended-2.7.jar" # CHANGEME

export WNHOME="${HOME}/tools/WordNet-3.0" # WORDNET

export JAMR_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." > /dev/null && pwd )"

export DATA_DIR="${JAMR_HOME}/data/LDC-2013-Sep"

export CLASSPATH=".:${JAMR_HOME}/target/scala-2.10/jamr-assembly-0.1-SNAPSHOT.jar"

export MODEL_DIR="${JAMR_HOME}/experiments/current"

