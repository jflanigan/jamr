#!/bin/bash

# assumes this script (config.sh) lives in "${JAMR_HOME}/scripts/"

# change the following enviroment variables for your configuration

export CDEC="${HOME}/tools/cdec" # CHANGEME

export ILLINOIS_NER="${HOME}/tools/IllinoisNerExtended" # CHANGEME

export ILLINOIS_NER_JAR="${ILLINOIS_NER}/dist/LbjNerTagger-2.3.jar" # CHANGEME

# the variables below shouldn't need to be changed

export JAMR_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." > /dev/null && pwd )"

export CLASSPATH=".:${JAMR_HOME}/target/scala-2.10/jamr-assembly-0.1-SNAPSHOT.jar"

export DATA_DIR="${JAMR_HOME}/data/LDC-2013-Sep"

export MODEL_DIR="${JAMR_HOME}/experiments/current"


