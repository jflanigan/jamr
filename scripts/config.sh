#!/bin/bash

# assumes this script (config.sh) lives in "${JAMR_HOME}/scripts/"
JAMR_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." > /dev/null && pwd )"

CLASSPATH=".:${JAMR_HOME}/target/scala-2.10/jamr-assembly-0.1-SNAPSHOT.jar"

DATA_DIR="${JAMR_HOME}/data/LDC-2013-Sep"

MODEL_DIR="${JAMR_HOME}/experiments/current"

CDEC="${HOME}/code/cdec" # CHANGEME

ILLINOIS_NER="${HOME}/code/IllinoisNerExtended-v2.3" # CHANGEME

STANFORD_PARSER="${HOME}/code/stanford-parser-2011-09-14/stanford-parser.jar" # CHANGEME
