#!/bin/bash

exec java -Dsbt.log.noformat=true ${JAVA_OPTS} -jar $(dirname $0)/sbt-launch.jar "$@"
