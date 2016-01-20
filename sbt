#!/bin/bash

exec java -Dsbt.log.noformat=true -Xmx32g -XX:MaxPermSize=1g -jar $(dirname $0)/sbt-launch.jar "$@"
