#!/bin/bash

exec java -Dsbt.log.noformat=true -Xmx4g -XX:MaxPermSize=256m -jar $(dirname $0)/sbt-launch.jar "$@"
