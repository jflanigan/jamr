#!/bin/bash
INPUT=$1
head -n 623 $INPUT > Output.DEV07_dev
head -n 1211 $INPUT | tail -n 588 > Output.DEV07_blind
head -n 1902 $INPUT | tail -n 691 > Output.MT08-nw
head -n 2568 $INPUT | tail -n 666 > Output.MT08-wb
head -n 2568 $INPUT | tail -n 1357 > Output.MT08
head -n 3446 $INPUT | tail -n 878 > Output.MT02
head -n 4365 $INPUT | tail -n 919 > Output.MT03
head -n 6153 $INPUT | tail -n 1788 > Output.MT04
tail -n 1082 $INPUT > Output.MT05
