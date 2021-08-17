#!/bin/bash

[ $# -ne 1 ] && echo "Usage: ${0##*/} Filename.md" && exit 1

SCRIPT=$(readlink -f $0)
AWK=${SCRIPT%.sh}.awk

echo "SCRIPT=${SCRIPT}"
echo "AWK=${AWK}"

awk -f ${AWK} vsn=$(git describe --abbrev=1 --tags) "$1"
