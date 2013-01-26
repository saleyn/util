#!/bin/bash

[ $# -ne 1 ] && echo "Usage: ${0##*/} Filename.md" && exit 1

SCRIPT=$(readlink -f $0)
AWK=${SCRIPT%.sh}.awk

gawk -f ${AWK} vsn=$(git describe master --always --tags) "$1"
