#!/bin/bash

[ $# -ne 1 ] && echo "Usage: ${0##*/} Filename.md" && exit 1

SCRIPT=$(readlink -f $0)
AWK=${SCRIPT%.sh}.awk

AUTHOR="$(sed -n '/\*\*Author\*\*/s/\*\*Author\*\* *//p' $1)"
VSN="$(git describe master --always --tags)"

gawk -f ${AWK} vsn=${VSN} author="$AUTHOR" "$1"
