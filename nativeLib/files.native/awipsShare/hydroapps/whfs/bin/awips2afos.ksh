#!/bin/ksh
#
# given awipsid, find afos id
# the input file contains 3 fields:
# 1) afos cccnnnxxx 2) wmo ttaaoo  3) awips cccc
#
# Last Modified: 01/31/2002
export FILENAME=/awips/fxa/data/afos2awips.txt

if [[ $1 = "" ]]
then
  echo NO_ID_GIVEN
  exit
fi

export AWIPSID=$1

CCCC=`echo $AWIPSID | cut -c1-4`
NNNXXX=`echo $AWIPSID | cut -c5-10`
LINE=`grep "$CCCC" $FILENAME | grep "$NNNXXX"`


if [ -n "$LINE" ]
then
  AFOSID=`echo $LINE | cut -f1 -d" " `
  echo $AFOSID
else
  echo NO_MATCH
fi
#
