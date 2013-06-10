#!/bin/ksh
#
# given afosid, find awips id
# the input file contains 3 fields:
# 1) afos cccnnnxxx 2) wmo ttaaoo  3) awips cccc
# Last Modified: 01/31/2002
export FILENAME=/awips/fxa/data/afos2awips.txt

if [[ $1 = "" ]]
then
  echo NO_ID_GIVEN
  exit
fi

export AFOSID=$1

LINE=`grep "$AFOSID" $FILENAME`

if [ -n "$LINE" ]
then
  CCCC=`echo $LINE | cut -f3 -d" " `
  NNNXXX=`echo $LINE | cut -c4-9 `
  AWIPSID=$CCCC$NNNXXX
  echo $AWIPSID
else
  echo NO_MATCH
fi


# ----------------------------
