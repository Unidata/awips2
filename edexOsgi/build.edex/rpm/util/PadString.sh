#!/bin/bash

# Expected Arguments:
#   ${1} == The String To Pad (Or Trim), If Necessary.
#   ${2} == The Desired String Length

STRING_OF_INTEREST=${1}
DESIRED_LENGTH=${2}

CURRENT_LENGTH=`echo ${#STRING_OF_INTEREST}`

if [ ${CURRENT_LENGTH} -le ${DESIRED_LENGTH} ];
then
   COUNTER=${CURRENT_LENGTH}
   while [ ${COUNTER} -le ${DESIRED_LENGTH} ]
   do
      COUNTER=$(( ${COUNTER} + 1 ))
      STRING_OF_INTEREST="${STRING_OF_INTEREST} "
   done
else
   # Due To 0-Indexing
   SUBSTRING_LENGTH=$(( ${DESIRED_LENGTH} + 1))
   STRING_OF_INTEREST=`echo ${STRING_OF_INTEREST:0:${SUBSTRING_LENGTH}}`
fi

echo "${STRING_OF_INTEREST}"
