#!/bin/sh
#############################################################################
#
# script: ASOS_smDecoder
#
# Purpose:  executes the program which decodes the ASOS Daily or Monthly
#           Summary Message for daily climate data
#
# Author: David T. Miller   PRC/TDL
#
# Date created: 6/16/99
#
# Modification History:
#
# 12/05/2002  OB2  Bob Morris        - Incorporated CLIMATE_BIN_DIR from envir.
#                                    - Fixed -a tests, use -f and -x
#
#############################################################################

# operations - set environmental files for Informix
export FXA_HOME=/awips/fxa
. $FXA_HOME/readenv.sh


if [ -f /tmp/ASOS_SM_tmp.txt ]
then
   rm -f /tmp/ASOS_SM_tmp.txt
fi


if [ ! -x ${CLIMATE_BIN_DIR}/ASOS_smDecoder ]
then
   echo "The ASOS DSM or MSM decoder not executable or does not exist!" >> /tmp/ASOS_SM_tmp.txt
   echo "Halting script execution...."
   exit 1
else
   echo "ASOS Summary Message Decoder starting...." >> /tmp/ASOS_SM_tmp.txt
   echo $1 >> /tmp/ASOS_SM_tmp.txt
   ${CLIMATE_BIN_DIR}/ASOS_smDecoder $1 >> /tmp/ASOS_SM_tmp.txt
   decode_status=$?
   case $decode_status in
       0)
             echo "The ASOS Summary Message Decoder returned a value of 0" >> /tmp/ASOS_SM_tmp.txt
	     ;;
       *)  
             echo "The ASOS Summary Message Decoder returned a value of $decode_status" >> /tmp/ASOS_SM_tmp.txt
	     exit $decode_status
	     ;;
    esac	     
fi

exit 0
