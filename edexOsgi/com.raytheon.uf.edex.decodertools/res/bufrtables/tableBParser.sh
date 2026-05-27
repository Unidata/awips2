#!/bin/bash
##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
#
#
# SOFTWARE HISTORY
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Aug 11, 2023 2034216    sharbison   Initial Creation of Table B parser
#
##

# tableBParser.sh - used to parse table B and convert to awips usable format.
# Usage:
# 1) Determine the version of table B to be parsed:
#    ex/ TableB_STD_0_35.txt, change input file name at bottom of script if different.
# 2) Copy TableB_STD_0_##.txt file containing unprocessed Table_B data to tableBParser.sh directory.
# 3) Run the tableBParser script: 
#    i.e. 'bash tableBParser.sh > BUFR_Table_B.txt'.
# 4) Some manual cleanup of auto-generated BUFR_Table_B.txt may be required.

# Reference: Table B Header (TableB_STD_0_35)
#========================================================================================================
# F-XX-YYY |SCALE| REFERENCE   | BIT |      UNIT         | MNEMONIC ;DESC ;  ELEMENT NAME
#          |     |   VALUE     |WIDTH|                   |          ;CODE ;
#========================================================================================================
# ex/ 
# Unparsed Data:
#  0-00-001 |   0 |           0 |  24 | CCITT IA5         | TABLAE   ;     ; Table A: entry
# Desired Format: (tab = /t)
#(BEGIN)0(tab)0(tab)1(tab)0(tab)0(tab)24(tab)CCITT IA5 Table A: entry(END)

while read line
do
    IFS='|' read -r -a array <<< "$line"
    if [ ${#array[@]} -eq 6 ]; then
        if [[ "${array[0]}" != \#* ]]; then
            # F-XX-YYY: 
            # Create array with '-' delimiter.
            p1_array=($(echo $array[0] | tr '-' "\n"))
            
            # Strip leading zeros '0' ex/ [0,00,001] -> [0,0,1]
            p1_a0="$(echo $((10#${p1_array[0]})))"
            p1_a1="$(echo $((10#${p1_array[1]})))"
            p1_a2="$(echo $((10#${p1_array[2]})))"
            
            # Create p1 (F-XX-YYY) with tab delimiter
            p1="${p1_a0}\t${p1_a1}\t${p1_a2}"
            
            # Scale
            # Trim leading/trailing whitespace.
            p2="$(echo -e "${array[1]}" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
            
            # Reference Value
            # Trim leading/trailing whitespace.
            p3="$(echo -e "${array[2]}" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
            
            # Bit Width
            # Trim leading/trailing whitespace.
            p4="$(echo -e "${array[3]}" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
            
            # Unit
            # Trim leading/trailing whitespace.
            p5="$(echo -e "${array[4]}" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
            
            # Mnemonic
            # Trim everything to the left of the 2nd ';' semicolon.
            # Trim leading/trailing whitespace.
            p6="$(echo -e "${array[5]}" | sed 's/.*;//' | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')"
            
            if [[ "$p1" != \#* ]]; then
              echo -e "${p1}\t${p2}\t${p3}\t${p4}\t${p5}\t${p6}"
            fi
        fi
    fi
done < ./TableB_STD_0_35.txt

