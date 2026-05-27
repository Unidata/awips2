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
# Aug 11, 2023 2034216    sharbison   Initial Creation of Table D parser
#
##

# tableDParser.sh - used to parse table D and convert to awips usable format.

defaultURI="https://raw.githubusercontent.com/NOAA-EMC/NCEPLIBS-bufr/develop/tables/bufrtab.TableD_STD_0_"
defaultVersion="35"

usageText="Usage:
  ***WARNING (Known Parser Bugs, Manual Cleanup required)***: 
    *Bug 1) Record(s) 3 06 005 (3-06-005) parser inserts as blank, add manually!
    *Bug 2) Header needs to be cleaned up manually.
  example (help/usage) no params supplied or help command:
    bash tableDParser.sh > BUFR_Table_D.txt
  example (file) supply Table D version:
    bash tableDParser.sh -f > BUFR_Table_D.txt
  example (auto curl) supply Table D version and curl for raw table d contents automatically.
    bash tableDParser.sh -v $defaultVersion > BUFR_Table_D.txt
      **Note: The default gihub URI may stop working, or need modification in the future:
  example (manual curl) supply curl uri manually:
    bash tableDParser.sh -c $defaultURI$defaultVersion > BUFR_Table_D.txt
"

if [[ ! -n $1 ]];
then 
    echo "No parameter passed."
    echo "${usageText}"
    exit 0
fi

[ $# -eq 0 ] && usage

while getopts ":v:c:f:" arg; do
    case "${arg}"
        in
        v)tableVersion=${OPTARG}
            ;;
        c)tableURI=${OPTARG}
            ;;
        f)inputFileName=${OPTARG}
            ;;
        *)
            echo "${usageText}"
            exit 0
            ;;
        h | *) # Display help.
          echo "${usageText}"
          exit 0
          ;;
    esac
done

# Collect the raw data from the file or curl command.

rawTableData=""

if [ ! -z "$tableVersion" ];then
    tableURI=$defaultURI
    rawTableData=$(curl -s ${tableURI}${tableVersion})
elif [ ! -z "$tableURI" ];then
    tableVersion=$defaultVersion
    tableURI=$defaultURI
    rawTableData=$(curl -s ${tableURI}${tableVersion})
elif [ ! -z "$inputFileName" ];then
    rawTableData=$(<${inputFileName})
else
    echo "${usageText}"
    exit 0
fi

# Read in the raw table data and format the lines to be in the AWIPS desired format.
# Note: the last sub-descriptor does not have a trailing comma.
#
# ex/
# Original data format (after header):
#  3-00-002 | TABLACAT   ;     ; 
#           | 0-00-002 > | Table A: data category description, line 1
#           | 0-00-003   | Table A: data category description, line 2
#
# Final AWIPS desired data format:
# 3 00 002:
#  0 00 002,
#  0 00 003

while read line
do
   IFS='|' read -r -a array <<< "$line"
   if [ ${#array[@]} -eq 2 ]; then
      # Main Descriptor (ex: 3 00 002)
      # line has 2 values
      # ignore first line that start with 'Table'
      # ignore lines with hashtag
      if ! [[ "${array[0]}" =~ ^#|Table ]]; then
        # format the main descriptor line descriptor to 'space-delimited' format
        # '0-00-000' => '0 00 000'
        myVar=`echo "${array[0]}" | sed 's/-/ /g' | sed 's/ *$//g'`
        echo "${myVar}:"
      fi
   elif [ ${#array[@]} -eq 3 ]; then
      # Sub Descriptor (ex: 0 00 002)
      # line has 3 values
      # ignore first line that start with 'Table'
      # ignore lines with hashtag
      if ! [[ "${array[1]}" =~ ^#|Table ]]; then
        # format the sub descriptor line descriptor to 'space-delimited' format
        # '0-00-000' => '0 00 000'
        # sub descriptors need a space prefix (in the front) and a comma postfix (at the end)
        # no trailing comma for the last record
        myVar=`echo "${array[1]}" | sed 's/-/ /g' | sed 's/ >/,/g' | sed 's/ *$//g'`
        echo "${myVar}"
      fi
   fi
done < <(printf '%s\n' "$rawTableData")

