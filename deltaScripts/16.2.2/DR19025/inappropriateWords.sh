#!/bin/bash
#
# Script to merge new inapporpriateWords.txt file with site level file
#

for file in `find /awips2/edex/data/utility/cave_static -name inappropriateWords.txt`
do
   echo Merging new inappropriate words into $file
   mv $file /tmp
   echo -e "# This file defines words that we do not allow the spellchecker to display as
# spelling suggestions.
#
# Overrides to this file are not incremental so if you want this base list
# included in your override, you must copy this word list to the override file.
#" > $file
   cat /data/fxa/INSTALL/awips2/scripts/deltaScripts/inappropriateWords.txt /tmp/inappropriateWords.txt | grep -v "#" | sort | uniq >> $file
   rm -f /tmp/inappropriateWords.txt
done
