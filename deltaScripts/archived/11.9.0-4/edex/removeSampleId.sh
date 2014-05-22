#!/bin/bash
# Script to remove sampleId tags from sample sets
if [ -d /awips2/edex/data/utility/common_static ]; then
  if [ "$1" == "removeBak" ]; then
     for bak in `ls -1 /awips2/edex/data/utility/common_static/*/*/gfe/sampleSets/*.xml.bk$`; do
     	echo "Removing $bak"
	rm $bak
     done
  else
     for ss in `ls -1 /awips2/edex/data/utility/common_static/*/*/gfe/sampleSets/*.xml`; do
        echo "Editing $ss..."
        sed -i.bk$ -e 's/<sampleId.*\/>//' $ss
     done
     echo 
     echo "  Update complete. Please verify you sample sets are still loading correctly."
     echo "  If you find an issue you can restore your previous version from the file with the .bk$ extension"
     echo "  and manually remove the <sampleId .../> tag."
     echo "  Once you have verified all your sample sets are ok rerun this script with removeBak to remove the .bk$ files"
     echo "  Example:"
     echo "  $0 removeBak"
  fi
fi
