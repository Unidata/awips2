#!/bin/bash
# DR #5495 - Edex now converts all input soundingBufr station files placed in the ndm
# drop to /common_static/configured.  Script runs on active EDEX Server: (dx3 or dx4).
# All (.spi) files 'xxx.spi' will be moved from <Start> to <Destination>:
#<Start>:       /awips2/edex/data/utility/common_static/site/XXX/basemaps/xxx.spi
#<Destination>: /awips2/edex/data/utility/common_static/configured/XXX/basemaps/xxx.spi

{
numMovableSiteFiles="$(find /awips2/edex/data/utility/common_static/site/*/basemaps -name poesBufr.spi -o -name goesBufr.spi | wc -l)" 
} &> /dev/null  # Count movable files

if [ $numMovableSiteFiles -eq 0 ] 
then
  echo "No poesBufr or goesBufr site (.spi) files found to move from /basemaps subfolders of /awips2/edex/data/utility/common_static/site/*"

else
  echo "Found $numMovableSiteFiles poesBufr or goesBufr (.spi) files to move:"
  echo " from /awips2/edex/data/utility/common_static/site/*/basemaps"
  echo " "

  find /awips2/edex/data/utility/common_static/site/ -maxdepth 1 -mindepth 1 -type d | while read subfolder
    do

    find "$subfolder" -name poesBufr.spi -o -name goesBufr.spi | while read pathToFile
      do

      SITE_CODE=${pathToFile:45:3}                           # site code (XXX)
      FILE_PATH_AFTER_SITE_CODE=${pathToFile:48}             # Path with filename

      # (Create destination if missing)
      echo "Moving (.spi) file: $FILE_PATH_AFTER_SITE_CODE"      
      mkdir -p /awips2/edex/data/utility/common_static/configured/"$SITE_CODE"/basemaps
      mv /awips2/edex/data/utility/common_static/site/"$SITE_CODE"/"$FILE_PATH_AFTER_SITE_CODE" /awips2/edex/data/utility/common_static/configured/"$SITE_CODE"/"$FILE_PATH_AFTER_SITE_CODE"
    done
  done
fi
