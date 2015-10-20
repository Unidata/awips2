#!/bin/bash

# This delta script is for DR 4897. This delta script has been created
# to remove all generated fog, safeseas, and snow index.xml files. The
# base versions of the files are now overriden by EDEX.

_localization_directory=/awips2/edex/data/utility/cave_static/configured
for site_dir in `ls -1 ${_localization_directory}`; do
   site_loc_dir=${_localization_directory}/${site_dir}

   if [ -f ${site_loc_dir}/menus/fog/index.xml ];then
      echo "Removing: ${site_loc_dir}/menus/fog/index.xml ..."
      rm -f ${site_loc_dir}/menus/fog/index.xml
      if [ $? -ne 0 ]; then
         echo "Update Failed!"
         exit 1
      fi
   fi
   if [ -f ${site_loc_dir}/menus/safeseas/index.xml ];then
      echo "Removing: ${site_loc_dir}/menus/safeseas/index.xml ..."
      rm -f ${site_loc_dir}/menus/safeseas/index.xml
      if [ $? -ne 0 ]; then
         echo "Update Failed!"
         exit 1
      fi
   fi
   if [ -f ${site_loc_dir}/menus/snow/index.xml ];then
      echo "Removing: ${site_loc_dir}/menus/snow/index.xml ..."
      rm -f ${site_loc_dir}/menus/snow/index.xml
      if [ $? -ne 0 ]; then
         echo "Update Failed!"
         exit 1
      fi
   fi
done

echo "Update Completed Successfully."
exit 0
