#!/bin/bash
#
# DR 6355 
# relocate volumebrowser/LevelMappingFile.xml into level/mappings/
#

BASE='/awips2/edex/data/utility/'
DEST='level/mappings/'

for file in `find $BASE -wholename  \*/volumebrowser/LevelMappingFile.xml`;
do
    dir=`echo $file | sed 's/\/volumebrowser\/LevelMappingFile.xml//g'`;
    destDir=${dir}/${DEST}

    if [ ! -d "${destDir}" ];
    then
        sudo -u awips mkdir -p ${destDir};
    fi

    echo "Moving ${file} to ${destDir}";
    mv $file $destDir;
done;
