#!/bin/bash
# AWIPS2 #3756
#
# Updates the non-base postProcessedModels.xml localization files to replace 
# precipitation post-processors that have been removed from the baseline with
# the configurable PrecipAccumPostProcessor.
# 
#

for file in `find /awips2/edex/data/utility/edex_static/ -name postProcessedModels.xml`
do
    level=`echo $file | cut -f 7 -d '/'`
    if [ $level != 'base' ] # base is assumed to be correct as deployed.
    then
        checkForUpdate=`grep PrecipAccumPostProcessor $file`
        if [ "${checkForUpdate}" == "" ]
        then
            echo "Updating $file"
            # create a copy of the file with the changes in a temporary file 
            newFile="${file}_`date +%s`.dr3756" 
            cat $file | sed 's/com.raytheon.edex.plugin.grib.decoderpostprocessors.//g' | sed -r 's/Nam80PostProcessor|CanadianNHPostProcessor|CanadianRegPostProcessor|gov.noaa.nws.crh.edex.grib.decoderpostprocessor.GFS20PostProcessor/PrecipAccumPostProcessor/g' | sed 's/<processorName>ECMWFHiResProcessor<\/processorName>/<processorName>ECMWFHiResProcessor<\/processorName>\n        <processorName>PrecipAccumPostProcessor<\/processorName>/g' > $newFile
            if [ -s $newFile ]
            then
                chmod --reference=$file $newFile
                chown --reference=$file $newFile
                mv $newFile $file 
            else
                echo "Failed to update $file"
                if [ -e $newFile ]
                then
                    rm $newFile
                fi
            fi
        else
            echo "$file has already been updated and will be skipped."
        fi
    fi
done
