#!/bin/bash
# AWIPS2 #5252
# Looks for postProcessedModels.xml localization files with GFSPostProcessor, 
# and if found, removes that processor. If GFSPostProcessor is the only processor
# defined for a model, the whole postProcessedModel block is removed and a comment
# stating so is added.


for file in `find /awips2/edex/data/utility/edex_static/ -name postProcessedModels.xml`
do
    level=`echo $file | cut -f 7 -d '/'`
    if [ $level != 'base' ] # base is assumed to be correct as deployed.
    then
        if grep -q -- 'processorName>\s*GFSPostProcessor' $file
        then
            echo "Updating $file"
            newFile="${file}_`date +%s`.dr5252"
            
            # Offload xml processing to python. Output will be written to $newFile
            python removeGFSPostProcessor.py "${file}" "${newFile}"

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
            echo "Skipping $file - no changes needed."
        fi
    fi
done
