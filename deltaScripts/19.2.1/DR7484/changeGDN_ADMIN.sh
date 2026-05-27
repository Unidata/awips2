#! /bin/bash
#
# DR 7484 - replace GDN_ADMIN with AV_ADMIN in all alertviz config files
#

BACKUP_EXT=".DR7484.bak"

if [ -z $@ ]
then
    echo "INFO: Running delta script for DR 7484: Replacing GDN_ADMIN with AV_ADMIN"
    for f in `find /awips2/edex/data/utility/cave_static/*/*/alertViz/configurations \
               /awips2/edex/data/utility/cave_static/*/*/alertViz/customizations \
               /awips2/edex/data/utility/cave_static/*/*/alertViz \
              -maxdepth 1 -type f -name "*.xml"`
    do 
        echo "Updating $f..."
        
        # don't overwrite backup file if it already exists
        if [ -f ${f}${BACKUP_EXT} ]
        then
            ext=""
            else
            ext=${BACKUP_EXT}
        fi
        sed -i${ext} -e "s/GDN_ADMIN/AV_ADMIN/g; s/Guardian/AlertViz/g" $f
    done
    
    echo "INFO: Script complete."
    echo "INFO: To roll back changes made by this script run:"
    echo "     $0 --rollback"
elif [ "$@" == "--rollback" ]
then
    echo "INFO: Rolling back changes for DR 7484"

    for f in `find /awips2/edex/data/utility/cave_static/*/*/alertViz/configurations \
               /awips2/edex/data/utility/cave_static/*/*/alertViz/customizations \
               /awips2/edex/data/utility/cave_static/*/*/alertViz \
              -maxdepth 1 -type f -name "*${BACKUP_EXT}"`
    do
        mv ${f} ${f/${BACKUP_EXT}/} 
    done 

    echo "INFO: Roll back complete."
else
    echo "ERROR: Unrecognized command line parameter: $@"
    echo "Usage: `basename $0` [--rollback]"
    echo "    --rollback roll back changes made by this script"
fi