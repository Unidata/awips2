#!/bin/bash

# Script to backup installer files
# Args:
#   $1 :: (optional) base location of files to archive

if [ -n "${1}" ]
then
   BASE_DIR=${1}
else
   BASE_DIR=/share1/installers
fi
ARCHIVE_DIR=${BASE_DIR}/install_archive
DATE=`date +%m-%d-%y`
echo "performing copy on ${DATE}"
echo "archive directory is ${ARCHIVE_DIR}"

# function to perform the archiving
archive() {
   # $1 :: src dir
   # $2 :: dst dir
   # $3 :: pattern
   CHECK=`ls ${1}/${3} | wc -l`
   if [ ${CHECK} -ne 0 ]
   then
      for a in ${1}/${3}
      do
         FILE_NAME=`basename ${a}`
         FILE_NAME_BASE=`echo ${FILE_NAME} | cut -d '.' -f 1`
         FILE_NAME_EXT=`echo ${FILE_NAME} | cut -d '.' -f 2`
         ARCHIVE_NAME="${FILE_NAME_BASE}-${DATE}.${FILE_NAME_EXT}"
         echo "   saving '${FILE_NAME}' as '${ARCHIVE_NAME}'"
         cp ${a} ${2}/${ARCHIVE_NAME}
      done
   fi
}

#function to delete old files
delete() {
   # $1 :: dir to trim
   # $2 :: pattern
   find ${1}/${2} -mtime +13 -exec rm -f {} \; 
}

echo "archiving bundle scripts"
echo "archiving started at `date`"
archive ${BASE_DIR} ${ARCHIVE_DIR} "awips*.sh"

echo "archiving component scripts"
archive ${BASE_DIR} ${ARCHIVE_DIR} "ade*.sh"

echo "archiving bundle installers"
archive ${BASE_DIR}/install_files ${ARCHIVE_DIR}/install_files "*.jar"

echo "archiving component installers"
archive ${BASE_DIR}/install_files/components ${ARCHIVE_DIR}/install_files/components "*.jar"

echo "archiving of nightly installers complete"

echo ""

echo "deleting files older than 14 days"
delete ${ARCHIVE_DIR} "*.sh"
delete ${ARCHIVE_DIR}/install_files "*.jar"
delete ${ARCHIVE_DIR}/install_files/components "*.jar"
echo "done deleting old files"

echo "archiving completed at `date`"
exit 0
