#!/bin/bash

# DR #1846 - this update script will re-arrange the existing modelsounding hdf5 files to divide them by
# model name and site. Currently, every file will be copied to every potential path that it could be
# accessed at. But, any new files that are written after the upgrade is complete, will contain only
# the minimum amount of required data.

# ensure that we actually have modellsounding data to re-arrange
DATA_DIRECTORY="/awips2/edex/data/hdf5/modelsounding"

if [ ! -d ${DATA_DIRECTORY} ]; then
   echo "INFO: No Model Sounding Data Was Found On The System!"
   echo "INFO: Update Terminated ..."
   exit 0
fi

# determine where we are
path_to_script=`readlink -f $0`
dir=$(dirname $path_to_script)

# first, retrieve all possible models
PSQL="/awips2/psql/bin/psql"
SQL="SELECT DISTINCT reporttype FROM awips.modelsounding ORDER BY reporttype;"
_modelslist=modelslist.txt

echo "INFO: update started."
pushd . > /dev/null 2>&1
cd ${DATA_DIRECTORY}

# retrieve the models
${PSQL} -U awips -d metadata -c "${SQL}" -t -o ${_modelslist}
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to retrieve the list of models."
   echo "FATAL: The update has failed."
   exit 1
fi

PYTHON="/awips2/python/bin/python"
_python_script="${dir}/determineRefTimeDirectory.py"
_python_script2="${dir}/modelsoundingFileName.py"
_fcsthourslist=fcsthourslist.txt

# now loop through the models
for model in `cat ${_modelslist}`; do
   # create a directory for the model.
   mkdir -p ${DATA_DIRECTORY}/${model}
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to create directory - ${DATA_DIRECTORY}/${model}!"
      echo "FATAL: The update has failed."
      exit 1
   fi

   # retrieve the potential forecast hours for the model that we are
   # currently processing.
   SQL_FCST_HOUR="SELECT DISTINCT (fcstSeconds / 3600) AS forecastHour FROM modelsounding WHERE reporttype = '${model}' ORDER BY forecastHour;"
   ${PSQL} -U awips -d metadata -c "${SQL_FCST_HOUR}" -t -o ${_fcsthourslist}

   # loop through the hdf5 files
   for file in `ls -1 *.h5`; do
      # determine which reftime directory would be associated with the file
      reftimeDirectory=`${PYTHON} ${_python_script} "${file}"`
      if [ $? -ne 0 ]; then
         echo "FATAL: The update has failed."
         exit 1
      fi

      # create the reftime directory
      mkdir -p "${DATA_DIRECTORY}/${model}/${reftimeDirectory}"
      if [ $? -ne 0 ]; then
         echo "ERROR: Failed to create directory - ${DATA_DIRECTORY}/${model}/${reftimeDirectory}!"
         echo "FATAL: The update has failed."
         exit 1
      fi

      # loop through the possible forecast hours
      for fcstHour in `cat ${_fcsthourslist}`; do
         # determine the new name of the file
         destinationFile=`${PYTHON} ${_python_script2} "${file}" "${model}" ${fcstHour}`
         if [ $? -ne 0 ]; then
            echo "ERROR: Failed to determine the adjusted name of file - ${file}!"
            echo "FATAL: The update has failed."
            exit 1
         fi

         # create a link between the files
         ln ${file} ${DATA_DIRECTORY}/${model}/${reftimeDirectory}/${destinationFile}
         if [ $? -ne 0 ]; then
            echo "ERROR: Failed create a link for ${file} to ${DATA_DIRECTORY}/${model}/${reftimeDirectory}/${destinationFile}!"
            echo "FATAL: The update has failed."
            exit 1
         fi
      done
   done

   rm -f ${_fcsthourslist}
   if [ $? -ne 0 ]; then
      echo "WARNING: Failed to remove temporary file - ${_fcsthourslist}."
   fi
done

# remove the models list text file
rm -f ${_modelslist}
if [ $? -ne 0 ]; then
   echo "WARNING: Failed to remove temporary file - ${_modelslist}."
fi

# remove the hdf5 files
rm -f *.h5
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to remove the obsolete hdf5 files!"
   echo "WARNING: Removing the files manually is recommended."
fi

popd > /dev/null 2>&1

echo "INFO: the update has completed successfully!"

exit 0












