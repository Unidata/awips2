#!/bin/bash

# Remove any logs from a week ago, if they exist.

_PYPIES_LOG_DIRECTORY="/awips2/httpd_pypies/var/log/httpd"

_LOG_NAME_PREFIXES=( 'access_log' 'error_log' )
_COUNT_DAYS=( 7 8 9 10 11 12 13 14 )

for day in ${_COUNT_DAYS[*]}; do
   _log_date=`date -d "-${day} day" +%Y.%m.%d`
   
   for logPrefix in ${_LOG_NAME_PREFIXES[*]}; do
      _log_file="${logPrefix}.${_log_date}"

      echo "${_PYPIES_LOG_DIRECTORY}/${_log_file}"
      rm -f ${_PYPIES_LOG_DIRECTORY}/${_log_file}
   done
done
