#!/bin/bash 
while read p; do
  if [ ! -d "utility/common_static/configured/$p" ]; then
    #echo "No warngen geometries for $p"
    echo $p
  fi
done <wfos.sql.txt

