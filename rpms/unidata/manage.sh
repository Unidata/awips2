#!/bin/bash
export LD_LIBRARY_PATH=/usr/lib:/lib:/usr/lib64:/lib64
if [ -z "$1" ]
  then
  exit 1
fi
unset LD_LIBRARY_PATH
if [ -d "$1" ]; then
  repomanage -k3 --old ${1} | xargs rm -f
  createrepo -g ${1}/comps.xml ${1}
fi
