#!/bin/bash -v
#
# Unidata AWIPS envvars
# 

# for all users to have 'edex' in $PATH
if [ -d /awips2/edex/bin ]; then
  PATH=/awips2/edex/bin:$PATH
fi

# Uncomment below to force AWIPS env vars ONLY for user 'awips'
#if [[ ${USER} != "awips" ]]; then
#  return
#fi

if [[ ${USER} = "root" ]]; then
  # to avoid ownership corruption of the product queue
  # alias to the LDM service (which runs ldm as user awips)
  if [ -f /etc/init.d/edex_ldm ]; then
    alias ldmadmin='service edex_ldm'
  fi
  # to avoid python mismatch with yum
  export LD_LIBRARY_PATH=/usr/lib64
  return
fi

# CAVE alias
alias cave='/awips2/cave/run.sh'
if [ -d /awips2/cave ]; then
  export TMCP_HOME=/awips2/cave/caveEnvironment
  export FXA_HOME=/awips2/cave/caveEnvironment
fi

# Add to $PATH and $LD_LIBRARY_PATH
add_bin() {
  if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
    export PATH="${1}/bin:${PATH}"
  fi
}
add_dir() {
  if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
    export PATH="${1}:${PATH}"
  fi
}
add_lib() {
  if [ -d "$1" ] && [[ ":$LD_LIBRARY_PATH:" != *":$1:"* ]]; then
    export LD_LIBRARY_PATH="${1}/lib:${LD_LIBRARY_PATH}"
  fi
}
add_all(){
  add_bin $1
  add_lib $1
}
# add to PATH
add_bin /awips2/java
add_bin /awips2/fxa
add_bin /awips2/groovy
add_bin /awips2/ant
add_bin /awips2/ldm
add_bin /awips2/GFESuite
# add to PATH and LD_LIBRARY_PATH
add_all /awips2/python
add_all /awips2/postgresql
add_all /awips2/psql
add_all /awips2/tools
# add LD_LIBRARY_PATH
add_lib /awips2/yajsw
add_lib /awips2/qpid
add_lib /awips2/ldm
# ldm directories for PATH
add_dir /awips2/ldm/util
add_dir /awips2/ldm/decoders

# _HOME envvars
if [ -d /awips2/ant ]; then
  export ANT_HOME=/awips2/ant
fi
if [ -d /awips2/yajsw ]; then
  export YAJSW_HOME=/awips2/yajsw
fi
if [ -d /awips2/groovy ]; then
  export GROOVY_HOME=/awips2/groovy
fi
if [ -d /awips2/java ]; then
  export JAVA_HOME=/awips2/java
fi
if [ -d /awips2/ldm ]; then
  export LDMHOME=/awips2/ldm
fi
