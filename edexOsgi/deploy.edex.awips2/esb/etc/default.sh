#!/bin/bash
##
# This is the default environment settings file for all EDEX modes. All
# non-operational and/or development EDEX instances will only use this file.
# All operational EDEX instances will utilize an instance-specific file
# that will override some or all of the environment settings in this file. 
# In the case that a new operational EDEX mode does not need to override any
# settings in this file, an instance-specific logback configuration file should
# be created for the new operational instance at a minimum. 
##
export INIT_MEM=512 # in Meg
export MAX_MEM=4096 # in Meg
export MAX_PERM_SIZE=128m
export EDEX_JMX_PORT=1616
export EDEX_DEBUG_PORT=5005
export METADATA_POOL_MAX=50
export METADATA_POOL_TIMEOUT=300
export DEBUG_PARAM_1=""
export DEBUG_PARAM_2=""
export DEBUG_PARAM_3=""
export DEBUG_PARAM_4=""
export PROFILER_PARAM_1=""
export PROFILER_PARAM_2=""
export PYPIES_MAX_CONN=50


export SERIALIZE_POOL_MAX_SIZE=16
export SERIALIZE_STREAM_INIT_SIZE_MB=2
export SERIALIZE_STREAM_MAX_SIZE_MB=6

export MGMT_PORT=9600

export WRAPPER_DEADLOCK_ACTION=RESTART
export WRAPPER_ON_EXIT_ACTION=RESTART
export WRAPPER_TRIGGER_ACTION=RESTART
export WRAPPER_USE_SYSTEM_JAVA=false

export SOFT_REF_LRU_POLICY_MS_PER_MB=1000
