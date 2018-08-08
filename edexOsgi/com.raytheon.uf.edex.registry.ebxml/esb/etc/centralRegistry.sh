ntralRegistry mode was designed to be an ebXML registry plus all the
# components necessary to harvest data from a data provider and manage
# SBN/Shared Subscriptions. Anything registry-related that is meant to run
# only once for all of the NWS would run in this mode.
##
export MAX_MEM=3072
export HTTP_PORT=9588
export EDEX_DEBUG_PORT=5011
export METADATA_POOL_TIMEOUT=60
export CLUSTER_ID=OAX
export SOFT_REF_LRU_POLICY_MS_PER_MB=50
