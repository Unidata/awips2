#! /bin/bash
#
# Removes FFMP bin file caches so they will be regenerated using 
# full resolution geometry
#
echo "RODO DR #6642: remove FFMP bin files so they will be regenerated"

rm -rf /awips2/edex/data/utility/common_static/configured/*/ffmp/ffti/*
rm -rf /awips2/edex/data/utility/common_static/configured/*/ffmp/sources/*
rm -rf /awips2/edex/data/utility/common_static/configured/*/ffmp/templates/*
rm -rf /awips2/edex/data/utility/common_static/site/*/ffmp/aggrGeom/*

echo "RODO DR #6642: Successfully removed FFMP bin files."
echo "Please start/restart ingestDAT JVM to regenerate them."