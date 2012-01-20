#!/bin/bash

export DELTA_BUILD="11.3"
export DELTA_ID="A21Proto1"
export DELTA_DESC="Removing Any Existing FFMP Templates So New Ones Can Be Generated."

function runUpdate()
{
   # Ensure that the awips2-hydroapps have been installed.
   rpm -q awips2-hydroapps > /dev/null
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      return 0
   fi

   # Ensure that we know where awips2-hydroapps has been installed too.
   if [ "${COMPONENT_INSTALL}" = "" ]; then
      return 1
   fi

   return 0;
   FFMP_TEMPLATE_DIR="${COMPONENT_INSTALL}/edex/data/hdf5/hydroapps/ffmp_templates"
   # Ensure that the ffmp_template directory exists.
   if [ ! -d ${FFMP_TEMPLATE_DIR} ]; then
      return 0
   fi

   # Purge the existing ffmp templates.
   rm -rf ${FFMP_TEMPLATE_DIR}/*
   RC="$?"
   if [ ! "${RC}" = "0" ]; then
      return 1
   fi

   return 0
}
