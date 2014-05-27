#!/bin/bash

# This script will just display the usage information.
function usage()
{
   echo "Usage: $0 OPTION [-nobinlightning]"
   echo "   -delta    perform a build of only the rpms that are likely to change."
   echo "   -full     perform a full build of all the rpms."
   echo "   -ade      build all rpms that are packaged in the ade."
   echo "   -viz      only build the Viz rpms (CAVE & AlertViz)."
   echo "   -edex     only build the EDEX rpms."
   echo "   -qpid     build only the QPID rpms."
   echo "   -ldm      build the awips2-ldm rpm; requires root privileges."
   echo "   -dev      call functions directly"
   echo "   -package  create a yum repository tar file with the rpms that were just built."
   echo "   --help    display this message and exit."

   return 0
}
