#!/usr/local/perl/bin/perl -w

use Env;

# This script is called by stopingest.  
# this script which is needed to stop a recordClimate process.

system (" ${FXA_HOME}/bin/killProc recordClimate");
