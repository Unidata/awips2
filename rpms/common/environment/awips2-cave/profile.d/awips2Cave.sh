#!/bin/bash
if [[ $USER != "awips" && $USER != 'root' ]]; then
  return
  exit 0
fi

# Determine where cave has been installed.
CAVE_INSTALL="/awips2/cave"
export TMCP_HOME=${CAVE_INSTALL}/caveEnvironment
export FXA_HOME=${CAVE_INSTALL}/caveEnvironment
