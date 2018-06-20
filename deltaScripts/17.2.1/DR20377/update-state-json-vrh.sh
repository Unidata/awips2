#!/bin/sh
# DR 20377 deltaScript for site VRH since Qpid is running on the PXs there

. /data/fxa/INSTALL/awips2/scripts/.global || exit 1

case "${SITE_IDENTIFIER}" in
    ${remCPCaseArray} ) dir=$(cd "$(dirname "$0")"; pwd)
                        "$dir"/update-state-json-vrh.py
                        a2pgca refresh server:cp1f ;;
esac
