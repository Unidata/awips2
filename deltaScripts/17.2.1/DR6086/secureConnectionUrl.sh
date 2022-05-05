#!/bin/sh
# DR 6086 - Update Radar Server / rcm to utilize SSL to connect to Qpid

sed -i "s/:5672'</:5672'\&amp;ssl='true'</g" /awips2/rcm/data/config/persist/config.xml