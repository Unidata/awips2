#!/usr/bin/env python
##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

import string

# DefaultEditAreaNaming
# this function defines the default edit area naming convention
# for use in Maps.py/LocalMaps.py and the MapManager

# could be a string, which is the edit area name (attribute)
# could be a list, which is the edit area name set of attributes
# "ZONE"

def defaultEditAreaNaming(info, eanDefinition):
    # simple case, the edit area name definition is the attribute key
    if type(eanDefinition) == str:
        if info.has_key(eanDefinition):
            return info[eanDefinition]
        else:
            return eanDefinition

    elif type(eanDefinition) == list:
        s = ''
        for e in eanDefinition:
            # valid attribute
            if info.has_key(e):
                if len(s) == 0:
                    s = info[e]
                else:
                    s = s + "_" + info[e]
            # not valid attribute, so use definition directly
            else:
                if len(s) == 0:
                    s = e
                else:
                    s = s + "_" + e

        return s


    else:
        return ''


def getEditAreaName(info, nameAttr):
    if callable(nameAttr):
        return nameAttr(info)
    return defaultEditAreaNaming(info, nameAttr)
