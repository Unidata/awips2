#!/usr/bin/env python
##
##

##
# This is a base file that is not intended to be overridden.
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
