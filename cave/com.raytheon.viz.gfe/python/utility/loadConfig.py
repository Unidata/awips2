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
#
# Loads a gfe config file into a hashmap
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer  Description
# ------------- -------- --------- ---------------------------------------------
# Dec 11, 2009           njensen   Initial Creation.
# Apr 02, 2014  2729     randerso  Fixed error handling in loadPreferences
# Jan 19, 2018  6594     randerso  Updated to reflect Jep 3.6 now returns Python
#                                  floats as Doubles.
# Jan 23, 2018  7153     randerso  Changes to allow new GFE config file to be
#                                  selected when perspective is re-opened.
##

import types
from java.util import HashMap, ArrayList
from java.lang import String, Double, Integer, Boolean

def loadPreferences(config):
    try:
        # import the config file
        if type(config) is str:
            configName = config
            __import__(config)
        elif type(config) is types.ModuleType:
            configName = config.__name__

        from com.raytheon.viz.gfe import Activator
        Activator.getDefault().loadConfiguration(configName)
        prefs = Activator.getDefault().getPreferenceStore()

        return prefs
    except Exception as e:
        import LogStream
        import traceback
        LogStream.logProblem("Unknown or invalid config file: %s\n%s" % (configName, traceback.format_exc()))
        raise


def loadConfig(configName):
    mod = __import__(configName)
    return getGlobals(mod)

def getGlobals(mod):
    mp = HashMap()
    for attrName in mod.__dict__:
        if not attrName.startswith('__'):
            attr = mod.__getattribute__(attrName)
            t = type(attr)
            if t is not list:
                if t is str:
                    mp.put(attrName, attr)
                elif t is int:
                    mp.put(attrName, Integer(attr))
                elif t is float:
                    mp.put(attrName, Double(attr))
                elif t is bool:
                    mp.put(attrName, Boolean(attr))
            else:
                arr = None
                if len(attr) > 0:
                    t = type(attr[0])
                    if t is int:
                        arr = __fillArray(attr, Integer)
                    elif t is float:
                        arr = __fillArray(attr, Double)
                    elif t is str:
                        arr = __fillArray(attr, String)
                mp.put(attrName, arr)
    return mp

def __fillArray(pylist, jclz):
    sz = len(pylist)
    jlist = ArrayList(sz)
    for i in range(sz):
        jlist.add(jclz(pylist[i]))
    return jlist
