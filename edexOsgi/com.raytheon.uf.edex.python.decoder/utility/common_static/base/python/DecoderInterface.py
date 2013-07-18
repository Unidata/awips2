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

import zipimport
import JUtil

from java.util import ArrayList

#
# Interface for python decoders
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/22/08                      njensen       Initial Creation.
#    
# 
#
def loadModule(pluginDir, pluginFQN, moduleName):
    jarname = pluginDir + pluginFQN + ".jar"

    if not sys.modules.has_key(moduleName):
        jar = zipimport.zipimporter(jarname)
        jar.load_module(moduleName)

def decode(moduleName, fileToDecode, commandArgs=None):
    mod = sys.modules[moduleName]
    exec 'dec = mod.' + moduleName + '(filePath=fileToDecode)'
    if commandArgs is not None:
        dec.setCommand(commandArgs)
    result = dec.decode()
    resultList = ArrayList()
    if result is not None:
        for resultDict in result:
            if type(resultDict) == dict:
                hashmap = JUtil.pyDictToJavaMap(resultDict)
                resultList.add(hashmap)
            else:
                resultList.add(resultDict)    
    return resultList
    