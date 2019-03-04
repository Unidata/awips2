##
##

#
# Interface for retrieving combinations
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/25/08                      njensen        Initial Creation.
#    09/05/13             #2329    randerso       Added error handling
#    02/06/2014           #2591    randerso       Changed log level to debug
#    11/11/2016           19293    randerso       Moved CombinationsInterface to common
#

import sys, traceback, os, time, LogStream
from java.util import ArrayList

def getCombinations(comboName): 
    try:
        outercombos = ArrayList()
        md = __import__(comboName)
        comList = md.Combinations
        for i in comList:        
            combos = ArrayList()
            innerList = i[0]
            for zone in innerList:
                combos.add(zone)
            outercombos.add(combos)
        return outercombos
     
    except AttributeError as e:
        filename = md.__file__
        if filename.endswith("pyc") or filename.endswith("pyo"):
            filename = filename[:-1]
        with open(filename,'r') as fd:
            filecontents = fd.read()
        
        LogStream.logDebug("ERROR loading combinations file: "+ comboName + 
              "\nmd.__file__: " + md.__file__ + 
              "\ndir(md): " + str(dir(md)) + 
              "\n" + md.__file__ + " last modified: " + time.strftime("%Y-%m-%d %H:%M:%S",time.gmtime(os.path.getmtime(md.__file__))) +
              "\n" + filename + " last modified: " + time.strftime("%Y-%m-%d %H:%M:%S",time.gmtime(os.path.getmtime(filename))) +
              "\nContents of " + filename + "\n" + filecontents)
        raise e
    
        
