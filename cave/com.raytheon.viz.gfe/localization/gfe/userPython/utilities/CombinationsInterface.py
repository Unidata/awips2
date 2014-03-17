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
# Interface for retrieving combinations
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/25/08                      njensen       Initial Creation.
#    09/05/13             #2329    randerso      Added error handling
#    02/06/2014           #2591    randerso      Changed log level to debug
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
    
        
