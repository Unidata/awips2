
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

__version__ = "1.0"

class SimpleLog():
    """
    Log messages to standard out
    
    SOFTWARE HISTORY
    Date            Ticket#        Engineer    Description
    ------------    ----------     ----------- --------------------------
    Jul 09, 2008    1222           jelkins     Initial version
    
    @author: jelkins
    
    Class with a default method handler idea from:
    http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/307618
    """
    def __init__(self,logName):
        self.logName = logName
    
    def handlerFunctionClosure(self,name):
        def handlerFunction(message):
            
            from time import strftime
            dateTime = strftime("%Y-%m-%d %H:%M:%S,000")
            
            from string import upper
            print "%s %s %s: %s" % (upper(name),dateTime,self.logName,message)
            
        return handlerFunction
    def __getattr__(self,name):
        return self.handlerFunctionClosure(name)