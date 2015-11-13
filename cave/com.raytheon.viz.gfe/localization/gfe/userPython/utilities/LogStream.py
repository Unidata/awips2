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

import sys, inspect, string, traceback

#
# Provides a wrapper to the Java logging classes
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/14/08                      njensen       Initial Creation.
#    
# 
#

try:
    from com.raytheon.viz.gfe.log import VizLogStream as JavaLogStream
except:    
    # must be on the server side
    try:    
        from com.raytheon.edex.plugin.gfe.log import EdexLogStream as JavaLogStream
    except:
        # must be in python command line
        import PyLogStream as JavaLogStream
    
    
def logEvent(*args):
    msg = ""
    for arg in args:
        msg += str(arg) + " "
    JavaLogStream.logEvent(msg)
    return msg
    
def logProblem(*args):
    ln = _line()
    fl = _file()
    msg = fl + " line " + str(ln) + ": "
    for arg in args:
        msg += str(arg) + " "
    JavaLogStream.logProblem(msg)
    return msg
    
def logVerbose(*args):
    msg = ""
    for arg in args:
        msg += str(arg) + " "
    JavaLogStream.logVerbose(msg)
    return msg
    
def logDebug(*args):
    msg = ""
    for arg in args:
        msg += str(arg) + " "
    JavaLogStream.logDebug(msg)
    return msg

def logUse(*args):
    logDebug(*args)
    
def exc():
    t, v, tb = sys.exc_info()
    return string.join(traceback.format_exception(t, v, tb))
    
def _line():
    return inspect.currentframe().f_back.f_back.f_lineno
    
def _file():
    return inspect.currentframe().f_back.f_back.f_code.co_filename
        
