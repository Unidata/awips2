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
##############################################################################
# Contains utility methods used by the Command Line Interface (CLI) tools.
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    10/28/08        1585          MW Fegan       Initial Creation.
#    10/09/12        DR 13901      D. Friedman    Add doWithinTime
##############################################################################
import sys
import Queue
import threading
import traceback
import types

import InputOutput as IO
import CommandLine as CL

# Determines is a string is empty. A string is empty if 
#   1) it is Null, or
#   2) it contains no characters.
# if the strip flag is set, it will first strip leading and training
# white space to determine if a non-Null string is empty.
#
# args:
#   string: the string to test
#   strip:  the strip flag - set to True to strip whitespace before test
#
# returns: true if the string is empty, false otherwise 
def isEmptyString(string,strip=False):
    if (string == None):
        return true
    if (strip):
        return (len(string.strip()) == 0)
    else:
        return (len(string) == 0)

# Prints a message to the specified stream. The stream can be any writable
# file. The message has two parts, both of which are optional; a header and
# a body. The basic output is:
#    header:
#    message
# 
# args:
#   stream: the output stream for the message
#   header: (optional) the first line of the message
#   body:   the body of the message
def printMessage(stream,header=None,body=None):
    io = IO.InputOutput()
    io.setStream(stream)
    if header != None:
        io.writeln(data=header+":")
    if body != None:
        io.writeln(data=body)

# Formats the error response from the script runner.
# The error response is printed to standard error.
#
# args:
#   report: the message to report
def reportHTTPResponse(report):
    io = IO.InputOutput()
    io.setStream(sys.stderr)
    io.writeln(data=report)

# Converts a list to a dictionary. To list to be converted has
# the following format:
#     list = [key1, value1, key2, value2, ... , key-n, value-n]
# The resulting dictionary is
#     dict = {key1:value1, key2:vlaue2, ... , key-n:value-n]
#
# args:
#   list: the list to convert to a dictionary
# returns:
#   the converted list as a dictionary
#
# raises:
#   ValueError: if the list has an odd number of elements
def convListToDict(list):
    if len(list) % 2 != 0:
        raise CL.ArgError("Invalid input list " + str(list))
    retVal = [] 
    while len(list) > 0:
        temp = []
        key = list.pop(0)
        val = list.pop(0)
        temp.append(key)
        temp.append(val)
        retVal.append(temp)
    return dict(retVal)

def doWithinTime(target_function, description='complete the operation', 
        max_tries = 3, max_try_time = 10.0, args=(), kwargs={}):
    q = Queue.Queue()
    def threadFunc(q, target_function, args, kwargs):
        try:
            r = (True, target_function(*args, **kwargs))
        except:
            traceback.print_exc()
            r = (False, sys.exc_info()[1])
        q.put(r)
    exc = None
    for i in range(0, max_tries):
        t = threading.Thread(target=threadFunc, args=(q, target_function, args, kwargs))
        t.daemon = True
        t.start()
        try:
            r, val = q.get(True, max_try_time)
            if r:
                return val
            else:
                exc = val
                break
        except Queue.Empty, e:
            continue
    reason = exc is None and " within the expected time" or (": " + str(exc))
    raise StandardError("Failed to %s%s" % (description, reason))

