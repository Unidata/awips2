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

##
# This is a base file that is not intended to be overridden.
##
#
# SOFTWARE HISTORY
#
# Date          Ticket#   Engineer       Description
# ------------- --------- -------------- --------------------------
#                                        Initial creation
# 2018-04-18    DCS 19952  dfriedman     Added AWIPS ID query support

#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    01/16/2018      6804          tgurney        Remove setEditor calls
#    03/04/2019      7601          tgurney        Use exec() to call other
#                                                 scripts directly instead of
#                                                 going back to Java. + Cleanup
#    10/28/2019      7601          tgurney        Fix scriptPath global definition
#


from com.raytheon.viz.texteditor.scripting.runner import TextWsCommands

# These two variables are set from Java after importing this file into the
# interpreter's global scope
observer = None
scriptPath = None

cmds = TextWsCommands()

# symbolic names/aliases to better match AWIPS I script syntax
on = True
off = False
until = "until"

# provides a single method to initialize the 'cmds' object
# should be called at the start to each command implementation
def initCmds():
    cmds.setObserver(observer)
#
# implements a standard exit strategy for cancel script
def cancel():
    raise ScriptCancelled()


# implements the basic repeat command
def repeat(count,body=""):
    initCmds()
    if body == "":
        body = count
        count = -1
    if count > -1:
        for i in range(count):
            exec(body)
            cmds.doEvents()
            if cmds.cancelRequested():
                cancel()
    else:
        while True:
            exec(body)
            cmds.doEvents()
            if cmds.cancelRequested():
                cancel()


# turns accumulation on/off in the text editor window
def accum(flag):
    initCmds()
    if cmds.cancelRequested():
        cancel()
    try:
        cmds.setAccumulation(flag)
    except:
        raise


# clears the test work editor window
def clear():
    initCmds()
    if cmds.cancelRequested():
        cancel()
    try:
        cmds.clearTextDisplay()
    except:
        raise


#implements the wait command
def wait(opt="",time=""):
    initCmds()
    if cmds.cancelRequested():
        cancel()
    # need to do a little more validation...
    if (opt == "") and (time == ""):
        try:
            cmds.waitIndefinate()
        except:
            raise
    elif opt == until:
        if time == "":
            raise AttributeError("wait(until) requires minutes argument")
        try:
            cmds.waitUntilTime(time)
        except:
            raise
    else:
        try:
            cmds.waitForTime(opt)
        except:
            raise
    if cmds.cancelRequested():
        cancel()

# implements the load(pid) command
def load(pid):
    initCmds()
    if cmds.cancelRequested():
        cancel()
    cmds.loadTextProduct(pid.upper(), True)

# implements loadawips(pid)
def loadawips(aid):
    initCmds()
    if cmds.cancelRequested():
        cancel()
    cmds.loadTextProduct(aid.upper(), False)

# implements the readdb(pid,filename) command
def readdb(pid,filename):
    initCmds()
    if cmds.cancelRequested():
        cancel()
    cmds.saveProductToFile(pid.upper(), True, filename)

# implements the readdbawips(aid,filename) command
def readdbawips(aid,filename):
    initCmds()
    if cmds.cancelRequested():
        cancel()
    cmds.saveProductToFile(aid.upper(), False, filename)

# implements the writedb(pid,filename) command
def writedb(pid,filename):
    initCmds()
    if cmds.cancelRequested():
        cancel()
    cmds.readProductFromFile(pid.upper(), True, filename)

# implements the writedbawips(aid,filename) command
def writedbawips(pid,filename):
    initCmds()
    if cmds.cancelRequested():
        cancel()
    cmds.readProductFromFile(pid.upper(), False, filename)

def run(filename):
    initCmds()
    if cmds.cancelRequested():
        cancel()
    callingScriptPath = scriptPath
    calledScriptPath = cmds.resolveScriptPath(callingScriptPath, filename)
    setScriptPath(calledScriptPath)
    try:
        with open(calledScriptPath) as f:
            code = compile(f.read(), calledScriptPath, 'exec')
        exec(code)
    finally:
        setScriptPath(callingScriptPath)


def setScriptPath(path):
    global scriptPath
    scriptPath = path


class writer():
    def write(self,text):
        cmds = TextWsCommands()
        cmds.setObserver(observer)
        cmds.writeText(text)

sys.stdout = writer()

class errwriter():
    def write(self,text):
        cmds = TextWsCommands()
        cmds.setObserver(observer)
        cmds.writeError(text)

sys.stderr = errwriter()

class ScriptCancelled(Exception):
    def __init__(self,value='User cancelled the script',cause=None):
      self.value = value
      self.cause = cause
    def __str__(self):
        msg = 'ScriptCancelled: ' + repr(self.value)
        if self.cause is not None:
            msg += "\n caused by " + repr(self.cause)
        return msg
    def __repr__(self):
        return self.__str__()
