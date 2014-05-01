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
from com.raytheon.viz.texteditor.scripting.runner import TextWsCommands
import os

cmds = TextWsCommands()
#
# symbolic names/aliases to better match AWIPS I script syntax
on = True
off = False
until = "until"
#
# provides a single method to initialize the 'cmds' object
# should be called at the start to each command implementation
def initCmds():
    cmds.setEditor(editor)
    cmds.setObserver(observer)
#
# implements a standard exit strategy for cancel script
def cancel():
    raise ScriptCancelled()

#
# implements the basic repeat command
def repeat(count,body=""):
    initCmds()
    if body == "":
        body = count
        count = -1
    if count > -1:
        for i in range(count):
            exec body
            cmds.doEvents()
            if cmds.cancelScript():
                cancel()
    else:
        while True:
            exec body
            cmds.doEvents()
            if cmds.cancelScript():
                cancel()

#
# turns accumulation on/off in the text editor window
def accum(flag):
    initCmds()
    if cmds.cancelScript():
        cancel()
    try:
        cmds.setAccumulation(flag)
    except:
        raise
#
# clears the test work editor window
def clear():
    initCmds()
    if cmds.cancelScript() :
        cancel()
    try:
        cmds.clearTextDisplay()
    except:
        raise

#
#implements the wait command
def wait(opt="",time=""):
    initCmds()
    if cmds.cancelScript() :
        cancel()
    # need to do a little more validation...
    if (opt == "") and (time == ""):
        try:
            cmds.waitIndefinate()
        except:
            raise
    elif opt == until:
        if time == "":
            raise AttributeError,"wait(unitl) requires minutes argument"
        try:
            cmds.waitUntilTime(time)
        except:
            raise
    else:
        try:
            cmds.waitForTime(opt)
        except:
            raise
    if cmds.isCanceled():
        cancel()
#
# implements the load(pid) command
def load(pid):
    initCmds()
    if cmds.cancelScript() :
        cancel()
    try:
        cmds.loadTextProduct(pid.upper())
    except:
        raise
# implements the readdb(pid,filename) command
def readdb(pid,filename):
    initCmds()
    if cmds.cancelScript() :
        cancel()
    try:
        cmds.saveProductToFile(pid.upper(),filename)
    except:
        raise

# implements the writedb(pid,filename) command
def writedb(pid,filename):
    initCmds()
    if cmds.cancelScript() :
        cancel()
    try:
        cmds.readProductFromFile(pid.upper(),filename)
    except:
        raise

def run(filename):
    initCmds()
    if cmds.cancelScript() :
        cancel()
    try:
        cmds.runLocalFile(filename)
    except:
        raise

class writer():
    def write(self,text):
        cmds = TextWsCommands()
#        initCmds()
        cmds.setEditor(editor)
        cmds.setObserver(observer)
        cmds.writeText(text)

sys.stdout = writer()

class errwriter():
    def write(self,text):
        cmds = TextWsCommands()
        cmds.setEditor(editor)
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