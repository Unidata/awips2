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
#    Name:
#       Busy.py
#       GFS1-NHD:A7883.0000-SCRIPT;1.4
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.4 (DELIVERED)
#         Created:  07-MAY-2005 11:31:22      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.3 (DELIVERED)
#         Created:  02-APR-2005 17:02:16      TROJAN
#           spr 6763
#       
#       Revision 1.2 (DELIVERED)
#         Created:  01-OCT-2004 13:42:39      TROJAN
#           spr 6400
#       
#       Revision 1.1 (APPROVED)
#         Created:  09-JUL-2004 19:19:13      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_6832
#       	Action Date:       07-JUN-2005 13:13:53
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Add PVCS doc blocks
#       
#
# Busy.py
# Dialog convenience functions
# based on code by Fredrich Lundh 
# Author: George Trojan, GSC/MDL, March 2001
# last update: 03/28/05

from Tkinter import TclError
import tkFileDialog
import tkCommonDialog

# icons
ERROR = "error"
INFO = "info"
QUESTION = "question"
WARNING = "warning"

# types
ABORTRETRYIGNORE = "abortretryignore"
OK = "ok"
OKCANCEL = "okcancel"
RETRYCANCEL = "retrycancel"
YESNO = "yesno"
YESNOCANCEL = "yesnocancel"

# replies
ABORT = "abort"
RETRY = "retry"
IGNORE = "ignore"
OK = "ok"
CANCEL = "cancel"
YES = "yes"
NO = "no"

# message dialog class
class Message(tkCommonDialog.Dialog):
    'A message box'
    command = 'tk_messageBox'

# convenience stuff
def _show(parent, message=None, icon=None, type=None, title=None, **options):
    options['parent'] = parent
    if icon:
        options['icon'] = icon
    if type:
        options['type'] = type
    if title:
        options['title'] = title
    if message:
        options['message'] = message
    return Message(**options).show()

Manager = None

###############################################################################
class BusyManager:
    def __init__(self, widget):
        self.toplevel = widget.winfo_toplevel()
        self.widgets = {}

    def busy(self, widget=None, except_widget=None):
        # attach busy cursor to toplevel, plus all windows
        # that define their own cursor.
        if widget is None:
            w = self.toplevel # myself
        else:
            w = widget
        if w == except_widget:
            return
        if not self.widgets.has_key(str(w)):
            try:
                # attach cursor to this widget
                cursor = w.cget("cursor")
                if cursor != "watch":
                    self.widgets[str(w)] = (w, cursor)
                    w.config(cursor="watch")
            except TclError:
                pass
        for w in w.children.values():
            self.busy(w, except_widget)

    def notbusy(self):
        # restore cursors
        for w, cursor in self.widgets.values():
            try:
                w.config(cursor=cursor)
            except TclError:
                pass
        self.widgets = {}

def instantiate(master):
    global Manager
    Manager = BusyManager(master)

###############################################################################
def showinfo(message, parent, **options):
    "Show an info message"
    Manager.busy()
    s = _show(parent, message, INFO, OK, **options)
    Manager.notbusy()
    return s

def showwarning(message, parent, **options):
    "Show a warning message"
    Manager.busy()
    s = _show(parent, message, WARNING, OK, **options)
    Manager.notbusy()
    return s

def showerror(message, parent, **options):
    "Show an error message"
    Manager.busy()
    s = _show(parent, message, ERROR, OK, **options)
    Manager.notbusy()
    return s

def askyesno(message, parent, **options):
    "Ask a question; return true if the answer is yes"
    Manager.busy()
    if not 'default' in options:
        options['default'] = 'no'
    s = _show(parent, message, QUESTION, YESNO, **options)
    Manager.notbusy()
    return s == YES

def askokcancel(message, parent, **options):
    "Ask if operation should proceed; return true if the answer is ok"
    Manager.busy()
    if not 'default' in options:
        options['default'] = 'cancel'
    s = _show(parent, message, QUESTION, OKCANCEL, **options)
    Manager.notbusy()
    return s == OK

def askopenfilename(parent, **options):
    "Ask for a filename to open"
    Manager.busy()
    options['parent'] = parent
    s = tkFileDialog.askopenfilename(**options)
    Manager.notbusy()
    return s

def asksaveasfilename(parent, **options):
    "Ask for a filename to save as"
    Manager.busy()
    options['parent'] = parent
    s = tkFileDialog.asksaveasfilename(**options)
    Manager.notbusy()
    return s
