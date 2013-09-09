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
# ----------------------------------------------------------------------------
# This software is in the public domain, furnished "as is", without technical
# support, and with no warranty, express or implied, as to its usefulness for
# any purpose.
#
# StartUpDialog.py
# Class for displaying GFE startup dialog and
#   retrying the Server when there has been a problem
#
# Author: romberg
# ----------------------------------------------------------------------------

import Tkinter, tkSimpleDialog

class Dialog(tkSimpleDialog.Dialog):
    def __init__(self, parent, title = None, wait=1, xoffset=50, yoffset=50,
                 **kw):
        Tkinter.Toplevel.__init__(self, parent, **kw)
        self.transient(parent)

        if title:
            self.title(title)

        self.parent = parent

        if self.parent is not None:
            self.geometry("+%d+%d" % (parent.winfo_rootx()+xoffset,
                                      parent.winfo_rooty()+yoffset))
        else:
            self.geometry("+0+0")

        self.result = None
        body = Tkinter.Frame(self)
        self.initial_focus = self.body(body)
        body.pack(expand=Tkinter.YES, fill=Tkinter.BOTH)
        self.buttonbox()
        if wait:
            self.grab_set()

        if not self.initial_focus:
            self.initial_focus = self

        self.protocol("WM_DELETE_WINDOW", self.cancel)

        self.initial_focus.focus_set()

        self.ctorHook()

        if wait:
            self.wait_window(self)
        else:
            self.update_idletasks()
    def ctorHook(self):
        pass

class IFPDialog(Dialog):
    def __init__(self, parent, title = None, modal=1, xoffset=50, yoffset=50,
                 constructionHook = None, **kw):
        self._chook = constructionHook
        Dialog.__init__(self, parent, title, modal, xoffset,
                                      yoffset, **kw)

    def ctorHook(self):
        if self._chook is not None:
            self._chook()

