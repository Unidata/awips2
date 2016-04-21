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
#       AvnDialog.py
#       GFS1-NHD:A3788.0000-SCRIPT;12
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 12 (DELIVERED)
#         Created:  15-FEB-2006 14:34:40      TROJAN
#           fixes transient dialog behavior - spr 7091
#       
#       Revision 11 (DELIVERED)
#         Created:  06-JUL-2005 20:50:37      TROJAN
#           spr 6910
#       
#       Revision 10 (DELIVERED)
#         Created:  07-MAY-2005 11:54:10      OBERFIEL
#           Added Item Header Block
#       
#       Revision 9 (DELIVERED)
#         Created:  16-NOV-2004 20:08:50      PCMS
#           Restoring history
#       
#       Revision 8 (DELIVERED)
#         Created:  08-JAN-2004 21:39:48      PCMS
#           Updating for code cleanup
#       
#       Revision 7 (APPROVED)
#         Created:  05-NOV-2003 19:04:57      OBERFIEL
#           Initial version for 2.0
#       
#       Revision 6 (DELIVERED)
#         Created:  24-APR-2003 14:54:43      TROJAN
#           sprs 5055, 5056, 5057, 5070
#       
#       Revision 5 (DELIVERED)
#         Created:  10-APR-2003 15:26:54      TROJAN
#           spr 4997
#       
#       Revision 4 (DELIVERED)
#         Created:  13-MAY-2002 22:30:29      PCMS
#           Fixed placement of dialog boxes.
#       
#       Revision 3 (DELIVERED)
#         Created:  14-NOV-2001 21:34:50      PCMS
#           Fixed raising of balloon messages.
#       
#       Revision 2 (DELIVERED)
#         Created:  30-OCT-2001 19:11:50      PCMS
#           Adding capability to iconify dialog windows
#       
#       Revision 1 (DELIVERED)
#         Created:  02-OCT-2001 18:14:54      PCMS
#           Updating with gui changes
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7091
#       	Action Date:       23-FEB-2006 14:02:08
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Transient dialog do not remain on the top on RHEL4 desktop
#       
#
# AvnDialog.py
# A simple dialog class containing common methods
# Author: George Trojan, SAIC/MDL, September 2001
# last update: 02/15/06

from Tkinter import *
import Pmw
from HelpDialog import HelpDialog
from Balloon import Balloon
import MessageBar

def setGeometry(parent, widget):
    """forces dialog 30 pixels off parent's position"""
    x, y = parent.winfo_rootx(), parent.winfo_rooty()
    s_w, s_h = parent.winfo_screenwidth(), parent.winfo_screenheight()
    w_w, w_h = widget.winfo_reqwidth(), widget.winfo_reqheight()
    x, y = min(x+30, s_w-w_w), min(y+30, s_h-w_h)
    widget.wm_geometry('+%d+%d' % (x, y))

def PrintDialog(parent, printcmd):
    dialog = Pmw.PromptDialog(parent,
        master=parent,
        label_text='Print command',
        command=printcmd,
        entryfield_labelpos='n',
        entryfield_value='lpr',
        defaultbutton=0,
        buttons=('Ok', 'Close'),
        )
    dialog.withdraw()
    setGeometry(parent, dialog)
    return dialog

##############################################################################
class Dialog(Pmw.Dialog):
    def __init__(self, parent=None, **kw):
        if parent and int(parent.option_get('transientDialogs', '')):
            kw['master'] = parent
        else:
            kw['master'] = None
        Pmw.Dialog.__init__(self, parent, **kw)

        self.protocol('WM_DELETE_WINDOW', self.close)
        self.withdraw()

        self._parent = parent

        self.configure(buttons=[])
        self.bind('<FocusIn>', lambda event: Balloon().tkraise())
        # set window manager hints: does not work on Sawfish
        swidth = self.winfo_screenwidth()
        sheight = self.winfo_screenheight()
        self.component('hull').wm_maxsize(swidth-40, sheight-40)

    def close(self, event=None):
        self.withdraw()
        self.deactivate()

    def createMessageBar(self, parent, history=0):
        frame = Frame(parent,
            relief='sunken',
            )
        self._messagebar = MessageBar.MessageBar(frame,
            entry_relief='sunken',
            history=history,
            entry_bd=1,
            )
        self._messagebar.pack(side='left', expand='yes', fill='x')
        frame.pack(side='bottom', expand='no', fill='x')

    def display(self, above=None):
        if self.winfo_ismapped():
            return
        self.transient(master=self['master'])
        self.show()
        self.focus_set()
        Balloon().tkraise(self)

    def messagebar(self):
        return self._messagebar

    def setGeometry(self):
        if self._parent:
            setGeometry(self._parent, self)

    def showHelp(self, text):
        HelpDialog().display(text)
