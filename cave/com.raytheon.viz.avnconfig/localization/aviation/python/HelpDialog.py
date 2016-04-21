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
#       HelpDialog.py
#       GFS1-NHD:A3648.0000-SCRIPT;8
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 8 (DELIVERED)
#         Created:  06-JUL-2005 20:50:38      TROJAN
#           spr 6910
#       
#       Revision 7 (DELIVERED)
#         Created:  07-MAY-2005 11:34:21      OBERFIEL
#           Added Item Header Block
#       
#       Revision 6 (DELIVERED)
#         Created:  01-JUL-2004 14:59:30      OBERFIEL
#           Update
#       
#       Revision 5 (DELIVERED)
#         Created:  05-NOV-2003 19:09:10      OBERFIEL
#           Initial version for 2.0
#       
#       Revision 4 (DELIVERED)
#         Created:  13-MAY-2002 22:30:26      PCMS
#           Fixed placement of dialog boxes.
#       
#       Revision 3 (DELIVERED)
#         Created:  30-OCT-2001 19:11:57      PCMS
#           Adding capability to iconify dialog windows
#       
#       Revision 2 (DELIVERED)
#         Created:  02-OCT-2001 17:49:04      PCMS
#           Updating for gui changes
#       
#       Revision 1 (DELIVERED)
#         Created:  20-AUG-2001 20:31:33      MOELLER
#           Initial version
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_6910
#       	Action Date:       09-AUG-2005 14:09:26
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: code cleanup
#       
#
# HelpDialog.py
# Generic help dialog
# Aviation setup editor 
# Author: George Trojan, GSC/MDL, March 2001
# last update: 05/25/05

from Tkinter import *
import Pmw
import Avn

class _HelpDialog(Pmw.TextDialog):
    def __init__(self, parent=None, **kw):
        optiondefs = (
            ('scrolledtext_labelpos',  'n', None),
        )
        self.defineoptions(kw, optiondefs)
        Pmw.TextDialog.__init__(self, parent)
        self.title(Avn.Name + ' Help')
        self.withdraw()
        
        self.configure(buttons=('Close',),
            defaultbutton=0,
            scrolledtext_vscrollmode='static',
            text_setgrid=1,
            text_wrap='none',
        )

        self.initialiseoptions(_HelpDialog)
        if parent is not None:
            x = parent.winfo_rootx() + 30
            y = parent.winfo_rooty() + 30
            self.wm_geometry('+%d+%d' % (x, y))

    def display(self, helpdict):
        self.configure(text_state='normal')
        self.clear()
        self.insert(END, helpdict['content'])
        self.configure(label_text=helpdict['title'], text_state='disabled')
        if self.winfo_ismapped():
            self.tkraise()
        else:
            self.show()
            self.focus_set()

_instance = None
def HelpDialog(parent=None, **kw):
    global _instance
    if not _instance:
        _instance = _HelpDialog(parent, **kw)
    elif kw:
        _instance.configure(**kw)
    return _instance

