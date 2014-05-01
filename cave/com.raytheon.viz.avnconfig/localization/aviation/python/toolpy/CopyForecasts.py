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
#       CopyForecasts.py
#       GFS1-NHD:A6830.0000-SCRIPT;6
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 6 (DELIVERED)
#         Created:  26-MAR-2009 20:25:24      OBERFIEL
#           Simplified keys for TAF dictionary.  Eliminated obsolete
#           argument to updateTafs function.
#       
#       Revision 5 (DELIVERED)
#         Created:  09-SEP-2005 14:04:17      TROJAN
#           spr 7011
#       
#       Revision 4 (DELIVERED)
#         Created:  04-AUG-2005 15:27:02      TROJAN
#           spr 6962, 6963
#       
#       Revision 3 (DELIVERED)
#         Created:  01-OCT-2004 14:36:20      TROJAN
#           stdr 862
#       
#       Revision 2 (APPROVED)
#         Created:  01-JUL-2004 15:18:21      OBERFIEL
#           Update
#       
#       Revision 1 (DELIVERED)
#         Created:  08-JAN-2004 21:22:40      PCMS
#           Initial version
#
#    Change Document History:
#       1:
#           Change Document:   GFS1-NHD_SPR_7409
#           Action Date:       15-AUG-2009 20:19:42
#           Relationship Type: In Response to
#           Status:           TEST
#           Title:             AvnFPS: TAF No Significant Weather (NSW) not QC'd correctly
#       
#
# Purpose:
#
#    Allows for interactive copy of forecasts in a collective

from Tkinter import *
import Pmw

def _getIds(taf):
    n = taf.find('\n')+1
    return taf[n:n+4]

###############################################################################
# private area
###############################################################################
class Gui():
    def __init__(self, items, fcsts):
        root = Tk()
        self.root = root
        self.items = items
        self.fcsts = fcsts

        self.root.title('AvnFPS Tool: CopyForecasts')
        body = Frame(root)
        frame = Frame(body)
        self.fromlist = Pmw.ScrolledListBox(frame,
                                            labelpos='n',
                                            label_text='Source',
                                            listbox_width=5,
                                            listbox_selectmode='browse',
                                            listbox_exportselection=0,
                                            items=items)
        self.fromlist.pack(side='left', expand='yes', fill='y', padx=5)
        btn = Button(frame,
                     text='Copy',
                     command=self.copy)
        btn.pack(side='left', expand='no', padx=5)
        self.tolist = Pmw.ScrolledListBox(frame,
                                          labelpos='n',
                                          label_text=' Destn ',
                                          listbox_width=5,
                                          listbox_selectmode='extended',
                                          listbox_exportselection=0,
                                          items=items)
        self.tolist.pack(side='left', expand='yes', fill='y', padx=5)
        frame.pack(side='top', expand='yes', fill='both')
        bbox = Pmw.ButtonBox(body)
        bbox.add('OK', command=self.ok)
        bbox.add('Cancel', command=self.cancel)
        bbox.alignbuttons()
        bbox.pack(side='top')
        self.messagebar = Pmw.MessageBar(body,
            entry_relief='sunken',
            entry_bd=1,
            labelpos=None,
            )
        self.messagebar.pack(side='bottom', expand='yes', fill='x')
        body.pack(padx=5, pady=5, expand='y', fill='both')
        root.grab_set()
        root.protocol("WM_DELETE_WINDOW", self.cancel)
        if root is not None:
            root.geometry("+%d+%d" % (root.winfo_rootx()+50,
                root.winfo_rooty()+50))
        root.focus_set()
        root.wait_window(root)

        root.mainloop()

    def copy(self):
        try:
            srcid = self.fromlist.getcurselection()[0]
        except IndexError:
            #Busy.showerror('Select source', self)
            return
        destlist = self.tolist.getcurselection()
        if not destlist:
            #Busy.showerror('Select destination', self)
            return
        for id in destlist:
            self.fcsts[id] = self.fcsts[srcid].replace(srcid, id)
        self.messagebar.message('userevent', 'Copy completed')
        self.messagebar.component('entry').configure(state='normal', 
            background='green')

    def ok(self, event=None):
        if self.fcsts is not None:
            for item in self.items:
                self.fcsts[item] = self.fcsts[item] + '=\n'
        self.root.withdraw()
        self.root.update_idletasks()
        if self.root is not None:
            self.root.focus_set()
            self.root.destroy()

    def cancel(self, event=None):
        self.fcsts = None
        self.ok()

###############################################################################
def updateTafs(bbb, fcsts):
    # master is the parent widget, needed to create GUI
    # fcsts is a dictionary of forecasts displayed in the editor window
    # the function returns dictionary of modified forecasts
    # may raise AvnError
    items = [_getIds(f) for f in fcsts]    
    return Gui(items, fcsts).fcsts
