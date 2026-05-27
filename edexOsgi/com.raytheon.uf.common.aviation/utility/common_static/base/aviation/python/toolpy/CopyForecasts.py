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

##
# This is a base file that is not intended to be overridden.
##

import tkinter as tkinter

def _getIds(taf):
    n = taf.find('\n')+1
    return taf[n:n+4]

###############################################################################
# private area
###############################################################################
class Gui():
    def __init__(self, items, fcsts):
        self.root = tkinter.Tk()
        self.items = items
        self.fcsts = fcsts

        self.root.title('AvnFPS Tool: CopyForecasts')
        body = tkinter.Frame(self.root)
        frame = tkinter.Frame(body)
        
        from_frame = tkinter.LabelFrame(frame, labelanchor=tkinter.N, text='Source', relief=tkinter.FLAT)
        vert_scroll_from = tkinter.Scrollbar(from_frame, orient=tkinter.VERTICAL)
        self._from_list = tkinter.Listbox(from_frame, 
                                          width=5, 
                                          selectmode=tkinter.BROWSE, 
                                          exportselection=0,
                                          yscrollcommand=vert_scroll_from.set,)
        self._from_list.insert(0, *self.items)
        vert_scroll_from['command'] = self._from_list.yview
        vert_scroll_from.pack(side=tkinter.RIGHT, fill=tkinter.Y)
        self._from_list.pack(side=tkinter.TOP, expand=tkinter.YES, fill=tkinter.Y, padx=5)
        from_frame.pack(side=tkinter.LEFT, expand=tkinter.YES, fill=tkinter.Y)
        
        btn = tkinter.Button(frame, text='Copy', command=self.copy)
        btn.pack(side='left', expand='no', padx=5)
        
        to_frame = tkinter.LabelFrame(frame, labelanchor=tkinter.N, text=' Destn ', relief=tkinter.FLAT)
        vert_scroll_to = tkinter.Scrollbar(to_frame, orient=tkinter.VERTICAL)
        self._to_list = tkinter.Listbox(to_frame, 
                                        width=5, 
                                        selectmode=tkinter.EXTENDED, 
                                        exportselection=0,
                                        yscrollcommand=vert_scroll_to.set,)
        self._to_list.insert(0, *self.items)
        vert_scroll_to['command'] = self._to_list.yview
        vert_scroll_to.pack(side=tkinter.RIGHT, fill=tkinter.Y)
        self._to_list.pack(side=tkinter.TOP, expand=tkinter.YES, fill=tkinter.Y, padx=5)
        to_frame.pack(side=tkinter.RIGHT, expand=tkinter.YES, fill=tkinter.Y)
        
        frame.pack(side='top', expand='yes', fill='both')
        
        bbox = tkinter.Frame(body)
        ok_btn = tkinter.Button(bbox, text='OK', command=self.ok)
        ok_btn.pack()
        ok_btn.grid(column=0, row=0, ipadx=10, padx=5)
        cancel_btn = tkinter.Button(bbox, text='Cancel', command=self.cancel)
        cancel_btn.pack()
        cancel_btn.grid(column=1, row=0, padx=5)
        bbox.pack(pady=5)
        
        self.messagebar = tkinter.Label(body, 
                                        relief=tkinter.SUNKEN,
                                        borderwidth=1,
                                        state=tkinter.DISABLED,)
        self.messagebar.pack(side=tkinter.BOTTOM, expand=tkinter.YES, fill=tkinter.X)
        
        body.pack(padx=5, pady=5, expand=tkinter.Y, fill=tkinter.BOTH)
        self.root.grab_set()
        self.root.protocol("WM_DELETE_WINDOW", self.cancel)
        if self.root is not None:
            self.root.geometry("+%d+%d" % (self.root.winfo_rootx()+50,
                self.root.winfo_rooty()+50))
        self.root.focus_set()
        self.root.wait_window(self.root)

        self.root.mainloop()

    def copy(self):
        try:
            srcid = self._from_list.get(self._from_list.curselection()[0])
        except IndexError:
            return
        destlist = [self._to_list.get(i) for i in self._to_list.curselection()]
        if not destlist:
            return
        for d in destlist:
            self.fcsts[d] = self.fcsts[srcid].replace(srcid, d)
        reset_color = self.messagebar.cget('background')
        self.messagebar.configure(text='Copy completed', 
                                  background='green', 
                                  state=tkinter.NORMAL)
        self.root.after(5000, 
                        lambda: self.messagebar.configure(text='', 
                                                          background=reset_color, 
                                                          state=tkinter.DISABLED))

    def ok(self):
        if self.fcsts is not None:
            for item in self.items:
                self.fcsts[item] = self.fcsts[item] + '=\n'
        self.root.withdraw()
        self.root.update_idletasks()
        if self.root is not None:
            self.root.focus_set()
            self.root.destroy()

    def cancel(self):
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
