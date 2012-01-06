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
#       QueueViewer.py
#       GFS1-NHD:A7814.0000-SCRIPT;1.7
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.7 (DELIVERED)
#         Created:  20-FEB-2006 11:18:20      TROJAN
#           removed unnecessary Pmw options
#       
#       Revision 1.6 (DELIVERED)
#         Created:  06-JUL-2005 18:16:41      TROJAN
#           spr 6548
#       
#       Revision 1.5 (DELIVERED)
#         Created:  07-MAY-2005 11:37:09      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.4 (DELIVERED)
#         Created:  30-SEP-2004 20:53:43      TROJAN
#           stdr 873
#       
#       Revision 1.3 (APPROVED)
#         Created:  19-AUG-2004 20:53:04      OBERFIEL
#           Change code
#       
#       Revision 1.2 (APPROVED)
#         Created:  09-JUL-2004 19:10:50      OBERFIEL
#           Replaced busy dialogs
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:43:29      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
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
# QueueViewer.py
# Transmission log file viewer dialog
# Author: George Trojan, SAIC/MDL, August 2001
# last update: 02/20/06

import logging, os, time
from Tkinter import *
import Pmw
from Balloon import Balloon
import Avn, AvnDialog, Busy, Globals

_Help = {
    'title': 'Queue Viewer Help',
    'content': """
This dialog is used to manage transmission and to transmission log files.

The top area is to manage forecast files written by the forecast editor.
The 'Files' scrolled list window lists files in one of 'pending', 'sent'
and 'bad' directories. The time is when the file was written. The file
name is 
    xxx-CCCCNNNXXX-yymmddHHMM-BBB
where xxx is the forecaster number. The transmission program 
avnxmitserv uses NNN to determine the transmission window for regular 
forecasts.

The bottom area is used to view transmission log files.  There is one 
file for each day of the week. By default, log files for the current day 
are shown.

Buttons:
   Refresh:    refreshes both the directory list and log file windows.
   View:       allows to view selected transmission file(s)
   Remove:     deletes transmission files
   Retransmit: forces the transmission program to send selected files.
               If the file is in the 'bad' or 'sent' directory, it is 
               moved back to 'pending'. The transmission time (the last
           part of the file name) is updated to the current time.
   Help:       displays this window
"""
}

_Logger = logging.getLogger(__name__)

class Viewer(AvnDialog.Dialog):
    def __init__(self, parent, **kw):
        AvnDialog.Dialog.__init__(self, parent, **kw)
        self.title(Avn.Name + ' Transmission Queue')

        self.__createCmdBtnBox()
        self.createMessageBar(self.interior())

        pane = Pmw.PanedWidget(self.interior())
        pane.add('top', min=100)
        pane.add('bottom', min=100)

        self.dirbuttons = Pmw.RadioSelect(pane.pane('top'),
            buttontype='radiobutton',
            orient='vertical',
            labelpos='n',
            label_text='Directory',
            hull_borderwidth=2,
            hull_relief='ridge',
            command=self.__listDir,
            )
        self.dirbuttons.pack(side='left', expand='no', fill='x', padx=5)
        for dir in ['pending', 'sent', 'bad']:
            self.dirbuttons.add(dir)

        self.scrolledlist = Pmw.ScrolledListBox(pane.pane('top'),
            labelpos='n',
            label_text='Files',
            vscrollmode='static',
            listbox_selectmode='extended',
            )
        self.scrolledlist.pack(side='left', expand='yes', fill='both')
        
        self.daybuttons = Pmw.RadioSelect(pane.pane('bottom'),
            buttontype='radiobutton',
            orient='horizontal',
            hull_borderwidth=2,
            hull_relief='ridge', \
            command=self.__showLog,
            )
        self.daybuttons.pack(side='top', expand='yes', fill='x', padx=2)
        for day in ['Sunday', 'Monday', 'Tuesday', 'Wednesday', \
            'Thursday', 'Friday', 'Saturday']:
            self.daybuttons.add(day)
        self.scrolledtext = Pmw.ScrolledText(pane.pane('bottom'),
            labelpos='n',
            label_text='',
            borderframe=1,
            vscrollmode='static',
            text_state='disabled',
            text_wrap='none',
            )
        self.scrolledtext.pack(side='top', expand='yes', fill='both')
        pane.pack(side='top', expand='yes', fill='both')
        self.resizable(width='true', height='true')

        self.initialiseoptions(Viewer)

    def __createCmdBtnBox(self):
        cmdbtnbox = Pmw.ButtonBox(self.interior(),
            hull_relief='groove',
            hull_bd=2,
            )
        btn = cmdbtnbox.add('Close', command=self.close)
        Balloon().bind(btn, 'Closes this dialog')
        btn = cmdbtnbox.add('Refresh', command=self.__refresh)
        Balloon().bind(btn, 'Refresh display')
        btn = cmdbtnbox.add('View', command=self.__viewFiles)
        Balloon().bind(btn, 'View selected forecasts')
        btn = cmdbtnbox.add('Remove', command=self.__deleteFiles)
        Balloon().bind(btn, 'Delete selected forecasts')
        btn = cmdbtnbox.add('Retransmit', command=self.__send)
        Balloon().bind(btn, 'Resend selected forecasts')
        btn = cmdbtnbox.add('Help',
            command=Avn.curry(self.showHelp, _Help),
            )
        Balloon().bind(btn, 'Show help')
        cmdbtnbox.alignbuttons()
        cmdbtnbox.pack(side='top', expand='no', fill='x')

    def __createTextDialog(self):
        self.textdialog = Pmw.TextDialog(self._hull,
            buttons=('Close',),
            defaultbutton=0,
            scrolledtext_vscrollmode='static',
            )
        self.textdialog.transient(master=self._hull)
        self.textdialog.withdraw()
        AvnDialog.setGeometry(self, self.textdialog)

    def __deleteFiles(self):
        if not self.scrolledlist.getcurselection():
            Busy.showwarning('Select file(s) first', 
                self.interior())
            return
        dir = self.dirbuttons.getcurselection()
        for f in self.scrolledlist.getcurselection():
            try:
                fname = os.path.join('xmit', dir, f)
                Globals.DRC.rmFile(fname)
                _Logger.debug('Removed %s', fname)
            except OSError:
                msg = 'Cannot remove %s' % fname
                _Logger.exception(msg)
                Busy.showerror(msg, self.interior())
        self.__listDir()

    def __listDir(self, tag=None):
        dir = os.path.join('xmit', self.dirbuttons.getcurselection())
        try:
            files = Globals.DRC.getDir(dir)
            tmplist = [(f.split('-')[-1], f) for f in files]
            tmplist.sort()
            self.scrolledlist.setlist([x[1] for x in tmplist])
        except Exception:
            msg = 'Cannot list %s' % dir
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())

    def __refresh(self):
        dir = self.dirbuttons.getcurselection()
        if dir:
            self.dirbuttons.invoke(dir)
        else:
            self.dirbuttons.invoke('pending')
        day = self.daybuttons.getcurselection()
        if day:
            self.daybuttons.invoke(day)
        else:
            self.daybuttons.invoke(time.strftime('%A'))
        self.messagebar().message('userevent', 'Window refreshed')

    def __send(self):
        dir = self.dirbuttons.getcurselection()
        if dir == 'pending':
            Busy.showerror('Cannot do this', self.interior())
            return
        if not self.scrolledlist.getcurselection():
            Busy.showwarning('Select file(s) first', self.interior())
            return
        now = int(time.time())
        for f in self.scrolledlist.getcurselection():
            newname = os.path.join('xmit', 'pending', '%s%10d' % (f[:-10], now))
            fname = os.path.join('xmit', dir, f)
            try:
                Globals.DRC.mvFile(fname, newname)
                _Logger.info('Renamed %s to %s', fname, newname)
            except:
                msg = 'Cannot send %s' % f
                _Logger.exception(msg)
                Busy.showerror(msg, self.interior())

    def __showLog(self, tag=None):
        day = self.daybuttons.getcurselection()
        self.scrolledtext.configure(text_state='normal', label_text=day)
        self.scrolledtext.clear()
        fname = os.path.join('xmit', day[:3])
        try:
            data = Globals.DRC.getFile(fname)
            self.scrolledtext.setvalue(data)
        except (TclError, OSError, IOError):
            msg = 'Cannot get log file for %s' % day
            _Logger.exception(msg)
            self.messagebar().message('systemerror', msg)
        self.scrolledtext.configure(text_state='disabled')
        self.scrolledtext.see('end')

    def __viewFiles(self):
        if not self.scrolledlist.getcurselection():
            Busy.showwarning('Select file(s) first', self.interior())
            return
        if not hasattr(self, 'textdialog'):
            self.__createTextDialog()
        self.textdialog.clear()
        dir = self.dirbuttons.getcurselection()
        self.textdialog.configure(text_state='normal')
        for f in self.scrolledlist.getcurselection():
            try:
                path = os.path.join('xmit', dir, f)
                data = Globals.DRC.getFile(path)
                self.textdialog.appendtext(data)
            except Exception:
                msg = 'Cannot access %s' % f
                _Logger.exception(msg)
                self.messagebar().message('systemerror', msg)
        self.textdialog.configure(text_state='disabled')
        self.textdialog.show()

    def display(self):
        AvnDialog.Dialog.display(self)
        self.__refresh()
