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
#       EditDialog.py
#       GFS1-NHD:A3646.0000-SCRIPT;22
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 22 (DELIVERED)
#         Created:  03-FEB-2007 08:47:40      OBERFIEL
#           Verify Send Dialog was not updated on the new forecaster
#           transmit flag resource.
#       
#       Revision 21 (DELIVERED)
#         Created:  28-JUN-2006 14:08:14      TROJAN
#           spr 7188: fixed undo/redo functionality
#       
#       Revision 20 (DELIVERED)
#         Created:  15-FEB-2006 14:34:46      TROJAN
#           fixes transient dialog behavior - spr 7091
#       
#       Revision 19 (DELIVERED)
#         Created:  06-JUL-2005 20:50:38      TROJAN
#           spr 6910
#       
#       Revision 18 (DELIVERED)
#         Created:  07-MAY-2005 11:32:33      OBERFIEL
#           Added Item Header Block
#       
#       Revision 17 (DELIVERED)
#         Created:  04-APR-2005 15:51:04      TROJAN
#           spr 6775
#       
#       Revision 16 (APPROVED)
#         Created:  21-MAR-2005 16:53:04      TROJAN
#           spr 6740
#       
#       Revision 15 (APPROVED)
#         Created:  21-MAR-2005 16:46:26      TROJAN
#           spr 6740
#       
#       Revision 14 (DELIVERED)
#         Created:  30-SEP-2004 18:56:02      TROJAN
#           stdr 874
#       
#       Revision 13 (APPROVED)
#         Created:  01-JUL-2004 14:59:19      OBERFIEL
#           Update
#       
#       Revision 12 (DELIVERED)
#         Created:  08-JAN-2004 21:39:56      PCMS
#           Updating for code cleanup
#       
#       Revision 11 (APPROVED)
#         Created:  05-NOV-2003 19:07:51      OBERFIEL
#           Initial version for 2.0
#       
#       Revision 10 (DELIVERED)
#         Created:  24-APR-2003 14:54:48      TROJAN
#           sprs 5055, 5056, 5057, 5070
#       
#       Revision 9 (DELIVERED)
#         Created:  10-APR-2003 15:26:57      TROJAN
#           spr 4997
#       
#       Revision 8 (BUILD_RELEASE)
#         Created:  28-FEB-2003 12:35:56      TROJAN
#           spr 4754 4823
#       
#       Revision 7 (DELIVERED)
#         Created:  11-JUN-2002 19:11:05      PCMS
#           Prevented wrapping lines in error dialogs.  Enabled ability
#           to change cursor color.
#       
#       Revision 6 (DELIVERED)
#         Created:  29-MAY-2002 22:17:51      PCMS
#           Adding dialog to edit resource configuration file
#       
#       Revision 5 (DELIVERED)
#         Created:  21-MAY-2002 19:16:52      PCMS
#           updating for 5.2.2 s5
#       
#       Revision 4 (DELIVERED)
#         Created:  13-MAY-2002 22:30:28      PCMS
#           Fixed placement of dialog boxes.
#       
#       Revision 3 (DELIVERED)
#         Created:  30-OCT-2001 19:11:51      PCMS
#           Adding capability to iconify dialog windows
#       
#       Revision 2 (DELIVERED)
#         Created:  02-OCT-2001 17:48:23      PCMS
#           Updating for gui changes
#       
#       Revision 1 (DELIVERED)
#         Created:  20-AUG-2001 20:30:36      MOELLER
#           Initial version
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7242
#       	Action Date:       20-MAR-2007 09:51:12
#       	Relationship Type: Affected
#       	Status:           CLOSED
#       	Title:             AvnFPS: Privilege to transmit products should be user configurable
#       
#

import logging, os
from Tkinter import *
import Pmw
from Balloon import Balloon
import Avn, AvnDialog, Busy, EditorTools, SearchDialog

HelpEdit = {
    'title': 'Editor Help',
    'content': """
This is a generic text editor. Edit any text file via the file
selection dialog that is invoked by pressing the 'Open' button.

Menu items:
   File:
        Print invokes print dialog
   Edit:
        provides the usual editing functions (i.e. Cut, Copy, Paste,
        and Find/Replace). The menu can be also invoked by pressing
        right mouse button within the text window area.

Buttons:
   Clear: clears text window.
   Open:  invokes file selection dialog
   Save:  saves content of the text window
   Save as:  invokes file selection dialog

Toggles:
   Insert: toggles insert/overwrite mode
   Wrap:   toggles word wrap

The currently loaded (if any) file name is displayed in a label above the
text window.
"""
}

_Logger = logging.getLogger(__name__)

###############################################################################
class Editor(AvnDialog.Dialog):
    """A simple editor"""
    def __init__(self, parent, **kw):
        AvnDialog.Dialog.__init__(self, parent, **kw)
        self.title(Avn.Name + ' Text Editor')

        self.appInit(**kw)
        self.__getOptions()
        self.__createMenuBar()
        # from AvnDialog
        self.createMessageBar(self.interior())
        self.__createMainWindow()
        self.appFinish()
        self.initialiseoptions(Editor)

    def appInit(self):
        self._tkInsert = IntVar()
        self._tkWrap = StringVar()
        self._workdir = None

    def appFinish(self):
        self.configure(command=self.close)

    def __createMenuBar(self):
        self.menubar = Pmw.MenuBar(self.interior(),
            hull_relief='raised',
            hull_borderwidth=1,
            )
        self.menubar.pack(side='top', fill='x')

        self.menubar.addmenu('Help', 'Help actions', side='right')
        self.menubar.addmenuitem('Help', 'command',
            label='Key bindings',
            command=Avn.curry(self.showHelp, EditorTools.Help),
            )
        self.menubar.addmenuitem('Help', 'command',
            label='Usage',
            command=Avn.curry(self.showHelp, HelpEdit),
            )
        self.menubar.addmenu('File', 'Main actions', side='left')
        self.menubar.addmenuitem('File', 'command',
            'Invoke print dialog',
            label = 'Print',
            command=self.showPrintDialog,
            )
        self.menubar.addmenuitem('File', 'separator')
        self.menubar.addmenuitem('File', 'command',
            label = 'Close',
            command=self.close,
            )

        self.menubar.addmenu('Edit', 'Editing actions', side='left')
        self.menubar.addmenuitem('Edit', 'command',
            label = 'Cut',
            command=self.__cut,
            )
        self.menubar.addmenuitem('Edit', 'command',
            label = 'Copy',
            command=self.__copy,
            )
        self.menubar.addmenuitem('Edit', 'command',
            label = 'Paste',
            command=self.__paste,
            )
        self.menubar.addmenuitem('Edit', 'command',
            label = 'Find',
            command=self.__find,
            )
        self.menubar.addmenuitem('Edit', 'command',
            label = 'Undo',
            command=self.__undo,
            )
        self.menubar.addmenuitem('Edit', 'command',
            label = 'Redo',
            command=self.__redo,
            )

    def __createMainWindow(self):
        # shortcut buttons
        cmdbtnbox = Pmw.ButtonBox(self.interior())
        btn = cmdbtnbox.add('Clear', command=self.clear)
        Balloon().bind(btn, 'Clear text area')
        btn = cmdbtnbox.add('Open', command=self.open)
        Balloon().bind(btn, 'Open file')
        btn = cmdbtnbox.add('Save', command=self.save)
        Balloon().bind(btn, 'Save file with current name')
        btn = cmdbtnbox.add('Save As', command=self.saveAs)
        Balloon().bind(btn, 'Saves file')
        cmdbtnbox.alignbuttons()
        cmdbtnbox.pack(side='top', expand='no', fill='x')

        frame = Frame(self.interior())
        self.filelabel = Label(frame,
            relief='ridge',
            justify='left')
        self.filelabel.pack(side='left', expand='yes', fill='x')
        checkbutton = Checkbutton(frame,
            text='Insert',
            variable=self._tkInsert,
            )
        checkbutton.pack(side='left', padx=2)
        checkbutton = Checkbutton(frame,
            text='Wrap',
            onvalue='word',
            offvalue='none',
            variable=self._tkWrap,
            command=self.__toggleWrap,
            )
        checkbutton.pack(side='left', padx=2)
        frame.pack(side='top', expand='no', fill='x')

        # scrolled text
        self.scrolledtext = Pmw.ScrolledText(self.interior(),
            borderframe=1,
            hscrollmode='static',
            vscrollmode='static',
            text_setgrid=TRUE,
            text_undo=TRUE,
#           text_maxundo=16,
            )
        self.scrolledtext.pack(side='bottom', expand='yes', fill='both')
        self.scrolledtext.configure(text_wrap=self._tkWrap.get())
        # shortcut
        t = self.text = self.scrolledtext.component('text')
        t.bind('<KeyPress>', self.__keyPress)
        t.bind('<Button-3>', self.__popupMenu)
        t.bind('<Insert>', self.__toggleInsert)
        t.bind('<Control-u>', self.__undo)
        t.bind('<Control-r>', self.__redo)
        self._lastix = t.index('insert')

    def __createPopupMenu(self):
        self.popmenu = Menu(self._hull,
            tearoff=0,
            type='normal',
            )
        self.popmenu.add_command(label='Cut', command=self.__cut)
        self.popmenu.add_command(label='Copy', command=self.__copy)
        self.popmenu.add_command(label='Paste', command=self.__paste)
        self.popmenu.add_command(label='Undo', command=self.__undo)
        self.popmenu.add_command(label='Redo', command=self.__redo)

    def __createSearchDialog(self):
        self.searchdialog = SearchDialog.Dialog(self._hull, self.text)
        self.searchdialog.transient(master=self._hull)
        AvnDialog.setGeometry(self, self.searchdialog)

    def __getOptions(self):
        value = self.option_get('insert', '')   
        if value:
            self._tkInsert.set(int(value))
        value = self.option_get('wrap', '') 
        if not value:
            value = 'none'
        self._tkWrap.set(value)
        
#############################################################################
# editing methods
    def __copy(self):
        self.text.selection_own()
        try:
            selection = self.text.selection_get()
            self.text.clipboard_clear()
            self.text.clipboard_append(selection)
            return 1
        except:
            return 0

    def __cut(self):
        if not self.__copy():
            return
        start, end = self.text.tag_ranges('sel')
        self.text.delete(start, end)

    def __find(self, event=None):
        if not hasattr(self, 'searchdialog'):
            self.__createSearchDialog()
        self.searchdialog.show()
        self.searchdialog.focus_set()

    def __keyPress(self, e):
        if not self._tkInsert.get() and e.widget.get('insert') != '\n' \
            and e.char and e.keysym != 'Delete':
            e.widget.delete('insert')

    def __paste(self):
        try:
            self.text.insert('insert',
                self.text.selection_get(selection='CLIPBOARD'))
        except:
            pass

    def __popupMenu(self, e):
        if not hasattr(self, 'popmenu'):
            self.__createPopupMenu()
        self.popmenu.tk_popup(e.widget.winfo_rootx() + e.x,
            e.widget.winfo_rooty() + e.y)

    def __toggleInsert(self, e=None):
        if self._tkInsert.get():
            self._tkInsert.set(0)
        else:
            self._tkInsert.set(1)
        return 'break'

    def __toggleWrap(self):
        if not hasattr(self, 'scrolledtext'):
            return
        self.scrolledtext.configure(text_wrap=self._tkWrap.get())

    def __undo(self, e=None):
        try:
            self.text.edit_undo()
        except TclError:
            self.messagebar().message('userevent', 'Nothing to undo')

    def __redo(self, e=None):
        try:
            self.text.edit_redo()
        except TclError:
            self.messagebar().message('userevent', 'Nothing to redo')

##############################################################################
# menu methods
    def clear(self):
        self.scrolledtext.clear()
        self.filelabel.configure(text='')
        self.text.edit_modified(0)

    def close(self, event=None):
        if int(self.option_get('confirmClose', '')) != 0 and \
            self.text.edit_modified():
            if not Busy.askokcancel('Abort changes?', 
                self.interior()):
                return
        self.text.edit_modified(0)
        self.withdraw()

    def display(self, above=None):
        AvnDialog.Dialog.display(self, above)
        self.scrolledtext.focus_set()

    def open(self, event=None):
        filename = Busy.askopenfilename(self.interior(),
            initialdir=self._workdir, filetypes=[('all', '*')])
        try:
            self.getFile(filename)
        except IOError:
            msg = 'Cannot read %s' % filename
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())
            self.filelabel.configure(text='')

    def getFile(self, filename):
        if not filename:
            return
        if self.text.edit_modified() and not Busy.askokcancel( \
            'File not saved. Really want to continue?',
            self.interior()):
            return
        self.scrolledtext.clear()
        self.scrolledtext.importfile(filename)
        self.filelabel.configure(text=filename)
        self._lastix = self.text.index('insert')
        self.text.edit_reset()
        self.text.edit_modified(0)

    def finish(self, *filenames):
        try:
            self.getFile(*filenames)
        except IOError:
            pass

    def save(self, event=None):
        filename = self.filelabel.cget('text')
        if not filename:
            Busy.showerror('No file name', self.interior())
            return
        self.__save(filename)

    def saveAs(self, event=None):
        filename = Busy.asksaveasfilename(self.interior(),
            initialdir=self._workdir)
        if not filename:
            return
        self.__save(filename)

    def setworkdir(self, dir='.'):
        self._workdir=dir

    def showPrintDialog(self):
        if not hasattr(self, 'printdialog'):
            self.printdialog = AvnDialog.PrintDialog(self._hull,
                self.__print)
        self.printdialog.transient(self.interior())
        self.printdialog.show()
        self.printdialog.focus_set()

##############################################################################
# other methods
    def __print(self, result):
        if result is None or result == 'Close':
            self.printdialog.withdraw()
            return
        cmd = self.printdialog.get().strip()
        if not cmd:
            return
        content = self.scrolledtext.get().strip()
        (chldin, chldout) = os.popen4(cmd, -1)
        chldin.write(content)
        chldin.close()
        msg = chldout.read()
        if msg:
            _Logger.debug(msg)
            Busy.showinfo(msg, self.interior())
        else:
            msg = 'Sent to printer'
            _Logger.info(msg)
            self.messagebar().message('userevent', msg)
            
        self.printdialog.withdraw()
        return

    def __save(self, filename):
        try:
            file(filename, 'w').write('%s\n' % \
                self.scrolledtext.get().strip().expandtabs())
            self.text.edit_modified(0)
            msg = 'Saved file %s' % filename
            _Logger.info(msg)
            self.messagebar().message('userevent', msg)
        except IOError:
            msg = 'Cannot save file %s' % filename
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())
