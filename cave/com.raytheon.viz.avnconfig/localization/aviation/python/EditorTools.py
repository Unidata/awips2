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
#       EditorTools.py
#       GFS1-NHD:A6834.0000-SCRIPT;19
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 19 (DELIVERED)
#         Created:  18-DEC-2008 17:31:05      OBERFIEL
#           Changed label on default button in the QC dialog.
#       
#       Revision 18 (DELIVERED)
#         Created:  01-OCT-2008 22:07:36      OBERFIEL
#           Simplified layout now that customization can be found in
#           the info.cfg files.
#       
#       Revision 17 (DELIVERED)
#         Created:  23-SEP-2008 08:44:54      GILMOREDM
#           Changed so that all TAFs are QC'd regardless of a user's
#           selection in the QC dialog
#       
#       Revision 16 (REVIEW)
#         Created:  19-SEP-2008 12:41:17      GILMOREDM
#           minor change to TafQC
#       
#       Revision 15 (INITIALIZE)
#         Created:  19-SEP-2008 12:20:46      GILMOREDM
#           Slight fix for TafQC
#       
#       Revision 14 (REVIEW)
#         Created:  16-SEP-2008 11:34:37      GILMOREDM
#           Refreshed changes where needed for TafQC
#       
#       Revision 13 (DELIVERED)
#         Created:  18-AUG-2008 12:49:28      OBERFIEL
#           Remove obsolete CM info
#       
#       Revision 12 (REVIEW)
#         Created:  15-MAY-2008 15:11:29      GILMOREDM
#           Configured QC dialog to handle individual site QC
#           configurations
#       
#       Revision 11 (DELIVERED)
#         Created:  27-FEB-2008 10:26:09      GILMOREDM
#           Added functionality to overcome a bug with the
#           Pmw.TimeCounter object
#       
#       Revision 10 (DELIVERED)
#         Created:  23-JAN-2007 12:36:15      OBERFIEL
#           Further dereference of the fcstr information was needed to
#           return forecaster ID.
#       
#       Revision 9 (DELIVERED)
#         Created:  09-SEP-2005 13:53:20      TROJAN
#           spr 7011
#       
#       Revision 8 (DELIVERED)
#         Created:  07-JUL-2005 12:27:35      TROJAN
#           spr 6911
#       
#       Revision 7 (DELIVERED)
#         Created:  07-MAY-2005 11:32:43      OBERFIEL
#           Added Item Header Block
#       
#       Revision 6 (DELIVERED)
#         Created:  04-APR-2005 15:51:05      TROJAN
#           spr 6775
#       
#       Revision 5 (DELIVERED)
#         Created:  24-JAN-2005 15:51:13      TROJAN
#           spr 6259
#       
#       Revision 4 (APPROVED)
#         Created:  01-OCT-2004 13:42:40      TROJAN
#           spr 6400
#       
#       Revision 3 (APPROVED)
#         Created:  09-JUL-2004 19:10:54      OBERFIEL
#           Replaced busy dialogs
#       
#       Revision 2 (APPROVED)
#         Created:  01-JUL-2004 14:59:21      OBERFIEL
#           Update
#       
#       Revision 1 (DELIVERED)
#         Created:  08-JAN-2004 21:29:52      PCMS
#           Initial version
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7407
#       	Action Date:       03-JAN-2009 09:26:51
#       	Relationship Type: In Response to
#       	Status:           TEST
#       	Title:             AvnFPS: Allow WFOs to update HDF5 climate files
#       
#
import logging, time
from Tkinter import *
import Pmw
import Avn, AvnLib, AvnParser, Busy, Globals

Help = {
    'title': 'Editor Help',
    'content': """
The text window key bindings are essentially the same as documented in Tcl/Tk
text widget manual. See also Brent B. Welch, Practical Programming in Tcl and 
Tk, second edition, pp 385-387). Note, however, that some of the key 
combination may be intercepted by the window manager.

Any-Key             Insert normal printing characters.
Control-u           Undo changes 
Control-r           Redo changes
Button-1            Sets the insert point, clear the selection, set focus.
Control-Button-1    Set the insert point without affecting the selection.
B1-Motion           Sweep out a selection from the insert point.
Double-Button-1     Select the word under the mouse.
Triple-Button-1     Select the line under the mouse.
Shift-Button-1      Adjust the end of selection closest to the mouse.
Shift-B1-Motion     Continue to adjust the selection.
Button-2            Paste the selection, or set the scrolling anchor.
B2-Motion           Scroll the window.
Key-Left or Control-b   Move the cursor left one character. Clear selection.
Shift-Left          Move the cursor and extend the selection.
Control-Left        Move the cursor by words. Clear the selection.
Control-Shift-Left  Move the cursor by words. Extend the selection.
Key-Right or Control-f  Right bindings are analogous to Left bindings.
Alt-b or Alt-f      Same as Control-Left, Control-Right.
Key-Up or Control-p Move the cursor up one line. Clear the selection.
Control-Up          Move the cursor by paragraph which are group of lines
                    separated by a blank line.
Control-Shift-Up    Move the cursor by paragraph. Extend selection.
Key-Down or Control-n   All Down bindings are analogous to Up bindings.
PageUp, PageDown    Move the cursor by one screen. Clear the selection.
Shift-PageUp, PageDown  Move the cursor by one screen. Extend the selection.
Home or Control-a   Move the cursor to line start. Clear the selection.
Shift-Home          Move the cursor to line start. Extend the selection.
End or Control-e    Move the cursor to line end. Clear the selection.
Shift-End           Move the cursor to line end. Extend the selection.
Control-Home        Move the cursor to the beginning of text. Clear the
                    selection.
Control-End         Move the cursor to the beginning of text. Extend the
                    selection.
Control-slash       Select everything in the text widget.
Control-backslash   Clear the selection.
Delete              Delete the selection, if any. Otherwise delete the
                    character to the right of the cursor.
BackSpace or Control-h  Delete the selection, if any. Otherwise delete the
                    character to the left of the cursor.
Control-d           Delete character to the right of the cursor.
Alt-d               Delete word to the right of the cursor.
Control-k           Delete from cursor to the end of the line. If you are
                    at the end of the line, delete the newline character.
Control-o           Insert a newline but do not advance the cursor.
Alt-Delete          Delete the word to the left of the cursor.
Alt-Backspace       
Control-t           Transpose the characters on either side of the cursor.
"""
}
_Logger = logging.getLogger(__name__)

#############################################################################
class SendDialog(Pmw.Dialog):
    def __init__(self, parent, **kw):
        Pmw.Dialog.__init__(self, parent.interior())
        self.title(Avn.Name + ' Send')
        self.withdraw()
        self.configure(buttons=('OK', 'Cancel'),
            defaultbutton='OK',
            command=self.__execute)
        self._time = Pmw.TimeCounter(self.interior(),
            labelpos='w',
            label_text='Transmit at:\nHH:MM:SS',
            min='00:00:00',
            max='23:59:59',
            value='00:00:00',
            )
        self._time.pack(padx=10, pady=5)
        self._slist = Pmw.ScrolledListBox(self.interior(),
            labelpos='n',
            label_text='Responsible for this forecast',
            )
        self._slist.pack()
        self.__load()
        self._position = 'first+%d+%d' % (parent.winfo_rootx()+30, \
            parent.winfo_rooty()+30)

    def __load(self):
        try:
            self._fcstdict = AvnParser.getForecasters()
            if not self._fcstdict:
                raise ValueError, 'Empty file'
            fcstnames = self._fcstdict.keys()
            fcstnames.sort()
            self._slist.setlist(fcstnames)
        except IOError:
            msg = 'Cannot open forecaster file'
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())
            raise SystemExit
        except (ValueError, IndexError):
            msg = 'Invalid entry in forecaster file'
            _Logger.exception(msg)
            Busy.showerror(msg, self.interior())
            raise SystemExit

    def __execute(self, result=None):
        if result == 'OK':
            now = time.time()
            self.time = self._getTime() + (now//86400.0)*86400.0
            if self.time < now - 43200:
                self.time += 86400
            if not (now - 600 < self.time < now + 10800):
                if not Busy.askokcancel('Are you sure?', self.interior()):
                    return
            try:
                fcstname = self._slist.getcurselection()[0]
                self.id = self._fcstdict[fcstname].get('id',0)
            except IndexError:
                Busy.showerror('Select name', self.interior())
        else:
            self.time, self.id = None, 0
        self.deactivate()

    def _getTime(self):
	# this solves a bug in the Pmw.TimeCounter object not returning correct 
	# time if values are updated in the entryfield only (no arrows are clicked)
	if self.thetime != self._time.getint():
	    inttime = self._time.getint()
	else:
	    thetime = [int(self._time.component(comp).get()) for comp in ['hourentry','minuteentry','secondentry']]
	    inttime = thetime[0]*3600 + thetime[1]*60 + thetime[2]
	return inttime

    def activate(self, kind, headertime, bbb):
        if bbb and bbb[0] != ' ':
            t = time.time()
        else:
            t = AvnLib.getXmitTime(kind, headertime)
        self._time.setvalue(time.strftime('%H:%M:00', time.gmtime(t)))
	self.thetime = self._time.getint() # used in self._getTime() method
        ix = list(self._slist.get()).index(Globals.Forecaster)
        self._slist.selection_set(ix)
        self._slist.see(ix)
        Busy.Manager.busy(None, self.component('hull'))
        Pmw.Dialog.activate(self, geometry=self._position)
        Busy.Manager.notbusy()
        return self.time, self.id

#############################################################################
class QCDialog(Pmw.Dialog):
    def __init__(self, parent, **kw):
        Pmw.Dialog.__init__(self, parent)
        self.withdraw()
        self.title(Avn.Name + ' QC')

        self.configure(buttons=('Apply', 'Close'),
            defaultbutton='Apply',
            command=self.__execute,
            )

        self.parent = parent
        self._tkProduct = StringVar()

        self.radiobuttons = Pmw.RadioSelect(self.interior(),
            buttontype='checkbutton',
            orient='vertical',
            labelpos='n',
            label_text='QC Options  ',
            hull_borderwidth=0,
            padx=5,
            pady=1,
            hull_relief='ridge',
            )
        
        self.KeysLabels = [('currentwx', 'Current Wx'), ('climate', 'Climate'), ('impact', 'Impact')]
        self.radiobuttons.pack(side='left', expand='no', padx=5)
        for item, label in self.KeysLabels:
            self.radiobuttons.add(item, text=label)
            self.radiobuttons.invoke(item)

        self.resizable(width='false', height='true')
        self._position = 'first+%d+%d' % (parent.winfo_rootx()+30, \
            parent.winfo_rooty()+30)

    def __execute(self, result):
        self.items = dict([(a[0],0) for a in self.KeysLabels])
        if result == 'Apply':
            for item in self.radiobuttons.getcurselection():
                self.items[item] = 1

        self.deactivate()

    def activate(self):
        Busy.Manager.busy(None, self.component('hull'))
        Pmw.Dialog.activate(self, geometry=self._position)
        Busy.Manager.notbusy()
        return self.items

###############################################################################
class TkIndex:
    # Used to convert offsets to Tk text indices
    def __init__(self, text):
        self.newlines = []
        self.length = len(text)
        i = 0
        while 1:
            try:
                k = text.index('\n', i)
                self.newlines.append((i, k))
                i = k+1
            except ValueError:
                break

    def ix(self, offset, length):
        if offset < 0 or length <= 0 or offset > self.length:
            raise ValueError, 'Invalid arguments %d %d' % (offset, length) 
        start, end = '1.0', None
        n = 1
        for i, k in self.newlines:
            if i <= offset <= k:
                start = '%d.%d' % (n, offset-i)
                break
            n += 1
        offset += length
        for i, k in self.newlines[n-1:]:
            if i <= offset <= k:
                end = '%d.%d' % (n, offset-i)
                break
            n += 1
        if end is None:
            end = 'end'
        return start, end

#############################################################################
def validateBBB(text):
    # validator used by Pmw.EntryField
    if not text:
        return Pmw.OK
    if len(text) < 3:
        return Pmw.PARTIAL
    elif len(text) > 3:
        return Pmw.ERROR
    try:
        bb = text[:2].upper()
        if not bb in ('AA', 'CC', 'RR'):
            return Pmw.ERROR
        if not text[2].isalpha():
            return Pmw.ERROR
        return Pmw.OK
    except:
        return Pmw.ERROR

#############################################################################
def validateHeaderTime(text):
    # validator used by Pmw.EntryField
    if len(text) < 6:
        return Pmw.PARTIAL
    try:
        day = int(text[:2])
        hour = int(text[2:4])
        minute = int(text[4:])
        if day < 1 or day > 31:
            return Pmw.ERROR
        if hour < 0 or hour > 23:
            return Pmw.ERROR
        if minute < 0 or minute > 59:
            return Pmw.ERROR
        return Pmw.OK
    except:
        return Pmw.ERROR

def xmitServerOK():
    # find server name
    for key in Globals.ServerStatus:
        if key.startswith('XMIT'):
            return time.time() - Globals.ServerStatus[key] < 60.0
    else:
        return False
