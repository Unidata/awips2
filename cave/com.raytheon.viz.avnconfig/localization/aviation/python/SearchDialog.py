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
#       SearchDialog.py
#       GFS1-NHD:A3649.0000-SCRIPT;10
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 10 (DELIVERED)
#         Created:  01-OCT-2007 10:41:33      OBERFIEL
#           Surrounded tag_ranges() call with exception handling.
#       
#       Revision 9 (DELIVERED)
#         Created:  07-MAY-2005 11:38:02      OBERFIEL
#           Added Item Header Block
#       
#       Revision 8 (DELIVERED)
#         Created:  01-OCT-2004 13:42:40      TROJAN
#           spr 6400
#       
#       Revision 7 (APPROVED)
#         Created:  12-JUL-2004 12:05:38      OBERFIEL
#           Fixed missing BusyTkDialog messages
#       
#       Revision 6 (APPROVED)
#         Created:  01-JUL-2004 14:59:50      OBERFIEL
#           Update
#       
#       Revision 5 (DELIVERED)
#         Created:  05-NOV-2003 19:10:16      OBERFIEL
#           Initial version for 2.0
#       
#       Revision 4 (DELIVERED)
#         Created:  24-APR-2003 14:54:51      TROJAN
#           sprs 5055, 5056, 5057, 5070
#       
#       Revision 3 (DELIVERED)
#         Created:  30-OCT-2001 20:15:25      PCMS
#           Correcting heirarchy of dialog boxes.
#       
#       Revision 2 (DELIVERED)
#         Created:  02-OCT-2001 17:48:49      PCMS
#           Updating for gui changes
#       
#       Revision 1 (DELIVERED)
#         Created:  20-AUG-2001 20:32:00      MOELLER
#           Initial version
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7337
#       	Action Date:       19-MAR-2008 08:04:02
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Fixes from ATAN 893 flight category testing
#       
#
from Tkinter import *
import Pmw
import Busy

class Dialog(Pmw.Dialog):
	"""For use by EditDialog and its derivatives"""
	def __init__(self, parent, text, **kw):
		self._replacepattern = '?'
                self._history = []
		self._text = text
		optiondefs = ()
        	self.defineoptions(kw, optiondefs)
		Pmw.Dialog.__init__(self, parent)
		self.withdraw()

		self.configure(buttons=('Close',), defaultbutton=0)

		interior = self.interior()

        	self.__findentry = Pmw.EntryField(interior, labelpos='w',
			label_text='Find what:', validate=None)
        	self.__replaceentry = Pmw.EntryField(interior, labelpos='w',
			label_text='Replace by:', validate=None)
		entries = (self.__findentry, self.__replaceentry)
		for entry in entries:
			entry.pack(side='top', fill='x', expand='no', 
				padx=5, pady=5)
		Pmw.alignlabels(entries)

		self.__buttonbox = Pmw.ButtonBox(interior, padx=0, pady=0)
		self.__buttonbox.add('Find', command=self.__find)
		self.__buttonbox.add('Replace', command=self.__replace)
		self.__buttonbox.add('Replace All', command=self.__replaceAll)
		self.__buttonbox.add('Undo', command=self.__undo)
		self.__buttonbox.pack(side=TOP, expand=NO, fill=X)
		self.__buttonbox.alignbuttons()

		self.initialiseoptions(Dialog)

	def __find(self, quiet=0):
		self._searchpattern = self.__findentry.get().rstrip().upper()
		if not self._searchpattern:
			return 0
		self._text.selection_clear()
		tkLength = IntVar()
		last_pos = self._text.index('insert')
		next_pos = self._text.search(self._searchpattern, last_pos,
			nocase=1, count=tkLength) 
		if not next_pos:
			if not quiet:
				Busy.showinfo('Pattern not found',
					self.interior())
			return 0
		self._text.see(next_pos)
		len = tkLength.get()
		self._text.tag_add(SEL, next_pos, '%s + %d char' % \
			(next_pos, len))
		self._text.mark_set('insert', '%s + %d char' % (next_pos, len))
		return 1

	def __replace(self):
		pattern = self.__replaceentry.get().rstrip().upper()
		if not pattern and self._replacepattern:
			if not Busy.askokcancel('Replace with empty string?',
				self.interior()):
				return -1
		self._replacepattern = pattern
		if not self.__find(1):
			return 0
		# prevent infinite loop
		if self._replacepattern == self._searchpattern:
			return 0
		try:
			(start, end) = self._text.tag_ranges(SEL)
		except ValueError:
			return 0
		
		self._text.delete(start, end)
		repend = '%s + %d char' % (start, len(self._replacepattern))
		if self._replacepattern:
			self._text.insert(start, self._replacepattern) 
			self._text.tag_add(SEL, start, repend)
		self._history.append((start, repend, self._searchpattern))
		ll = len(self._history)
		if ll > 16:
			self._history = self._history[-16:]
		return 1

	def __replaceAll(self):
		n = 0
		while 1:
			rc = self.__replace()
			if rc == -1:
				return
			elif rc == 0:
				break
			n += 1
		if n == 0:
			Busy.showwarning('Replace failed', self.interior())
		else:
			Busy.showinfo('Replaced %d occurences' % n,
				self.interior())

	def __undo(self):
		ll = len(self._history)
		if ll == 0:
			Busy.showwarning('Nothing to undo', self.interior())
			return
		(start, end, pattern) = self._history[-1]
		self._text.delete(start, end)
		self._text.insert(start, pattern)
		del self._history[-1]

	def setText(self, text):
		self._text = text
