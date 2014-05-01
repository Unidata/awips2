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
#       FontChooser.py
#       GFS1-NHD:A4935.0000-SCRIPT;7
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 7 (DELIVERED)
#         Created:  18-MAY-2007 11:10:59      OBERFIEL
#           Sync'd up code between 3.4 and 3.5 worksets since snapshot
#       
#       Revision 6 (DELIVERED)
#         Created:  18-APR-2007 14:12:46      OBERFIEL
#           Small change to allow users to better see fixed width
#           fonts.
#       
#       Revision 5 (DELIVERED)
#         Created:  07-MAY-2005 11:33:27      OBERFIEL
#           Added Item Header Block
#       
#       Revision 4 (DELIVERED)
#         Created:  09-JUL-2004 19:11:28      OBERFIEL
#           Replaced busy dialogs
#       
#       Revision 3 (APPROVED)
#         Created:  01-JUL-2004 14:59:22      OBERFIEL
#           Update
#       
#       Revision 2 (DELIVERED)
#         Created:  05-NOV-2003 19:08:46      OBERFIEL
#           Initial version for 2.0
#       
#       Revision 1 (DELIVERED)
#         Created:  29-MAY-2002 22:14:28      PCMS
#           Initial version
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7300
#       	Action Date:       20-JUL-2007 10:06:32
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Tweb Product truncated in collective messages
#       
#
import os
from Tkinter import *
import Pmw

class Chooser(Toplevel):
	def __init__(self, parent=None, title=None, font='fixed'):
		Toplevel.__init__(self, parent)
		self.transient(parent)
		self.parent = parent
		self.title(title)
		self.tkShort = IntVar()
		self.tkShort.set(1)
		self.result = font

		body = Frame(self)
		self.initial_focus = self.body(body) 
		body.pack(padx=5, pady=5, expand='yes', fill='both')

		box = Frame(self)
		w = Button(box, text='OK', width=10, command=self.ok)
		w.pack(side='left', padx=5, pady=5)
		w = Button(box, text='Cancel', width=10, command=self.cancel)
		w.pack(side='right', padx=5, pady=5)
		self.bind('<Escape>', self.cancel)
		box.pack(side='bottom', expand='no', fill='x')

	        self.grab_set()
		if not self.initial_focus:
			self.initial_focus = self
		self.protocol("WM_DELETE_WINDOW", self.cancel)

		if self.parent is not None:
			self.geometry("+%d+%d" % (parent.winfo_rootx()+50,
				parent.winfo_rooty()+50))

		self.initial_focus.focus_set()

		self.wait_window(self)

	def destroy(self):
		self.initial_focus = None
		Toplevel.destroy(self)

	def ok(self, event=None):
		self.withdraw()
		self.update_idletasks()

		if self.parent is not None:
			self.parent.focus_set()
			self.destroy()

	def cancel(self, event=None):
		# put focus back to the parent window
               	self.result = None
		if self.parent is not None:
			self.parent.focus_set()
			self.destroy()

	def body(self, master):
		f = Frame(master)
		self.label = Label(f, text='ABCDEFGHIJ\n0123456789')
		self.label.pack(side='top', expand='no', fill='x')
		
		self.scrolledlist = Pmw.ScrolledListBox(f, \
			vscrollmode='static', \
			selectioncommand=self.__setFont, \
			listbox_width=80, \
			)
		self.scrolledlist.pack(side='top', expand='yes', fill='both')

		frame = Frame(f)
		self.entry = Pmw.EntryField(frame, \
			labelpos='w', \
			label_text='Search pattern', \
			validate=None, \
			command=self.__listFonts, \
			)
		self.entry.pack(side='left', expand='yes', fill='x')
		toggle = Checkbutton(frame, \
			text='Short Names', \
			command=self.__listFonts, \
			variable=self.tkShort, \
			)
		toggle.pack(side='right')
		frame.pack(side='bottom', expand='yes', fill='x')

		f.pack(side='top', expand='yes', fill='both')

		self.__listFonts()

	def __listFonts(self):
		cmd = 'xlsfonts'
		pattern = self.entry.get().strip()
		if pattern:
			cmd += ' -fn %s' % pattern
		try:
			cin, cout = os.popen2(cmd)
			# avoid duplicates
			lines = [x.rstrip() for x in \
				dict.fromkeys(cout.readlines(), 0).keys()]
			if self.tkShort.get():
				lines = [x for x in lines if len(x) < 32]
			lines.sort()
			self.scrolledlist.setlist(lines)
		except OSError:
			self.scrolledlist.clear()
                	self.result = None

	def __setFont(self):
		sellist = self.scrolledlist.getcurselection()
		if len(sellist) == 0:
			self.label.configure(text='')
			return
		self.result = sellist[0] 
		self.label.configure(font=self.result)

	def show(self):
		return self.result

def askfont(**options):
	if not options:
		options = {}
	return Chooser(options.get('parent', None), \
		options.get('title', None), \
		options.get('font', 'fixed')).result

if __name__ == '__main__':
	root = Tk()
	root.withdraw()
	print askfont(parent=root, title='Font Browser')
