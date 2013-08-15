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
#       MessageBar.py
#       GFS1-NHD:A7804.0000-SCRIPT;1.3
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.3 (DELIVERED)
#         Created:  07-MAY-2005 11:35:24      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.2 (DELIVERED)
#         Created:  15-FEB-2005 18:23:19      TROJAN
#           spr 6529
#       
#       Revision 1.1 (APPROVED)
#         Created:  01-JUL-2004 14:41:32      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_6832
#       	Action Date:       07-JUN-2005 13:13:53
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Add PVCS doc blocks
#       
#
# Class to display messages in an information line.
# code derived derived from Pmw.MessageBar
# implements blinking label, history display and logging capabilities
# last update: 02/01/05

import Tkinter
import Pmw
import logging, time
import Balloon

class MessageBar(Pmw.MegaWidget):
    def __init__(self, parent = None, **kw):

	# Define the megawidget options.
	INITOPT = Pmw.INITOPT
	defaultMessageTypes = {
			   # (priority, showtime, flash, loglevel)
	    'systemerror'  : (5, 20, 'red', logging.ERROR),
	    'usererror'    : (4, 12, 'orange', logging.WARNING),
	    'busy'         : (3, 0, None, 0),
	    'systemevent'  : (3, 8, 'green', logging.INFO),
	    'userevent'    : (2, 5, 'green', logging.INFO),
	    'help'         : (1, 5, None, 0),
	    'state'        : (0, 0, None, 0),
	}
	optiondefs = (
	    ('labelmargin',    0,                     INITOPT),
	    ('labelpos',       'w',                   INITOPT),
	    ('label_width',    2,                     INITOPT),
	    ('messagetypes',   defaultMessageTypes,   INITOPT),
	    ('silent',         0,                     None),
	    ('sticky',         'ew',                  INITOPT),
	    ('logname',        '',                    INITOPT),
	    ('history',        0,                     INITOPT),
	)
	self.defineoptions(kw, optiondefs)

	# Initialise the base class (after defining the options).
	Pmw.MegaWidget.__init__(self, parent)

	# Create the components.
	interior = self.interior()
	self._messageBarEntry = self.createcomponent('entry',
		(), None,
		Tkinter.Entry, (interior,))

        # somehow readonly background is not processed correctly
        bg = self._messageBarEntry.cget('background')
	self._messageBarEntry.configure(readonlybackground=bg, state='readonly')

        self._messageBarEntry.grid(column=2, row=2, sticky=self['sticky'])
        interior.grid_columnconfigure(2, weight=1)
        interior.grid_rowconfigure(2, weight=1)
        if self['history']:
            self._textDialog = Pmw.TextDialog(interior, \
                scrolledtext_labelpos = 'n', \
		label_text = 'Past messages', \
		defaultbutton=0, \
		)
            self._textDialog.withdraw()
            self._textDialog.configure(text_state='disabled')
            self._pushButton = self.createcomponent('button', \
                (), None, \
                Tkinter.Button, (interior,), \
                command=self._showHistory, \
                )
            self._pushButton.grid(column=3, row=2, sticky=self['sticky'])
            Balloon.Balloon().bind(self._pushButton, 'Show past messages')

	self.createlabel(interior)
        Balloon.Balloon().bind(self.component('label'), 'New message indicator')

	# Initialise instance variables.
	self._numPriorities = 0
	for info in self['messagetypes'].values():
	    if self._numPriorities < info[0]:
		self._numPriorities = info[0]

	self._numPriorities = self._numPriorities + 1
	self._timer = [None] * self._numPriorities
	self._messagetext = [''] * self._numPriorities
	self._activemessage = [0] * self._numPriorities
	self._activebackground = [0] * self._numPriorities
	self._flashtimer = None
	self._flash = {'off': self.component('label').cget('background')}
        if self['logname']:
            self._logger = logging.getLogger(self['logname'])
        else:
            self._logger = None

	# Check keywords and initialise options.
	self.initialiseoptions()

    def createlabel(self, parent, childCols = 1, childRows = 1):
	labelpos = self['labelpos']
	labelmargin = self['labelmargin']
	if labelpos is None:
	    return
	frame = Tkinter.Frame(parent, relief='groove', bd=2)
	label = self.createcomponent('label',
		(), None,
		Tkinter.Label, (frame,))
	label.pack()
	if labelpos[0] in 'ns':
	    # vertical layout
	    if labelpos[0] == 'n':
		row = 0
		margin = 1
	    else:
		row = childRows + 3
		margin = row - 1
	    frame.grid(column=2, row=row, columnspan=childCols, sticky=labelpos)
	    parent.grid_rowconfigure(margin, minsize=labelmargin)
	else:
	    # horizontal layout
	    if labelpos[0] == 'w':
		col = 0
		margin = 1
	    else:
		col = childCols + 3
		margin = col - 1
	    frame.grid(column=col, row=2, rowspan=childRows, sticky=labelpos)
	    parent.grid_columnconfigure(margin, minsize=labelmargin)

    def destroy(self):
	for timerId in self._timer:
	    if timerId is not None:
		self.after_cancel(timerId)
	self._timer = [None] * self._numPriorities
	Pmw.MegaWidget.destroy(self)

    def message(self, type, text):
        # Display a message in the message bar.

	(priority, showtime, flashbg, loglevel) = self['messagetypes'][type]

	self._activemessage[priority] = 1
	self._activebackground[priority] = flashbg
	if text is None:
	    text = ''
	self._messagetext[priority] = text.replace('\n', ' ')
	self._redisplayInfoMessage()

        if loglevel and self._logger:
            self._logger.log(loglevel, text)

        if self['history']:
            # FIXME: implement circular buffer
	    self._textDialog.appendtext('%s: %s\n' % \
		(time.strftime('%x %X'), text.rstrip()))

	if showtime > 0:
	    if self._timer[priority] is not None:
		self.after_cancel(self._timer[priority])

	    # Define a callback to clear this message after a time.
	    def _clearmessage(self=self, priority=priority):
		self._clearActivemessage(priority)

	    mseconds = int(showtime * 1000)
	    self._timer[priority] = self.after(mseconds, _clearmessage)

    def helpmessage(self, text):
        if text is None:
            self.resetmessages('help')
        else:
            self.message('help', text)

    def resetmessages(self, type):
	priority = self['messagetypes'][type][0]
	self._clearActivemessage(priority)
	for messagetype, info in self['messagetypes'].items():
	    thisPriority = info[0]
	    showtime = info[1]
	    if thisPriority < priority and showtime != 0:
		self._clearActivemessage(thisPriority)

    def _clearActivemessage(self, priority):
	self._activemessage[priority] = 0
	self._activebackground[priority] = None
	if self._timer[priority] is not None:
	    self.after_cancel(self._timer[priority])
	    self._timer[priority] = None
	self._redisplayInfoMessage()

    def _flashLabel(self):
        label = self.component('label')
        bg = label.cget('background')
        if bg == self._flash['off']:
            label.configure(background=self._flash['on'])
        else:
            label.configure(background=self._flash['off'])
        self._flashtimer = self.after(500, self._flashLabel)

    def _redisplayInfoMessage(self):
	text = ''
        for priority in range(self._numPriorities - 1, -1, -1):
	    if self._activemessage[priority]:
		text = self._messagetext[priority]
		flashbg = self._activebackground[priority]
	        break
	    else:
		flashbg = None
	self._messageBarEntry.configure(state = 'normal')
	self._messageBarEntry.delete(0, 'end')
	self._messageBarEntry.insert('end', text)

	if self._flashtimer is not None:
	    self.after_cancel(self._flashtimer)
	    self._flashtimer = None
	    self.component('label').configure(background=self._flash['off'])

	if not self['silent'] and flashbg:
	    self._flash['on'] = flashbg
	    self._flashtimer = self.after(100, self._flashLabel)
	else:
            self._flashtimer = None

        self._messageBarEntry.configure(state = 'readonly')

    def _showHistory(self):
        self._textDialog.show()

Pmw.forwardmethods(MessageBar, Tkinter.Entry, '_messageBarEntry')
