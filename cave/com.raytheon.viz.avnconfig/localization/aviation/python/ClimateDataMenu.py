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
#       %PM%
#       %PID%
#
#    Status:
#       %PS%
#    
#    History:
#       %PL%
#
#    Change Document History:
#       %PIRC%

import logging, os, time, stat, types, sys

import ConfigParser
from Tkinter import *
import tkFileDialog
import Pmw
from Balloon import Balloon
import Avn, AvnDialog
import ClimateDataDialog

TopDir = os.environ.get('TOP_DIR', '/awips/adapt/avnfps/')
FileLoc = os.path.join(TopDir,'data/climate')

_Help = {
    'title': 'Climate Data Site Creation Help',
    'content': """
Some Future Stuff
"""
}

_Logger = logging.getLogger(__name__)


##########################################################################################
#
#
class Menu(AvnDialog.Dialog):
    def __init__(self, parent=None, top=None, **kw):
	AvnDialog.Dialog.__init__(self, parent, **kw)

	# from AvnDialog
	self.title('Main Menu')

	self.createWidgets()

    def __del__(self):
	if hasattr(self, 'cdeditor'):
	    del self.cdeditor
	del self.parent.climenu

    def createWidgets(self):
	frame = Frame(self.interior())

	bbox = Pmw.ButtonBox(frame,
	    orient='vertical',
	    hull_borderwidth=2,
	    hull_relief='groove',
	    pady=1,
	    )
	btn = bbox.add('Append', command=self.__update)
	Balloon().bind(btn, 'Append New Data to Existing Climate File')
	btn = bbox.add('Create', command=self.__create)
	Balloon().bind(btn, 'Create New Climate File')
	btn = bbox.add('Resume', command=self.__resume)
	Balloon().bind(btn, 'Resume Previous Session')
	bbox.pack(side='right', expand='no', padx=5, pady=5)

	frame.pack(side='left', expand='yes')

	#self.setGeometry()

    def __create(self):
	if not hasattr(self, 'cdeditor'):
	    self.cdeditor = ClimateDataDialog.Editor(self.interior())
	self.cdeditor.display()
	self.cdeditor.rbsel.invoke(1)

    def __update(self):
	if not hasattr(self, 'cdeditor'):
	    self.cdeditor = ClimateDataDialog.Editor(self.interior())
	self.cdeditor.display()

    def __resume(self):
	if not hasattr(self, 'cdeditor'):
	    self.cdeditor = ClimateDataDialog.Editor(self.interior())
	self.cdeditor.display()
	self.cdeditor.restore()
