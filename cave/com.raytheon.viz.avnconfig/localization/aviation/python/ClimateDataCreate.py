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
#
import logging, os, re
import Avn, AvnDialog, ConfigParser, Pmw, ClimateProcessLogger
import ClimateDataUtils as cdutils
from Tkinter import *
from Balloon import Balloon

TopDir = os.environ.get('TOP_DIR','/awips/adapt/avnfps/')

_Help = {
    'title': 'Climate Data Site Creation Help',
    'content': """
Some Future Stuff
"""
}

_Logger = logging.getLogger(ClimateProcessLogger.CLIMATE_CATEGORY)

##########################################################################################
#
#
class Create(AvnDialog.Dialog):
    def __init__(self, parent=None, top=None, **kw):
	AvnDialog.Dialog.__init__(self, parent, **kw)

	self.top = top
	# from AvnDialog
	self.title('New Climate Site')
	self.createWidgets()

    def createWidgets(self):
	frame = Frame(self.interior())
        group = Pmw.Group(frame, tag_text='Identifiers')

	self.site_id = Pmw.EntryField(group.interior(),
                                      labelpos='nw',
                                      label_text='Site ID:',
                                      validate={'validator': 'alphanumeric',
                                                'min': 4, 'max': 4, 'minstrict': 0},
                                      entry_width=4,
                                      )
        
	self.site_id.grid(row=0, column=0,sticky='ew', padx=5)
	self.pil = Pmw.EntryField(group.interior(),
                                  labelpos='nw',
                                  label_text='METAR AFOS ID',
                                  entry_width=9,
                                  validate=self.__validateAFOS
                                  )

	self.pil.grid(row=0, column=1, sticky='ew', padx=5)
        Balloon().bind(self.pil, 'Used in Cig/Vis Trend Tool')

        group.pack(side='left', expand='yes', fill='x', pady=5)
	frame.pack(side='top', expand='yes')

        frame = Frame(self.interior(), relief='ridge', bd=2)
	bbox = Pmw.ButtonBox(frame)

	bbox.add('Save', command=self.__ok)
	bbox.add('Cancel', command=self.__cancel)
        
        bbox.alignbuttons()
        bbox.pack(side='left', expand='no')                
	frame.pack(side='top', expand='yes')

	self.setGeometry()

    def fill_fields(self):
	if self.site_id.getvalue() == '':
	    stns = self.top.idents.getvalue()
	    if not stns: return
	    pil, file = cdutils.getIdCfg(stns[0])
	    self.site_id.setvalue(stns[0])
	    self.pil.setvalue(pil)

    def __ok(self):
	stn = self.site_id.getvalue()
	file = os.path.join(TopDir,'data/climate/%s.hd5' % stn)
	pil = self.pil.getvalue()
	cdutils.updateIdsCfg(stn, pil, file)
	self.close()
	self.top.listIdents(stn)

    def __cancel(self):
	self.close()

    def __validateAFOS(self,text):
        text = text.strip()
        length = len(text)
        if length < 8:
            return Pmw.PARTIAL
        value = text.upper()
        if re.match('^[A-Z]{3,3}MTR[(0-9)|(A-Z)]{2,3}$', value):
            return Pmw.OK
    
        if length == 9:
            return Pmw.ERROR
        else:
            return Pmw.PARTIAL
        
