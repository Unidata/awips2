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
#       MosViewer.py
#       GFS1-NHD:A6642.0000-SCRIPT;1.38
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.38 (DELIVERED)
#         Created:  17-JUL-2009 16:40:06      OBERFIEL
#           Viewers now use a resource to determine the Routine button
#           setting.
#       
#       Revision 1.37 (DELIVERED)
#         Created:  26-MAR-2009 20:31:31      OBERFIEL
#           Removed references to NGM MOS and made few other cosmetic
#           changes
#       
#       Revision 1.36 (REVIEW)
#         Created:  20-MAR-2009 18:22:15      OBERFIEL
#           Removed code cruft. ETA changed to NAM. NGMMOS removed.
#       
#       Revision 1.35 (INITIALIZE)
#         Created:  18-MAR-2009 10:51:14      GILMOREDM
#           Now uses tagBalloon function to avoid certain issues with
#           using the Balloon function
#       
#       Revision 1.34 (DELIVERED)
#         Created:  30-DEC-2008 12:46:08      OBERFIEL
#           Check to see that balloon remains below mouse pointer
#       
#       Revision 1.33 (DELIVERED)
#         Created:  01-AUG-2008 15:44:44      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 1.32 (DELIVERED)
#         Created:  19-MAR-2008 14:30:35      OBERFIEL
#           Removed carriage returns.
#       
#       Revision 1.31 (DELIVERED)
#         Created:  19-FEB-2008 14:52:33      GILMOREDM
#           Fixed to allow users to enter TAF Editor when TAF
#           generation using MOS data not possible.
#       
#       Revision 1.30 (REVIEW)
#         Created:  25-JAN-2008 10:50:24      GILMOREDM
#           Removed call to Pmw.Balloon method, because raised balloons
#           created with that method were persisting when a "Check now"
#           was triggered from the status window.  Call to Balloon()
#           method corrects this problem.
#       
#       Revision 1.29 (DELIVERED)
#         Created:  14-MAY-2007 10:04:49      OBERFIEL
#           Removed references to the obsolete prototype XTF product.
#           Allow decoder and encoder to format TAF in two different
#           ways.  New format will be triggered by day and time to be
#           specified at a later date.
#       
#       Revision 1.28 (DELIVERED)
#         Created:  05-JAN-2007 10:54:14      OBERFIEL
#           Update to put more balloon messages when MOS and LAMP
#           messages are displayed in viewer.
#       
#       Revision 1.27 (DELIVERED)
#         Created:  23-OCT-2006 11:05:32      BLI
#           Fixed an outdated method name
#       
#       Revision 1.26 (DELIVERED)
#         Created:  26-SEP-2006 09:37:05      BLI
#           Modified to work with new TafGen
#       
#       Revision 1.25 (DELIVERED)
#         Created:  02-AUG-2006 15:40:21      BLI
#           Fixed showAll to use LampTafGenerator when gfs lamp is the
#           data source
#       
#       Revision 1.24 (DELIVERED)
#         Created:  06-FEB-2006 09:53:25      TROJAN
#           missing check for undefined alternate sites
#       
#       Revision 1.23 (REVIEW)
#         Created:  02-FEB-2006 15:58:02      TROJAN
#           wrong method prevented display when "All" toggle  set
#       
#       Revision 1.22 (APPROVED)
#         Created:  02-FEB-2006 12:32:49      BLI
#           Call LampTafGen when model=gfslamp
#       
#       Revision 1.21 (REVIEW)
#         Created:  01-FEB-2006 15:15:38      TROJAN
#           Incorrect handling of missing data
#       
#       Revision 1.20 (REVIEW)
#         Created:  31-JAN-2006 08:35:26      TROJAN
#           Change in naming convention for MOS/LAMP data, added LAMP
#           to plotting module
#       
#       Revision 1.19 (INITIALIZE)
#         Created:  30-JAN-2006 09:15:41      TROJAN
#           stdr 958
#       
#       Revision 1.18 (APPROVED)
#         Created:  27-JAN-2006 14:54:02      BLI
#           Modified to toggle on/off prob. of LAMP
#       
#       Revision 1.17 (REVIEW)
#         Created:  03-NOV-2005 13:39:38      TROJAN
#           spr 7052
#       
#       Revision 1.16 (DELIVERED)
#         Created:  15-SEP-2005 18:31:15      TROJAN
#           spr 7023
#       
#       Revision 1.15 (DELIVERED)
#         Created:  06-JUL-2005 21:01:41      TROJAN
#           spr 6909
#       
#       Revision 1.14 (DELIVERED)
#         Created:  07-MAY-2005 11:36:42      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.13 (DELIVERED)
#         Created:  18-APR-2005 18:56:32      BLI
#           Added handler for hide prob. of cloud cover
#       
#       Revision 1.12 (APPROVED)
#         Created:  18-APR-2005 18:12:20      TROJAN
#           stdr 917
#       
#       Revision 1.11 (DELIVERED)
#         Created:  02-APR-2005 17:47:24      TROJAN
#           spr 6772
#       
#       Revision 1.10 (APPROVED)
#         Created:  21-MAR-2005 15:47:11      TROJAN
#           spr 6736
#       
#       Revision 1.9 (DELIVERED)
#         Created:  02-FEB-2005 18:44:23      BLI
#           Fixed a bug
#       
#       Revision 1.8 (APPROVED)
#         Created:  27-JAN-2005 19:09:44      BLI
#           Modified a module name
#       
#       Revision 1.7 (APPROVED)
#         Created:  27-JAN-2005 18:35:22      BLI
#           Modified to use new taf formatter
#       
#       Revision 1.6 (APPROVED)
#         Created:  07-DEC-2004 19:25:38      TROJAN
#           spr 6528
#       
#       Revision 1.5 (APPROVED)
#         Created:  30-SEP-2004 20:53:43      TROJAN
#           stdr 873
#       
#       Revision 1.4 (APPROVED)
#         Created:  09-JUL-2004 19:11:12      OBERFIEL
#           Replaced busy dialogs
#       
#       Revision 1.3 (APPROVED)
#         Created:  01-JUL-2004 14:59:47      OBERFIEL
#           Update
#       
#       Revision 1.2 (DELIVERED)
#         Created:  08-JAN-2004 21:40:20      PCMS
#           Updating for code cleanup
#       
#       Revision 1.1 (APPROVED)
#         Created:  06-NOV-2003 16:46:15      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7417
#       	Action Date:       06-OCT-2009 09:42:01
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: TUG code does not handle transition from warm to cold seasons
#       
#

import copy, logging, time
from Tkinter import *
import Pmw
from Balloon import *
import Avn, AvnLib, AvnParser, Globals, TafDecoder, TafGen

_Logger = logging.getLogger(__name__)

###############################################################################
class Viewer:
    Format = ['table', 'long', 'short']
    def __init__(self, master, tafviewer, **kw):
        # master: parent widget (page in a notebook)
        # mos: MOS class implementing methods:
        #   makeHeader(), makeRecords(), makeFMRec(), 
        #   makeTEMPORec(), makeReport()
        # tafviewer: TafViewer instance
        self._master = master
        self._id = None
        self._ids = []
        self._mosid = None
        self._model = kw['model']
        self._tafviewer = tafviewer
        self._decoder = TafDecoder.Decoder()
        self.messagebar = kw['messagebar']

        self._tkShowProbs = IntVar()
        self._tkShowProbs.set(int(master.option_get('showProbs', '')))
        self._tkHighlightFlightCat = IntVar()
        self._tkHighlightFlightCat.set(int(master.option_get( \
            'highlightFlightCat', '')))
        self._format = master.option_get('showFormatted', '')
        if self._format not in self.Format:
            self._format = self.Format[0]
        self._tkBBB = StringVar()
        self._tkBBB.set({'0':'AAA'}.get(master.option_get('showRoutine', ''),'   '))
        self._tkAll = IntVar()

        frame = Frame(master)
        cb = Checkbutton(frame,
            text='Probabilities',
            variable=self._tkShowProbs,
            command=self.load,
            )
        cb.pack(side='right', padx=5)
        Balloon().bind(cb, 'Display Vsby and Cig probabilities')
        cb = Checkbutton(frame,
            text='Flight Categories',
            variable=self._tkHighlightFlightCat,
            command=self.load,
            )
        cb.pack(side='right', padx=5)
        Balloon().bind(cb, 'Background color depicts flight categories')
        rbtns = Pmw.RadioSelect(frame,
            buttontype = 'radiobutton',
            labelpos='w',
            label_text='Format',
            orient='horizontal',
            hull_borderwidth=2,
            hull_relief='ridge',
            command=self.__setformat,
            )
        rbtns.pack(side='right', padx=5)
        for text in self.Format:
            rbtns.add(text)
        Balloon().bind(rbtns, 'Display format')
        rbtns.setvalue(self._format)
        cb = Checkbutton(frame,
            text='Routine',
            variable=self._tkBBB,
            onvalue='   ',
            offvalue='AAX',
            command=self.load,
            )
        cb.pack(side='right', padx=5)
        Balloon().bind(cb, 'Determines TAF issuance time')
        cb = Checkbutton(frame,
            text='All',
            variable=self._tkAll,
            command=self.load,
            )
        cb.pack(side='right', padx=5)
        Balloon().bind(cb, 'Displays data for all sites')
        frame.pack(side='top', expand='no', fill='x')

        self.text = Pmw.ScrolledText(master,
            columnheader=1,
            columnheader_height=2,
            columnheader_name='textViewer',
            borderframe = 1,
            vscrollmode='static',
            hscrollmode='dynamic',
            text_state='disabled',
            text_wrap='none',
            text_setgrid='true',
            text_name='textViewer',
            )
        self.text.pack(side='top', expand='yes', fill='both')
        self.text.component('text').bind('<Button-3>', self.__popupMenu)

        self.popmenu = Menu(master,
            tearoff=0,
            type='normal',
            )
        self.popmenu.add_command(label='Copy', command=self.__copy)

        AvnLib.configureFlightCatColors(self.text)

    def __copy(self):
        try:
            t = self.text.component('text')
            t.selection_own()
            selection = t.selection_get()
            t.clipboard_clear()
            t.clipboard_append(selection)
        except:
            pass

    def __popupMenu(self, e):
        self.popmenu.tk_popup(e.widget.winfo_rootx() + e.x,
            e.widget.winfo_rooty() + e.y)

    def __makeHeader(self, data):
        return '%s %s Guidance  %s' % (self._model.upper()[:3],self._model.upper()[3:],
                                       time.strftime('%m/%d/%y %H%M UTC', 
                                                     time.gmtime(data['itime']['value'])))

    def __showFormatted(self):
        data = Globals.DRC.getMosData(self._mosid, self._model)
        if data is None:
            raise Avn.AvnError('Cannot retrieve data for %s' % self._mosid)
        self.text.component('columnheader').insert('end', 
            self.__makeHeader(data.data))
        tc = TafGen.TafGen(self._model,data.data,self._tkBBB.get())
        donot_group = self._format == 'long'
        taf=tc.createTaf(donot_group)
        self.text.insert('end','\n'.join(taf))
        if self._tkHighlightFlightCat.get():
            dcd = self._decoder(taf)
            if 'fatal' in dcd:
                return
            for ix, g in AvnLib.getTafPeriods(dcd):
                fc = AvnLib.flightCategory(g)
                self.text.tag_add(fc, *ix)

    def __showTable(self):
        data = Globals.DRC.getMosTable(self._mosid, self._model)
        if data is None:
            raise Avn.AvnError('Cannot retrieve data for %s' % self._mosid)
        rpt = data.table.split('\n')
        htw = self.text.component('columnheader')
        htw.insert('end', '\n'.join(rpt[:2]))
        if self._tkShowProbs.get():
            self.text.insert('end', '\n'.join(rpt[2:]))
        else:
            self.text.insert('end', '\n'.join([x for x in rpt[2:] \
                if not x.startswith('cat') and not x.startswith('PSKC') \
		and not x.startswith('PFEW') and not x.startswith('PBKN') \
		and not x.startswith('POVC') and not x.startswith('PSCT') \
		and not x.startswith('PRain') and not x.startswith('PSnow') \
                and not x.startswith('PFrz')]))
        
        balloonMsgs = self.__createCategoryBalloons(self.text.getvalue().split('\n'))
        for n, msg in enumerate(balloonMsgs):
            tag = 'cat'+str(n)
            self.text.tag_add(tag,*msg[1])
            tagBalloon(relmouse='both',xoffset=10,yoffset=-15,pinned=True).tagbind(self.text,tag,msg[0])
            
        if self._tkHighlightFlightCat.get():
            data = Globals.DRC.getMosData(self._mosid, self._model)
            if not data:
                return
            nrows = len(rpt) - 2
            c = 9   # length of row label
            for g in data.data['group']: 
                fc = AvnLib.flightCategory(g)
                for n in range(1, nrows+1):
                    self.text.tag_add(fc, '%d.%d' % (n,c), '%d.%d' % (n,c+3))
                c += 4  # column width + space

    def __createCategoryBalloons(self,table):
        """Create informative balloon messages about MOS categories"""
        balloonMsgs = []
        if self._model[3:] not in ['mos','lamp']:
            return balloonMsgs
        
        visDict = { '1':'<1/2SM',
                    '2':'1/2-<1SM',
                    '3':'1-<2SM',
                    '4':'2-<3SM',
                    '5':'3-5SM',
                    '6':'6SM',
                    '7':'>6SM' }
        
        cigDict = { '1':'<200ft',
                    '2':'200-400ft',
                    '3':'500-900ft',
                    '4':'1000-1900ft',
                    '5':'2000-3000ft',
                    '6':'3100-6500ft',
                    '7':'6600-12000ft',
                    '8':'>12000ft' }
        
        qpfDict = { '1':'<0.1"',
                    '2':'0.1-<1/4"',
                    '3':'1/4-<1/2"',
                    '4':'1/2-<1"',
                    '5':'1-<2"',
                    '6':'>=2"' }
        r = 0
        for line in table:
            r +=  1                
            if line.startswith('VIS') or line.startswith('CVIS'):
                lastCategory = 'VIS'
                c = 9
                llen=len(line.rstrip())
                while c < llen:
                    index=line[c:c+3].strip()
                    if index:
                        balloonMsgs.append((visDict.get(index,'>6SM'),('%d.%d'%(r,c),'%d.%d'%(r,c+3))))
                    c += 4                    
            elif line.startswith('CIG') or line.startswith('CCIG'):
                lastCategory = 'CIG'
                c = 9
                llen=len(line.rstrip())
                while c < llen:
                    index=line[c:c+3].strip()
                    if index:
                        balloonMsgs.append((cigDict.get(index,'Ulmtd'),('%d.%d'%(r,c),'%d.%d'%(r,c+3))))
                    c += 4
            elif line.startswith('QPF06'):
                c = 17
                for fprd in line[9:].split():
                    balloonMsgs.append((qpfDict.get(fprd,'None'),('%d.%d'%(r,c),'%d.%d'%(r,c+3))))
                    c += 8
                
            if line.startswith('cat'):
                if lastCategory == 'VIS':
                    category=line[3]
                    balloonMsgs.append((visDict.get(line[3],'>6SM'),('%d.0'%r,'%d.4'%r)))
                elif lastCategory == 'CIG':
                    balloonMsgs.append((cigDict.get(line[3],'Ulmtd'),('%d.0'%r,'%d.4'%r)))
                    
        return balloonMsgs

    def __showAll(self):
        donot_group = self._format == 'long'
        bbb = self._tkBBB.get()
        data = None
	mosdata = Globals.DRC.getMosData(self._ids, self._model)
	if mosdata:
	    for data in mosdata:
		tc=TafGen.TafGen(self._model,data.data,bbb)
		taf=tc.createTaf(donot_group)
		offset = int(self.text.index('end').split('.')[0])-2
		self.text.insert('end', '\n'.join(taf))
		self.text.insert('end', '\n')
		if self._tkHighlightFlightCat.get():
		    dcd = self._decoder(taf, bbb, offset)
		    if 'fatal' in dcd:
			continue
		    for ix, g in AvnLib.getTafPeriods(dcd):
			fc = AvnLib.flightCategory(g)
			self.text.tag_add(fc, *ix)
	else:
	    self.messagebar().message('usererror',"Insufficient Data for TAF Generation")
        if data:
            self.text.component('columnheader').insert('end', 
                self.__makeHeader(data.data))
	    
    def __setformat(self, tag):
        self._format = tag
        self.load()

###############################################################################
# public methods
###############################################################################
    def load(self, arg=None):
        self._tafviewer.load(self._id)
        self.text.configure(text_state='normal')
        self.text.clear()
        htw = self.text.component('columnheader')
        htw.configure(state='normal')
        htw.delete('1.0', 'end')
        msg = None
        if self._tkAll.get():
            if self._format == 'table':
                msg = 'Table display for all sites is not supported'
            else:
                self.__showAll()
        else:
            if self._mosid:
                try:
                    if self._format == 'table':
                        self.__showTable()
                    else:
                        self.__showFormatted()
                except Avn.AvnError, e:
                    msg = str(e)
                    _Logger.warning(msg)
                except Exception:
                    msg = 'Cannot load data for %s' % self._id
                    _Logger.exception(msg)
            else:
                msg = 'There is no MOS data for %s' % self._id
        self.text.configure(text_state='disabled')
        htw.configure(state='disabled')
        if self._master.winfo_ismapped():
            if msg:
                self.messagebar().message('usererror', msg)
            else:
                self.messagebar().resetmessages('usererror')

    def setSite(self, id_, ids):
        if self._id == id_:
            return
        self._id = id_
        self._mosid = None
        self._ids = []
        for i in ids:
            siteinfo = AvnParser.getTafSiteCfg(i)
            if siteinfo and siteinfo['sites'].get(self._model):
                self._ids.extend(siteinfo['sites'][self._model])
                if i == self._id:
                    self._mosid = self._ids[-1]
