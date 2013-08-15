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
#       TUGViewer.py
#       GFS1-NHD:A10031.0000-SCRIPT;6
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 6 (DELIVERED)
#         Created:  17-JUL-2009 16:40:07      OBERFIEL
#           Viewers now use a resource to determine the Routine button
#           setting.
#       
#       Revision 5 (DELIVERED)
#         Created:  01-AUG-2008 15:44:45      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 4 (DELIVERED)
#         Created:  19-JUN-2008 14:20:42      OBERFIEL
#           Allowed variable length of TAF -- not just 24h.
#       
#       Revision 3 (DELIVERED)
#         Created:  18-APR-2008 14:38:03      OBERFIEL
#           Improved exception handling, changed header to be
#           consistent with other viewers.
#       
#       Revision 2 (DELIVERED)
#         Created:  18-MAR-2008 14:41:08      OBERFIEL
#           Added check for missing thresholds and TAFs when All button
#           is pressed.
#       
#       Revision 1 (DELIVERED)
#         Created:  20-NOV-2007 16:29:54      OBERFIEL
#           New Module for TAF Editor/Viewer
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
from Balloon import Balloon
import Avn, AvnLib, AvnParser, Globals, TafDecoder, TafGen, TAMPGenerator

_Logger = logging.getLogger(__name__)

###############################################################################
class Viewer:
    Format = ['long', 'short']
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
        self._tkHighlightFlightCat.set(int(master.option_get('highlightFlightCat', '')))
        self._format = master.option_get('showFormatted', '')
        if self._format not in self.Format:
            self._format = self.Format[0]
        self._tkBBB = StringVar()
        self._tkBBB.set({'1':'AAA'}.get(master.option_get('showProbs', ''),'   '))
        self._tkAll = IntVar()
        self._tkCigVisToggle = IntVar()

        frame = Frame(master)
        cb = Checkbutton(frame,
            text='Cig/Vis Only',
            variable=self._tkCigVisToggle,
            command=self.load,
            )
        cb.pack(side='right', padx=5)
        Balloon().bind(cb, 'Change ceiling and visibility only')

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
        return '%s TAF/%s Guidance %s' % (self._id, self._model.upper(), \
            time.strftime('%m/%d/%y %H%M UTC', 
            time.gmtime(data['itime']['value'])))

    def __showFormatted(self):
        
        newTaf = None
        bbb=self._tkBBB.get()
        cvOnly = self._tkCigVisToggle.get()
        LAMP = Globals.DRC.getMosData(self._mosid, self._model)
        if LAMP is None:
            msg = 'Cannot retrieve LAMP data for %s' % self._mosid
            self.messagebar().message('usererror', msg)
        
        tmtuple = time.gmtime(LAMP.data['itime']['value'])
        thresholds = Globals.DRC.getProbs(self._mosid,[tmtuple[7],('%02d'%tmtuple[3])])
        if thresholds is None:
            msg = 'Missing LAMP thresholds at this hour for %s' % self._mosid
            self.messagebar().message('usererror', msg)

        self.text.component('columnheader').insert('end',self.__makeHeader(LAMP.data))
        
        try:
            TAF = Globals.DRC.getTafs(self._id,True,time.time()-25200,1)[0]
        except:
            msg = 'Cannot retrieve TAF for %s' % self._id
            self.messagebar().message('usererror', msg)

        try:
            siteinfo = AvnParser.getTafSiteCfg(LAMP.data['ident']['str'])
            tafDuration = int(siteinfo['thresholds'].get('tafduration','24'))
            newTaf = TAMPGenerator.TAMPGenerator(LAMP,TAF.dcd['group'],thresholds,
                                                 bbb,cvOnly,(self._format == 'long'),
                                                 tafDuration)
            
        except (KeyError,TypeError,AttributeError):
            msg = 'Unable to format %s' % self._id
            self.messagebar().message('usererror', msg)
        
        if newTaf:
            self.text.insert('end','\n'.join(newTaf))
            if self._tkHighlightFlightCat.get():
                dcd = self._decoder(newTaf)
                if 'fatal' in dcd:
                    return
                for ix, g in AvnLib.getTafPeriods(dcd):
                    fc = AvnLib.flightCategory(g)
                    self.text.tag_add(fc, *ix)
                                       
    def __showAll(self):
        """Show all TAFs being actively monitored."""
        bbb = self._tkBBB.get()
        cvOnly = self._tkCigVisToggle.get()
        offset = 0
        LAMPs = Globals.DRC.getMosData(self._ids,self._model)
        if LAMPs is None:
            raise Avn.AvnError('No guidance available' )
        
        TAFs = []
        for  _id in self._ids:
            try:
                TAFs.append(Globals.DRC.getTafs(_id,True,time.time()-25200,1)[0])
            except IndexError:
                msg = 'No recent TAF for %s' % _id
                self.messagebar().message('usererror', msg)
                
        if TAFs is None:
            msg = 'No issued TAFs available'
            self.messagebar().message('usererror', msg)
        
        Thresholds = []
        for LAMP in LAMPs:
            tmtuple = time.gmtime(LAMP.data['itime']['value'])
            Thresholds.append(Globals.DRC.getProbs(LAMP.data['ident']['str'],
                                                  [tmtuple[7],('%02d'%tmtuple[3])]))

        if not (len(LAMPs) == len(TAFs) == len(Thresholds)):
            msg = 'Unable to show all TAFs due to missing data'
            self.messagebar().message('usererror', msg)
            
        for LAMP,TAF,thresholds in zip(LAMPs,TAFs,Thresholds):
            try:
                siteinfo = AvnParser.getTafSiteCfg(LAMP.data['ident']['str'])
                tafDuration = int(siteinfo['thresholds'].get('tafduration','24'))
                newTaf = TAMPGenerator.TAMPGenerator(LAMP,TAF.dcd['group'],thresholds,
                                                     bbb,cvOnly,(self._format == 'long'),
                                                     tafDuration)
                
                offset = int(self.text.index('end').split('.')[0])-2
                self.text.insert('end', '\n'.join(newTaf))
                self.text.insert('end', '\n\n')
                
                if self._tkHighlightFlightCat.get():
                    dcd = self._decoder(newTaf, bbb, offset)
                    if 'fatal' in dcd:
                        continue
                    
                    for ix, g in AvnLib.getTafPeriods(dcd):
                        fc = AvnLib.flightCategory(g)
                        self.text.tag_add(fc, *ix)
                        
            except (KeyError,TypeError,AttributeError):
                msg = 'Unable to format %s' % LAMP.data['ident']['str']
                self.messagebar().message('usererror', msg)
                
        if offset:
            self.text.component('columnheader').insert('end', 
                self.__makeHeader(LAMP.data))

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
            self.__showAll()
        else:
            if self._mosid:
                try:
                    self.__showFormatted()
                except Avn.AvnError, e:
                    msg = str(e)
                    _Logger.warning(msg)
                except Exception:
                    msg = 'Cannot load data for %s' % self._id
                    _Logger.exception(msg)
            else:
                msg = 'There is no GFSLAMP data for %s' % self._id
                
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
