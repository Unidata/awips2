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
#       SiteInfoEditor.py
#       GFS1-NHD:A8060.0000-SCRIPT;24
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 24 (DELIVERED)
#         Created:  12-SEP-2008 12:28:21      OBERFIEL
#           Greatly improved visual layout of the new QC Check section.
#       
#       Revision 23 (REVIEW)
#         Created:  19-AUG-2008 14:26:40      GILMOREDM
#           added section to configure qc checks
#       
#       Revision 22 (DELIVERED)
#         Created:  15-JUL-2008 07:30:45      OBERFIEL
#           By default TAF duration is 24 hours for any new TAF.  Also
#           added first guess for ACARS ID.
#       
#       Revision 21 (DELIVERED)
#         Created:  06-NOV-2007 07:56:49      OBERFIEL
#           Change to allow user to enter a mixed-cased identifier
#       
#       Revision 20 (REVIEW)
#         Created:  02-NOV-2007 08:01:27      OBERFIEL
#           Checked in code to support ACARS field.
#       
#       Revision 19 (DELIVERED)
#         Created:  04-MAY-2007 15:26:57      OBERFIEL
#           Updated to write and read tafduration attribute from the
#           TAF info.cfg file.
#       
#       Revision 18 (REVIEW)
#         Created:  03-MAY-2007 08:28:43      OBERFIEL
#           Removed obsolete cruft, added new TAF duration field.
#           Added code to switch TAF to different template format on
#           certain date.
#       
#       Revision 17 (DELIVERED)
#         Created:  27-APR-2007 08:48:52      OBERFIEL
#           Fixed WMO validator to accept alphanumeric identifiers in
#           the GUI text field.
#       
#       Revision 16 (DELIVERED)
#         Created:  29-JUN-2006 07:50:32      TROJAN
#           spr 7181: modified "Update" action logic
#       
#       Revision 15 (DELIVERED)
#         Created:  14-APR-2006 13:25:17      TROJAN
#           spr 7117
#       
#       Revision 14 (DELIVERED)
#         Created:  14-APR-2006 13:17:49      TROJAN
#           spr 7118
#       
#       Revision 13 (APPROVED)
#         Created:  14-APR-2006 08:24:42      TROJAN
#           spr 7117
#       
#       Revision 12 (DELIVERED)
#         Created:  06-FEB-2006 10:09:29      TROJAN
#           several coding errors related to change of alternate site
#           list format
#       
#       Revision 11 (APPROVED)
#         Created:  30-JAN-2006 10:05:40      TROJAN
#           stdr 956
#       
#       Revision 10 (DELIVERED)
#         Created:  09-SEP-2005 12:35:23      TROJAN
#           spr 7011
#       
#       Revision 9 (DELIVERED)
#         Created:  07-MAY-2005 11:38:11      OBERFIEL
#           Added Item Header Block
#       
#       Revision 8 (DELIVERED)
#         Created:  18-APR-2005 17:57:38      TROJAN
#           spr 6800
#       
#       Revision 7 (DELIVERED)
#         Created:  21-MAR-2005 15:14:37      TROJAN
#           spr 6732
#       
#       Revision 6 (DELIVERED)
#         Created:  15-FEB-2005 18:28:18      TROJAN
#           spr 6529
#       
#       Revision 5 (APPROVED)
#         Created:  14-FEB-2005 19:29:05      TROJAN
#           spr 6648
#       
#       Revision 4 (APPROVED)
#         Created:  07-DEC-2004 18:50:40      TROJAN
#           spr 6451
#       
#       Revision 3 (APPROVED)
#         Created:  09-NOV-2004 15:07:04      OBERFIEL
#           Added LLWS functions
#       
#       Revision 2 (APPROVED)
#         Created:  08-NOV-2004 19:01:29      OBERFIEL
#           Changes to support LLWS
#       
#       Revision 1 (APPROVED)
#         Created:  30-SEP-2004 18:21:27      TROJAN
#           rewrite of avnsetup
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7383
#       	Action Date:       06-NOV-2008 15:25:22
#       	Relationship Type: In Response to
#       	Status:           TEST
#       	Title:             AvnFPS: Lack of customization in QC check
#       
#
import logging, os, re
import ConfigParser
from Tkinter import *
import Pmw
from Balloon import Balloon
import Avn, AvnDialog, AvnParser, EditDialog

_Help = {
    'title': 'Site Setup Help',
    'content': """
This dialog is used to define TAF site information.

To add a new site, enter site id and press "Update". Edit displayed
entries and press "Save". Create default template files.

To change an existing TAF site attribute, enter site id and press
the "Load" button.

To create template files, press "Make" in the "Templates" area.

To edit template file, select issue hour, then press "Edit" in 
the "Templates" area.

You can use "Update" button to extract information from AWIPS
configuration files. Only non-empty fields will be overwritten.
"""
}

_Issue_Hours = ['00Z', '06Z', '12Z', '18Z']
_A2A = os.path.join(os.environ['FXA_HOME'], 'data', 'afos2awips.txt')
_Geo = os.path.join(os.environ['FXA_HOME'], 'data', 'metarStationInfo.txt')
_Logger = logging.getLogger(__name__)

def _validateAFOS(text):
    text = text.strip()
    length = len(text)
    if length < 8:
        return Pmw.PARTIAL
    value = text.upper()
    if re.match('^[A-Z]{3,3}TAF[(0-9)|(A-Z)]{2,3}$', value):
        return Pmw.OK
    
    if length == 9:
        return Pmw.ERROR
    else:
        return Pmw.PARTIAL

def _validateWMO(text):
    text = text.strip()
    length = len(text)
    if length < 11:
        return Pmw.PARTIAL
    value = text.upper()
    if re.match('^[A-Z]{4}[0-9]{2} [KPTI][A-Z]{3}$', value):
        return Pmw.OK
    else:
        return Pmw.ERROR

def _validateTafduration(text):
    try:
        times=[24,30]
        duration=int(text)
        if duration in times:
            return Pmw.OK
        else:
            if len(times) > 1:
                return Pmw.PARTIAL
            else:
                return Pmw.FAIL
    except:
        return Pmw.PARTIAL
        
def _validateRunway(text):
    if ',' not in text:
        return Pmw.PARTIAL
    try:
        tmp = [int(x) for x in text.split(',') if x]
    except ValueError:
        return Pmw.ERROR
    for x in tmp:
        if not 0 <= x <= 360:
            return Pmw.ERROR
    return Pmw.OK

def _validateHeight(text):
    text = text.strip()
    if not text:
        return Pmw.OK
    try:
        tmp = [int(x) for x in text.split(',') if x]
    except ValueError:
        return Pmw.ERROR
    return Pmw.OK

def _validateDist(text):
    text = text.strip()
    if not text:
	return Pmw.OK
    try:
	tmp = [int(x) for x in text.split(',') if x]
    except ValueError:
	return Pmw.ERROR
    return Pmw.OK

def _validateMetar(text):
    if not text:
        return Pmw.PARTIAL
    for x in text.split(','):
        n = len(x)
        if n == 0:
            continue
        elif n > 4:
            return Pmw.ERROR
        elif n < 4:
            return Pmw.PARTIAL
        if Pmw.alphanumericvalidator(x) == Pmw.ERROR:
            return Pmw.ERROR
    return Pmw.OK

def _validateProfiler(text):
    text = text.strip()
    if not text:
        return Pmw.OK
    for x in text.split(','):
        n = len(x)
        if n == 0:
            continue
        elif n > 5:
            return Pmw.ERROR
        elif n < 5:
            return Pmw.PARTIAL
        if Pmw.alphanumericvalidator(x) == Pmw.ERROR:
            return Pmw.ERROR
    return Pmw.OK

def _validateRadar(text):
    text = text.strip()
    if not text:
        return Pmw.OK
    for x in text.split(','):
        n = len(x)
        if n == 0:
            continue
        elif n > 4:
            return Pmw.ERROR
        elif n < 4:
            return Pmw.PARTIAL
        if Pmw.alphabeticvalidator(x) == Pmw.ERROR:
            return Pmw.ERROR
    return Pmw.OK

def _validateOther(text):
    text = text.strip()
    n = len(text)
    if n == 0:
        return Pmw.OK
    elif n > 4:
        return Pmw.ERROR
    elif n < 3:
        return Pmw.PARTIAL
    return Pmw.alphanumericvalidator(text)

def _validateVsby(text):
    if ',' not in text:
        return Pmw.PARTIAL
    try:
        tmp = [float(x) for x in text.split(',') if x]
    except ValueError:
        return Pmw.ERROR
    for x in tmp:
        if x < 0.0 or x > 10.0:
            return Pmw.ERROR
    tmps = tmp[:]
    tmps.sort()
    if tmps != tmp:
        return Pmw.PARTIAL
    return Pmw.OK

def _validateCig(text):
    if ',' not in text:
        return Pmw.PARTIAL
    try:
        tmp = [int(x) for x in text.split(',') if x]
    except ValueError:
        return Pmw.ERROR
    for x in tmp:
        if x < 0 or x > 30000:
            return Pmw.ERROR
    tmps = tmp[:]
    tmps.sort()
    if tmps != tmp:
        return Pmw.PARTIAL
    return Pmw.OK

def getHeaders(kind, ident):
    tid = kind+ident[1:]
    for line in file(_A2A):
        if line[3:9] == tid:
            return line.rstrip().split(None, 1)
    else:
        return None

def getGeography(ident):
    for line in file(_Geo):
        if line.startswith('#'):
            continue
        syno, i, lat, lon, elev, name, country, junk = line.split('|')
        if i.strip() == ident:
            return lat.strip(), lon.strip(), elev.strip()
    else:
        return None

##############################################################################
class Editor(AvnDialog.Dialog):
    AltSites = ['METAR', 'GFSLAMP', 'GFSMOS', 'ETAMOS', \
                'NGMMOS', 'ETA', 'Radars', 'Profilers', \
		'ACARS']
    def __init__(self, parent, **kw):
        AvnDialog.Dialog.__init__(self, parent, **kw)

        self.title('TAF Site Info Editor')
        self.wm_iconname(newName='SEditor')

        # from AvnDialog
        self.createMessageBar(self.interior(), True)

        interior = self.interior()
        frame = Frame(interior, relief='ridge', bd=2)
        self.site = Pmw.EntryField(frame,
                                   labelpos='w',
                                   label_text='Site Id',
                                   entry_width=4,
                                   validate={'validator': 'alphanumeric',
                                             'min': 4, 'max': 4, 'minstrict': 0},
                                   command=self.__load,
                                   )
        self.site.pack(side='left', expand='no', padx=5)
        bbox = Pmw.ButtonBox(frame)
        btn = bbox.add('Load', command=self.__load)
        Balloon().bind(btn, 'Retrieves data for selected site')
        btn = bbox.add('Update', command=self.__update)
        Balloon().bind(btn, 'Retrieves site info from fxa data files')
        btn = bbox.add('Save', command=self.__save)
        Balloon().bind(btn, 'Saves site info to a file')
        btn = bbox.add('Close', command=self.close)
        Balloon().bind(btn, 'Closes this dialog')
        btn = bbox.add('Help', command=Avn.curry(self.showHelp, _Help))
        Balloon().bind(btn, 'Shows help')
        bbox.alignbuttons()
        bbox.pack(side='right', expand='no')
        frame.pack(side='top', expand='yes', fill='x')

        frame = Frame(interior)
        headers = group = Pmw.Group(frame, tag_text='TAF Headers')
        self.wmo = Pmw.EntryField(group.interior(),
                                  labelpos='w',
                                  label_text='WMO',
                                  entry_width=12,
                                  validate=_validateWMO
                                  )
        self.wmo.grid(row=0, column=0, sticky='ew', padx=5)
        self.afos = Pmw.EntryField(group.interior(),
                                   labelpos='w',
                                   label_text='AFOS',
                                   entry_width=9,
                                   validate=_validateAFOS
                                   )
        self.afos.grid(row=0, column=1, sticky='ew', padx=5)
        group.pack(side='left', expand='yes', fill='x', pady=5)

        group = Pmw.Group(frame, tag_text='TAF Duration')
        self.tafDuration = Pmw.EntryField(group.interior(),
                                          labelpos='w',
                                          label_text='Hours',
                                          entry_width=2,
                                          validate=_validateTafduration)
        self.tafDuration.grid(row=0,column=1, sticky='ew', padx=5)
        group.pack(side='right', expand='yes', fill='x', pady=5)
        frame.pack(side='top', expand='yes', fill='x')

        group = Pmw.Group(interior, tag_text='Thresholds')
        self.vsby = Pmw.EntryField(group.interior(),
                                   labelpos='w',
                                   label_text='Visibility',
                                   entry_width=24,
                                   validate=_validateVsby,
                                   )
        self.vsby.grid(row=0, column=0, sticky='ew', padx=5)
        self.cig = Pmw.EntryField(group.interior(),
                                  labelpos='w',
                                  label_text='Ceiling',
                                  entry_width=24,
                                  validate=_validateCig,
                                  )
        self.cig.grid(row=0, column=1, sticky='ew', padx=5)
        self.radar_hgt = Pmw.EntryField(group.interior(),
                                        labelpos='w',
                                        label_text='Radar Cutoff',
                                        entry_width=24,
                                        validate=_validateHeight,
                                        )
        self.radar_hgt.grid(row=1, column=0, sticky='ew', padx=5)
        self.profiler_hgt = Pmw.EntryField(group.interior(),
                                           labelpos='w',
                                           label_text='Profiler Cutoff',
                                           entry_width=24,
                                           validate=_validateHeight,
                                           )
        self.profiler_hgt.grid(row=1, column=1, sticky='ew', padx=5)
        Pmw.alignlabels([self.vsby, self.cig, self.radar_hgt, \
                         self.profiler_hgt])
        group.pack(side='top', expand='yes', fill='x', pady=5)

        group = Pmw.Group(interior, tag_text='Geography')
        self.lat = Pmw.EntryField(group.interior(),
                                  labelpos='w',
                                  label_text='Latitude',
                                  entry_width=8,
                                  validate={'validator': 'real',
                                            'min': -90.0, 'max': 90.0},
                                  )
        self.lat.grid(row=0, column=0, sticky='ew', padx=5)
        self.lon = Pmw.EntryField(group.interior(),
                                  labelpos='w',
                                  label_text='Longitude',
                                  entry_width=8,
                                  validate={'validator': 'real',
                                            'min': -180.0, 'max': 180.0},
                                  )
        self.lon.grid(row=0, column=1, sticky='ew', padx=5)
        self.elev = Pmw.EntryField(group.interior(),
                                   labelpos='w',
                                   label_text='Elevation',
                                   entry_width=4,
                                   validate={'validator': 'integer'},
                                   )
        self.elev.grid(row=0, column=2, sticky='ew', padx=5)
        self.runway = Pmw.EntryField(group.interior(),
                                     labelpos='w',
                                     label_text='Runway(s)',
                                     entry_width=12,
                                     validate=_validateRunway,
                                     )
        self.runway.grid(row=0, column=3, sticky='ew', padx=5)
        group.pack(side='top', expand='yes', fill='x', pady=5)

        group = Pmw.Group(interior, tag_text='Alternate ids')
        self.ident = {}
        for n, tag in enumerate(self.AltSites):
            if tag == 'METAR':
                # to allow for multiple ids
                validator = _validateMetar
            elif tag == 'Radars':
                validator = _validateRadar
            elif tag == 'Profilers':
                validator = _validateProfiler
            else:
                validator = _validateOther
                
            row = n // 3; col = n % 3
            self.ident[tag] = Pmw.EntryField(group.interior(),
                                             labelpos='w',
                                             label_text=tag,
                                             entry_width=10,
                                             validate=validator,
                                             )
            self.ident[tag].grid(row=row, column=col, sticky='ew',
                                 padx=5)
            
        Pmw.alignlabels(self.ident.values())
        group.pack(side='top', expand='yes', fill='x', pady=5)

        group = Pmw.Group(interior, tag_text='QC Checks')
        self.impact = self.climate = self.currentwx = 1
	self.qcCheck = Pmw.RadioSelect(group.interior(), buttontype='checkbutton',
                                       orient='horizontal', command=self.__setQC)
	self.qcCheck.add("Impact")
	self.qcCheck.add("Climate")
	self.qcCheck.add("Current Wx")

        self.qcCheck.grid(row=0,column=0, sticky='news', padx=5)
        
        group.pack(side='top', expand='yes', fill='x', pady=5)
        frame.pack(side='top', expand='yes', fill='x')

        group = Pmw.Group(interior, tag_text='Templates')
        self._tkHour = StringVar()
        self._tkHour.set(_Issue_Hours[0])
        menu = Pmw.OptionMenu(group.interior(),
                              labelpos='w',
                              label_text='Issue',
                              menubutton_textvariable=self._tkHour,
                              items=tuple(_Issue_Hours),
                              )
        menu.grid(row=0, column=0, sticky='news', padx=10)
        bbox = Pmw.ButtonBox(group.interior())
        btn = bbox.add('Edit', command=self.__editTemplate)
        Balloon().bind(btn, 'Template editor')
        btn = bbox.add('Make', command=self.__makeTemplates)
        Balloon().bind(btn, 'Makes default templates')
        bbox.alignbuttons()
        bbox.grid(row=0, column=1, sticky='news')
        group.pack(side='left', expand='yes', fill='x', pady=5)

        self.initialiseoptions(Editor)
        self.setGeometry()

    def __clear(self):
        for name in ['wmo', 'afos', 'vsby', 'cig', 'radar_hgt', \
                     'profiler_hgt', 'lat', 'lon', 'elev', 'runway']:
            getattr(self, name).clear() 
        self.runway.clear()
        for tag in self.AltSites:
            self.ident[tag].clear()

    def __validate(self):
        if not self.site.valid():
            msg = 'Invalid site id'
            self.messagebar().message('usererror', msg)
            return
        for name, label in [('wmo', 'TAF WMO'), ('afos', 'TAF AFOS'), \
                            ('vsby', 'Visibility'), ('cig', 'Ceiling'), \
                            ('radar_hgt', 'Radar Cutoff'), \
                            ('profiler_hgt', 'Profiler Cutoff'), \
                            ('lat', 'Latitude'), ('lon', 'Longitude'), \
                            ('elev', 'Elevation'), ('runway', 'Runway(s)')]:
            if not getattr(self, name).valid():
                msg = 'Invalid %s entry' % label
                self.messagebar().message('usererror', msg)
                return False
            
        for tag in self.AltSites:
            if not self.ident[tag].valid():
                msg = 'Invalid %s entry' % tag
                self.messagebar().message('usererror', msg)
                return False
        # check for number of radar and profiler heights
        num_radars = len(AvnParser.split(self.ident['Radars'].getvalue()))
        num_heights = len(AvnParser.split(self.radar_hgt.getvalue()))
        if num_radars != num_heights:
            msg = 'Invalid number of Radar Cutoff heights'
            self.messagebar().message('usererror', msg)
            return False
        num_profilers = len(AvnParser.split(self.ident['Profilers'].getvalue()))
        num_heights = len(AvnParser.split(self.profiler_hgt.getvalue()))
        if num_profilers != num_heights:
            msg = 'Invalid number of Profiler Cutoff heights'
            self.messagebar().message('usererror', msg)
            return False
        return True

    def __setQC(self, tag, state):
	if tag == 'Impact':
            self.impact = state
	elif tag == 'Climate':
            self.climate = state
	elif tag == 'Current Wx':
            self.currentwx = state
	return

    def __load(self):
        self.messagebar().resetmessages('systemerror')
        if not self.site.valid():
            msg = 'Invalid site id'
            self.messagebar().message('usererror', msg)
            return
        ident = self.site.getvalue().upper()
        cfg = AvnParser.getTafSiteCfg(ident)
        if cfg is None:
            self.__clear()
            msg = 'Failed to retrieve configuration for %s' % ident
            self.messagebar().message('systemerror', msg)
            return
        
        self.wmo.setvalue(cfg['headers']['wmo'])
        self.afos.setvalue(cfg['headers']['afos'])
        
        self.vsby.setvalue(','.join([str(x) for x in \
                                     cfg['thresholds']['vsby']]))
        self.cig.setvalue(','.join([str(x) for x in \
                                    cfg['thresholds']['cig']]))
        self.radar_hgt.setvalue(','.join([str(x) for x in \
                                          cfg['thresholds']['radar_cutoff']]))
        self.profiler_hgt.setvalue(','.join([str(x) for x in \
                                             cfg['thresholds']['profiler_cutoff']]))
        self.tafDuration.setvalue(cfg['thresholds']['tafduration'])

        self.lat.setvalue(cfg['geography']['lat']) 
        self.lon.setvalue(cfg['geography']['lon']) 
        self.elev.setvalue(cfg['geography']['elev']) 
        self.runway.setvalue(','.join([str(x) for x in \
                                       cfg['geography']['runway']+['']]))
	
        for tag in self.AltSites:
            try:
                self.ident[tag].setvalue(','.join(cfg['sites'][tag.lower()]))
            except KeyError:
                if tag not in ['Radars', 'Profilers', 'ACARS']:
                    msg = 'Missing entry %s for %s' % (tag, ident)
                    self.messagebar().message('systemerror', msg)
                    _Logger.error(msg) 

	for btn, label in [('Impact','impact'),('Climate','climate'),('Current Wx','currentwx')]:
	    try:
                self.qcCheck.button(btn).select()
		if cfg['qc'][label] == 0:
                    self.qcCheck.button(btn).deselect()
                    
	    except KeyError:
                self.qcCheck.button(btn).select()
		continue
            
	    self.__setQC(label, cfg['qc'][label])
        
    def __update(self):
        self.messagebar().resetmessages('systemerror')
        if not self.site.valid():
            msg = 'Invalid site id'
            self.messagebar().message('usererror', msg)
            return
        ident = self.site.getvalue().upper()
        hdr = getHeaders('TAF', ident)
        if hdr:
            self.afos.setvalue(hdr[0])
            self.wmo.setvalue(hdr[1])
        else:
            msg = 'Failed to find headers for %s' % ident
            self.messagebar().message('systemerror', msg)
            _Logger.exception(msg)
            
        self.tafDuration.setvalue('24')
        self.vsby.setvalue('0.5,1.0,2.0,3.0,6.0')
        self.cig.setvalue('200,600,1000,2000,3100')
        
        geo = getGeography(ident)
        if geo:
            self.lat.setvalue(geo[0])
            self.lon.setvalue(geo[1])
            self.elev.setvalue(geo[2])
        else:
            msg = 'Failed to find geography for %s' % ident
            self.messagebar().message('systemerror', msg)
            _Logger.exception(msg)
        self.runway.setvalue('0,')
        # no radars and profilers here
        for tag in ['ACARS','METAR', 'ETA', 'ETAMOS', 'NGMMOS', 'GFSMOS', 'GFSLAMP']:
            if tag in ['ACARS', 'NGMMOS']:
                self.ident[tag].setvalue(ident[1:])
            else:
                self.ident[tag].setvalue(ident)

    def __save(self):
        self.messagebar().resetmessages('systemerror')
        if not self.__validate():
            return
        ident = self.site.getvalue().upper()
        path = os.path.join('etc', 'tafs', ident, 'info.cfg')
        try:
            cp = ConfigParser.ConfigParser()
            cp.add_section('headers')
            wmo = self.wmo.getvalue().strip().upper()
            cp.set('headers', 'wmo', wmo)
            afos = self.afos.getvalue().strip().upper()
            cp.set('headers', 'afos', afos)

            cp.add_section('thresholds')
            vsby = self.vsby.getvalue().strip()
            cp.set('thresholds', 'vsby', vsby)
            cig = self.cig.getvalue().strip()
            cp.set('thresholds', 'cig', cig)
            hgts = self.radar_hgt.getvalue().strip()
            cp.set('thresholds', 'radar_cutoff', hgts)
            hgts = self.profiler_hgt.getvalue().strip()
            cp.set('thresholds', 'profiler_cutoff', hgts)
            hours = self.tafDuration.getvalue().strip()
            cp.set('thresholds', 'tafduration', hours)
            
            cp.add_section('geography')
            lat = self.lat.getvalue().strip()
            cp.set('geography', 'lat', lat)
            lon = self.lon.getvalue().strip()
            cp.set('geography', 'lon', lon)
            elev = self.elev.getvalue().strip()
            cp.set('geography', 'elev', elev)
            runway = self.runway.getvalue().strip()
            cp.set('geography', 'runway', runway)

	    cp.add_section('qc')
	    cp.set('qc','impact',self.impact)
	    cp.set('qc','climate',self.climate)
	    cp.set('qc','currentwx',self.currentwx)

            cp.add_section('sites')
            for tag in self.AltSites:
                if tag != 'ACARS':
                    i = self.ident[tag].getvalue().strip().upper()
                else:
                    i = self.ident[tag].getvalue().strip()                   
                cp.set('sites', tag, i)
                
            try:
                if not os.path.exists(os.path.dirname(path)):
                    os.mkdir(os.path.dirname(path))
                    
                fp = file(path, 'w')
                fp.write('# %s\n\n' % path) 
                cp.write(fp)
                
            finally:
                fp.close()
                
            msg = 'Updated %s' % path
            self.messagebar().message('systemevent', msg)
            
        except Exception, e:
            msg = 'Failed to update %s' % path
            _Logger.exception(msg)
            self.messagebar().message('systemerror', msg)

    def __makeTemplates(self):
        self.messagebar().resetmessages('systemerror')
        if not self.site.valid():
            msg = 'Invalid site id'
            self.messagebar().message('usererror', msg)
            return
        
        id_ = self.site.getvalue().upper()
        duration=24
        if self.tafDuration.valid():
            duration=int(self.tafDuration.getvalue())
        f = []
        for starthour in [int(x[:2]) for x in _Issue_Hours]:
            path = os.path.join('etc', 'tafs', id_, '%02d.template' % starthour)
            if os.path.exists(path):
                continue
            #
            endhour=(starthour+duration)%24
            if endhour == 0:
                endhour = 24
                
            if Avn.DTGImplementationSwitch():    
                line='%s DD%02d00Z D1%02dD2%02d =\n' % (id_, starthour, starthour,
                                                        endhour)
            else:
                line='%s DD%02d00Z DD%02d%02d =\n' % (id_, starthour, starthour,
                                                      endhour)
            try:
                if not os.path.exists(os.path.dirname(path)):
                    os.mkdir(os.path.dirname(path))
                file(path, 'w').write(line)
                f.append(starthour)
            except IOError:
                msg = 'Cannot make template %s' % path
                _Logger.exception(msg)
                self.messagebar().message('systemerror', msg)
        if f:
            msg = 'Template files created'
            self.messagebar().message('userevent', msg)

    def __editTemplate(self):
        if not hasattr(self, '_textEditor'):
            self._textEditor = EditDialog.Editor(self.interior())
        if not self.site.valid():
            msg = 'Invalid site id'
            self.messagebar().message('usererror', msg)
            return
        ident = self.site.getvalue().upper()
        hour = int(self._tkHour.get()[:2])
        path = os.path.join('etc', 'tafs', ident, '%02d.template' % hour)
        if os.path.exists(path):
            self._textEditor.getFile(path)
        self._textEditor.setworkdir(os.path.dirname(path))
        self._textEditor.display()

