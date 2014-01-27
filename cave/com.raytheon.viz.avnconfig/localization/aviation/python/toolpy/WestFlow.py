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
#       WestFlow.py
#       GFS1-NHD:A6832.0000-SCRIPT;6
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 6 (DELIVERED)
#         Created:  26-MAR-2009 20:22:55      OBERFIEL
#           Restored function.  Simplified dictionary key.
#       
#       Revision 5 (DELIVERED)
#         Created:  04-AUG-2005 15:27:03      TROJAN
#           spr 6962, 6963
#       
#       Revision 4 (DELIVERED)
#         Created:  24-JAN-2005 18:48:55      TROJAN
#           spr 6609
#       
#       Revision 3 (APPROVED)
#         Created:  01-OCT-2004 14:36:20      TROJAN
#           stdr 862
#       
#       Revision 2 (APPROVED)
#         Created:  01-JUL-2004 15:18:25      OBERFIEL
#           Update
#       
#       Revision 1 (DELIVERED)
#         Created:  08-JAN-2004 21:23:40      PCMS
#           Initial version
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7409
#       	Action Date:       15-AUG-2009 20:19:42
#       	Relationship Type: In Response to
#       	Status:           TEST
#       	Title:             AvnFPS: TAF No Significant Weather (NSW) not QC'd correctly
#       
#
# Purpose:
#
#    An example of a TAF modification tool this tool adjusts sky
# and visibility conditions at KCKB and KEKN based on forecast for
# KPKB, and KCRW and KBKW based on forecast for KHTS time periods
# within first 12 hours of the forecast are shifted depending on the
# site: 1hr for KCKB and KCRW, 2 hours for KEKN, 3 hours for KBKW.
#
import copy, time, logging
from Tkinter import *
import Tkinter
import Pmw
import Avn, AvnLib, Busy, TafDecoder

_Logger = logging.getLogger(Avn.CATEGORY)

###############################################################################
# private area
###############################################################################
def _fixObvis(v, obvis):
    if v < 0.7:
        obvis['str'] = obvis['str'].replace('BR', 'FG')
    else:
        obvis['str'] = obvis['str'].replace('FG', 'BR')

def _nil(arg):
    pass

# shifts forecast by 'shift' hours
def _shiftTaf(groups, shift=0, modifyWx=_nil):
    shift *= 3600.0
    endtime = groups[-1]['prev']['time']['to']
    tmpg = [g for g in groups if g['prev']['time']['from'] < endtime-shift]
    for g in tmpg[1:]:
        gp = g['prev']
        gp['time']['to'] += shift
        gp['time']['from'] += shift
        modifyWx(gp)
        if 'ocnl' in g:
            og = g['ocnl']
            og['time']['from'] += shift
            og['time']['to'] += shift
            modifyWx(og)
    g = tmpg[-1]
    g['prev']['time']['to'] = endtime
    if 'ocnl' in g:
        if endtime - g['ocnl']['time']['from'] < 3600.0:
            del g['ocnl']
        elif g['ocnl']['time']['to'] > endtime:
            g['ocnl']['time']['to'] = endtime
    return tmpg

def _shiftSky(cig, sky):
    # separate case for vertical visibility
    if 'VV' in sky['str']:
        sky['str'] = 'VV%03d' % cig
        sky['cig'] = cig * 100
        return
    skylist = [(x[:3], int(x[3:6]), x[6:]) for x in sky['str'].split()]
    # find index of the lowest BKN/OVC cloud layer
    h = sky['cig']//100
    for n, s in enumerate(skylist):
        if s[1] == h:
            ix = n
            break
    else:
        # this should not happen
        raise Avn.AvnError, 'Possible bug in _shiftSky()'
    # preserve cloud layers below the new ceiling
    cld = ['%s%03d%s' % x for x in skylist if x[1] < cig]
    # add new ceiling
    c, h, cb = skylist[ix]
    cld.append('%s%03d%s' % (c, cig, cb))
    # preserve cloud layers above the old ceiling
    cld.extend(['%s%03d%s' % x for x in skylist if x[1] > cig])
    sky['str'] = ' '.join(cld)
    sky['cig'] = cig * 100

# forecast modification functions
def _updateKCKB(g):
    if not 'sky' in g:
        return
    cig = g['sky']['cig'] / 100
    if cig > 50:
        return
    if cig > 10:
        cig -= 5
    elif cig > 2:
        cig -= 1
    newcig = AvnLib.fixCldBase(cig)
    _shiftSky(newcig, g['sky'])
    if not 'vsby' in g:
        return
    v = g['vsby']['value']
    if v < 4.0:
        v -= 0.5
    if v < 0.0:
        v = 0.0
    g['vsby'] = AvnLib.fixTafVsby(v)
    if 'obvis' in g:
        _fixObvis(v, g['obvis'])

def _updateKEKN(g):
    if not 'sky' in g:
        return
    cig = g['sky']['cig'] / 100
    if cig > 50:
        return
    if cig > 10:
        cig -= 10
    elif cig > 5:
        cig -= 4
    newcig = AvnLib.fixCldBase(cig)
    _shiftSky(newcig, g['sky'])
    if not 'vsby' in g:
        return
    v = g['vsby']['value']
    if v < 6.1:
        v -= 1.5
    if v < 0.0:
        v = 0.0
    g['vsby'] = AvnLib.fixTafVsby(v)
    if 'obvis' in g:
        _fixObvis(v, g['obvis'])

def _updateKCRW(g):
    if not 'vsby' in g:
        return
    v = g['vsby']['value']
    if v < 4.0:
        v -= 0.5
    if v < 0.0:
        v = 0.0
    g['vsby'] = AvnLib.fixTafVsby(v)
    if 'obvis' in g:
        _fixObvis(v, g['obvis'])

def _updateKBKW(g):
    if not 'sky' in g:
        return
    cig = g['sky']['cig'] / 100
    if cig > 50:
        return
    if cig > 10:
        cig -= 10
    elif cig > 5:
        cig -= 4
    newcig = AvnLib.fixCldBase(cig)
    _shiftSky(newcig, g['sky'])
    if not 'vsby' in g:
        return
    v = g['vsby']['value']
    if v < 4.0:
        v -= 0.5
    if v < 0.0:
        v = 0.0
    g['vsby'] = AvnLib.fixTafVsby(v)
    if 'obvis' in g:
        _fixObvis(v, g['obvis'])

###############################################################################
_updateWx = {'KCKB': _updateKCKB, 'KEKN': _updateKEKN, 'KCRW': _updateKCRW, \
    'KBKW': _updateKBKW}

# number of hours to shift forecasts for KPKB nad KHTS
# the actual value is multiplied by user specified value
_shift = {'KCKB': 1, 'KEKN': 2, 'KCRW': 1, 'KBKW': 3}

def _updateForecast(ident, shift, bbb, srcgroups):
    shift *= _shift[ident]
    f = _updateWx[ident]
    groups = copy.deepcopy(srcgroups)
    groups = _shiftTaf(groups, shift, f)
    lines = AvnLib.makeTafFromPeriods(ident, bbb, groups)
    return '\n'.join(AvnLib.indentTaf(lines)+[''])

class Gui(Toplevel):
    def __init__(self, bbb, fcsts):
        master = Tkinter.Tk()
        self.master = master
        self.bbb = bbb
        self.fcsts = fcsts

        self.master.title('AvnFPS Tool: WestFlow')
        body = Frame(master)
        label = Label(body, text='If forecasts for KPKB and KHTS are ready\nenter delay between KPKB and KCKB')
        label.pack(side='top', pady=5)
        self.counter = Pmw.Counter(body,
            entryfield_value=1,
            entryfield_validate={'validator': 'numeric', 'min': 0, 'max': 3},
            entry_width=1,
            )
        self.counter.pack(side='top')
        bbox = Pmw.ButtonBox(body)
        bbox.add('OK', command=self.ok)
        bbox.add('Cancel', command=self.cancel)
        bbox.alignbuttons()
        bbox.pack(side='top')
        body.pack(padx=5, pady=5, expand='y', fill='both')
    
        self._taf_decoder = TafDecoder.Decoder()
        
        master.grab_set()
        master.protocol("WM_DELETE_WINDOW", self.cancel)
        if master is not None:
            master.geometry("+%d+%d" % (master.winfo_rootx()+50,
                master.winfo_rooty()+50))
        master.focus_set()
        master.wait_window(master)

    def updateForecasts(self):
        v = self.counter.get()
        if not self.counter.valid():
            #Busy.showerror('Invalid shift: %s hours' % shift, self.dialog)
            return 0
        shift = int(v)
        taf = self._taf_decoder(self.fcsts['KPKB'], self.bbb)
        if not taf['group']:
            #Busy.showerror('NIL TAF', self.dialog)
            return 0
        for ident in ['KCKB', 'KEKN']:
            if not ident in self.fcsts:
                continue
            self.fcsts[ident] = _updateForecast(ident, shift, self.bbb, 
                taf['group'])
        taf = self._taf_decoder(self.fcsts['KPKB'], self.bbb)
        if not taf['group']:
            #Busy.showerror('NIL TAF', self.dialog)
            return 0
        for ident in ['KCRW', 'KBKW']:
            if not ident in self.fcsts:
                continue
            self.fcsts[ident] = _updateForecast(ident, shift, self.bbb, 
                taf['group'])
        for ident in ['KPKB', 'KHTS']:
            self.fcsts[ident] = self.fcsts[ident] + '=\n'
        return 1

    def _close(self):
        self.master.withdraw()
        self.master.update_idletasks()
        if self.master is not None:
            self.master.focus_set()
            self.master.destroy()

    def ok(self, event=None):
        if not self.updateForecasts():
            return
        self._close()

    def cancel(self, event=None):
        self.fcsts = None
        self._close()

###############################################################################
def updateTafs(bbb, fcsts):
    # master is the parent widget, needed to create GUI
    # fcsts is a dictionary of forecasts displayed in the editor window
    # the function returns dictionary of modified forecasts
    # may raise AvnError
    for ident in ['KPKB', 'KHTS']:
        if not ident in fcsts:
            msg = 'Missing forecast for %s' % ident
            _Logger.exception(msg)
            return None
    return Gui(bbb, fcsts).fcsts
