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
#       CCFPMonitor.py
#       GFS1-NHD:A8924.0000-SCRIPT;6
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 6 (DELIVERED)
#         Created:  24-AUG-2009 14:35:19      OBERFIEL
#           Added code to support "CB not in TAF" when evaluating CCFP
#           guidance.
#       
#       Revision 5 (REVIEW)
#         Created:  21-APR-2009 08:09:09      OBERFIEL
#           Corrected logic in method() call -- 'and' instead of 'or'
#       
#       Revision 4 (DELIVERED)
#         Created:  21-APR-2006 11:29:20      TROJAN
#           spr 7124: added exception catching code ro compare()
#       
#       Revision 3 (DELIVERED)
#         Created:  20-APR-2006 15:47:29      TROJAN
#           made compare() wrapper in base class handling exceptions
#           renamed compare() to __compare()
#       
#       Revision 2 (DELIVERED)
#         Created:  23-JAN-2006 08:23:12      TROJAN
#           stdr 956
#       
#       Revision 1 (DELIVERED)
#         Created:  07-SEP-2005 13:02:41      TROJAN
#           spr 7010
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7428
#       	Action Date:       31-AUG-2009 16:40:04
#       	Relationship Type: In Response to
#       	Status:           TEST
#       	Title:             AvnFPS: tpo indicator not monitoring properly
#    
#    Date             Ticket#       Engineer       Description
#    -------------    ----------    -----------    --------------------------
#    Feb. 21, 2013    15834         zhao           Modified for CCFP 8hr data
#    Jan. 14  2014    16289         zhao           Modified time format in _makeData() to match A1
#
import logging, time
import Avn, AvnLib, Globals, MonitorP
import CCFPData

_Logger = logging.getLogger(__name__)

_Code = { \
    'tops': {1: '400+   ', 2: '350-390', 3: '300-340', 4: '250-290'}, \
    'gwth': {1: '+ ', 2: 'NC', 3: '- '}, \
    'conf': {1: 'HIGH', 3: 'LOW'}, \
    'cvrg': {1: '75-100%', 2: ' 40-74%', 3: ' 25-39%'}, \
    }

##############################################################################
class Monitor(MonitorP.Monitor):
    Source = 'ccfp'
    Namespace = globals()
    
    def __makeData(self, data):
        # 6 hour forecast
        tstart = (time.time()//3600.0 + 1) * 3600.0
        tend = tstart + 9*3600.0 - 10.0
        seq = [{'time': t} for t in Avn.frange(tstart, tend, 3600.0)]
        fcst, text = {}, []
        try:
            for line in [d.text for d in data]:
                tok = line.split()
                if tok:
                    vtime = Avn.string2time(tok[1]) # tok[0] is site id
                    tmp = [(x.lower(), int(y)) for (x, y) in Avn.pairs(tok[2:])]
                    fcst[vtime] = d = dict(tmp)
                    ttok = ['%s: %s' % (x.upper(), _Code[x].get(int(d[x]), ''))\
                            for x in d]
                    text.append('%sZ %s' % (tok[1][6:-2], ' '.join(ttok)))
            if not text:
                text.append('NIL CONVECTION')
            for s in seq:
                s.update(fcst.get(s['time'], {}))
            return {'hourly': seq, 'text': text}
        except Exception:
            _Logger.exception('Failure parsing %s', data)
            return None

    def __compare(self, taf):
        now = time.time()
        # data is a sequence of up to 3 forecasts
        #data = Globals.DRC.getCCFP(self.info['ident'], now-18000.0)
        data = CCFPData.retrieve(self.info['ident'])        
        msg = None
        result = {}
        if not data:
            msg = 'Missing ccfp forecast'
        else:   
            fcst = self.__makeData(data)
            if fcst is None:
                msg = 'Cannot make forecast'
                _Logger.warning('%s for %s' % (msg, self.info['ident']))
            else:
                result['text'] = '\n'.join(fcst['text'])
        if msg:
            result['status'] = self.setMissing(msg)
            _Logger.warning('%s for %s' % (msg, self.info['ident']))
            return result
        try:
            delta_h = {}
            for n, data in enumerate(fcst['hourly']):
                t = data['time']
                delta_h[n] = MonitorP.applyRules(self.rules, t, 
                                                 taf.hourly.get(t), data)
            delta = delta_h[0][:]
            for m in range(len(delta)):
                if delta[m] != 0:   # pick first occurence
                    continue
                for n in range(1, len(fcst['hourly'])):
                    if delta_h[n][m] != 0:
                        delta[m] = delta_h[n][m]
                        break
            tmp = MonitorP.addRules(self.args['items'], self.rules, delta)
            result['status'] = MonitorP.addMessages(self.args['items'], tmp)
        except IndexError:
            _Logger.exception(self.info['ident'])
            result['status'] = self.setNIL()
        return result

###############################################################################
# Section containing editable rules
##############################################################################
class TSNotInTaf(MonitorP.Rule):
    """CCFP meets confidence and coverage thresholds with no TS in TAF.
Arguments: 
    CVRG (1: High, 2: Medium, 3: Low)
    CONF (1: High, 3: Low)"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wx'
        self.unique = True
        self.severity = 3
        self.args = {'conf': 1, 'cvrg': 2}
    
    def method(self, taf, ccfp):
        if 'ts' in taf:
            return False
        if ccfp.get('conf', 99) == self.args['conf'] and \
               ccfp.get('cvrg', 99) == self.args['cvrg']:
            self.setmsg('CCFP meets confidence and coverage thresholds' \
                        ' with no TS in TAF')
            return True
        return False

class CBNotInTaf(MonitorP.Rule):
    """CCFP meets confidence and coverage thresholds with no CB in TAF.
Arguments: 
    CVRG (1: High, 2: Medium, 3: Low)
    CONF (1: High, 3: Low)"""
    def __init__(self):
        # defaults, can be overwritten by configuration file
        MonitorP.Rule.__init__(self)
        self.type = 'wx'
        self.unique = True
        self.severity = 4
        self.args = {'conf': 3, 'cvrg': 3}
    
    def method(self, taf, ccfp):
        if 'cb' in taf:
            return False
        if ccfp.get('conf', 99) == self.args['conf'] and \
               ccfp.get('cvrg', 99) == self.args['cvrg']:
            self.setmsg('CCFP meets confidence and coverage thresholds' \
                        ' with no CB in TAF')
            return True
        return False
