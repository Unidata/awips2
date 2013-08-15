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
#       MetarMonitor.py
#       GFS1-NHD:A6636.0000-SCRIPT;1.26
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.26 (DELIVERED)
#         Created:  27-SEP-2009 22:55:47      OBERFIEL
#           Last bit of heart surgery. TEMPO groups evaluated more
#           frequently.
#       
#       Revision 1.25 (DELIVERED)
#         Created:  20-AUG-2009 16:18:22      OBERFIEL
#           Change for sake of making a change.
#       
#       Revision 1.24 (DELIVERED)
#         Created:  17-APR-2009 12:07:51      OBERFIEL
#           Now handles (ignores) AvnUnknwnPcp exceptions when checking
#           TEMPO groups
#       
#       Revision 1.23 (DELIVERED)
#         Created:  01-AUG-2008 15:44:47      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 1.22 (DELIVERED)
#         Created:  28-JUL-2008 13:57:54      OBERFIEL
#           Removed AFG specific feature.  Replaced by the
#           AirportOpsThresh rule.
#
#       Revision 1.21 (DELIVERED)
#         Created:  06-FEB-2008 08:52:29      GILMOREDM
#       
#       Revision 1.20 (DELIVERED)
#         Created:  03-OCT-2007 13:57:56      OBERFIEL
#           Added message when TPO is purple to include mention of
#       
#       Revision 1.19 (DELIVERED)
#         Created:  16-MAY-2006 10:50:31      TROJAN
#           spr 7146: added history button in TWEB Editor's statusbar,
#           fixed spelling
#       
#       Revision 1.18 (DELIVERED)
#         Created:  04-MAY-2006 14:16:16      TROJAN
#           SPR7125: fixed weather check in TafQC, changes to
#           checkTEMPO() in MetarMonitor
#       
#       Revision 1.17 (DELIVERED)
#         Created:  04-MAY-2006 14:02:08      TROJAN
#           SPR 7126: fixed weather check in TafQC, changes to
#           checkTEMPO() in MetarMonitor
#
#       Revision 1.16 (DELIVERED)
#         Created:  23-APR-2006 11:54:00      TROJAN
#           spr 7125 - changes to TEMPO and category monitoring
#       
#       Revision 1.15 (DELIVERED)
#         Created:  23-APR-2006 11:45:01      TROJAN
#           spr 7126 - fix to __checkTEMPO()
#       
#       Revision 1.14 (DELIVERED)
#         Created:  23-APR-2006 10:52:20      TROJAN
#           spr 7126 - changes to tempo and category alerts
#       
#       Revision 1.13 (DELIVERED)
#         Created:  21-APR-2006 11:29:24      TROJAN
#           spr 7124: added exception catching code ro compare()
#       
#       Revision 1.12 (APPROVED)
#         Created:  20-APR-2006 15:47:32      TROJAN
#           made compare() wrapper in base class handling exceptions
#           renamed compare() to __compare()
#       
#       Revision 1.11 (DELIVERED)
#         Created:  23-JAN-2006 08:23:15      TROJAN
#           stdr 956
#       
#       Revision 1.10 (DELIVERED)
#         Created:  06-JUL-2005 18:16:40      TROJAN
#           spr 6548
#
#       Revision 1.9 (DELIVERED)
#         Created:  07-MAY-2005 11:35:41      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.8 (DELIVERED)
#         Created:  04-APR-2005 15:51:08      TROJAN
#           spr 6775
#       
#       Revision 1.7 (DELIVERED)
#         Created:  28-FEB-2005 21:37:47      TROJAN
#           spr 6686
#       
#       Revision 1.6 (DELIVERED)
#         Created:  14-FEB-2005 20:54:50      TROJAN
#           spr 6649
#       
#       Revision 1.5 (APPROVED)
#         Created:  30-SEP-2004 18:56:03      TROJAN
#           stdr 874
#       
#       Revision 1.4 (APPROVED)
#         Created:  19-AUG-2004 20:51:36      OBERFIEL
#           Change code
#       
#       Revision 1.3 (APPROVED)
#         Created:  01-JUL-2004 14:59:41      OBERFIEL
#           Update
#
#       Revision 1.2 (DELIVERED)
#         Created:  08-JAN-2004 21:40:10      PCMS
#           Updating for code cleanup
#       
#       Revision 1.1 (APPROVED)
#         Created:  06-NOV-2003 16:45:55      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#               Change Document:   GFS1-NHD_SPR_7429
#               Action Date:       02-OCT-2009 15:11:26
#               Relationship Type: In Response to
#               Status:           TEST
#               Title:             AvnFPS: Unable to properly set QC functions in Site Info Editor
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06JUL2012       15153         zhao           Retrieve latest METAR record in database
#
import logging, os, time
import Avn, AvnLib, Globals, MonitorP, MetarMonitorP

_Logger = logging.getLogger(Avn.CATEGORY)
import MetarData

###############################################################################
class Monitor(MetarMonitorP.Monitor):
    Source = 'mtrs'

    def __checkTEMPO(self, tempo, mtrs):
        """Checks whether TEMPO rules verify at least one METAR"""
        d = {}
        for rule in self.rules:
            for mtr in mtrs:
                try:
                    if rule.severity <= self.args.get('tempolevel', 2) or \
                           not rule.method(tempo, AvnLib.makeMetarData(mtr)):
                        break
                except (Avn.AvnMissing,Avn.AvnUnknwnPcp):
                    break
            else:
                if rule.type != 'wx' or 'wx' in tempo:
                    d[rule.type] = 1
        return d

    def __compare(self, taf):
        now = time.time()
#        metars = Globals.DRC.getMetars(self.info['sites']['metar'], True,
#                                       now-15000.0)

# For DR15153: use 'maxSize=0' to indicate that the latest record is to be retrieved  
        metars = MetarData.retrieve(self.info['sites']['metar'],0) 
        msg = None
        result = {}
        if not metars:
            msg = 'Missing METAR'
            _Logger.warning('%s for %s', msg, self.info['sites']['metar'])
        else:
            rpt = metars[0]
            result['header'] = rpt.header
            result['text'] = rpt.text
            if 'fatal' in rpt.dcd:
                msg = 'Cannot decode METAR'
                _Logger.error('%s for %s:\n%s', msg,
                    self.info['sites']['metar'], rpt.text)
        if msg:
            result['status'] = self.setMissing(msg)
            return result

        result['dcd'] = rpt.dcd
        try:
            delta = MonitorP.applyRules(self.rules, now, taf.hourly.get(now),
                                        AvnLib.makeMetarData(rpt.dcd))
            result['status'] = MonitorP.addMessages(self.args['items'][1:],
                                                    MonitorP.addRules(self.args['items'][1:],
                                                                      self.rules, delta))
        except IndexError:
            result['status'] = self.setNIL()
            return result
        #
        # Begin check on TEMPO conditions
        for g in taf.dcd['group']:
            if g['prev']['time']['from'] <= now < g['prev']['time']['to']:
                break

        result['status']['tempo'] = Avn.Bunch(severity=0, msg='OK')
        #
        # If a TEMPO group is still in effect...
        if 'ocnl' in g and g['ocnl']['type'] == 'TEMPO' and now < g['ocnl']['time']['to']:
            tf = g['ocnl']['time']['from']
            halflife = (g['ocnl']['time']['to'] - g['ocnl']['time']['from'])/2.0
            delta = now-tf
            #
            # If an hour has passed or more than half of the TEMPO group valid time...
            if delta >= 3600. or delta >= halflife:
                tempo = AvnLib.TafData.makeTempo(g)
                if not tempo:
                    return result

                tmpdcd = [m.dcd for m in metars if 'fatal' not in m.dcd]
                dt = now
                delta = 0
                tdict = {}
                #
                # Generally 30 up to 90 minutes makes sense.
                tempograceperiod = min(max(int(self.args.get('tempograceperiod','3600')),\
                                           1800),5400)
                while (dt > tf):
                    dt -= tempograceperiod
                    #
                    # select observations that fall within grace period window
                    dcds = [d2 for d1, d2 in Avn.window(tmpdcd) if d1['itime']['value'] > dt]
                    try:
                        dcds.insert(0,tmpdcd[0])
                    except IndexError:
                        break
                    #
                    d = self.__checkTEMPO(tempo, dcds)
                    if d:
                        tdict.update(d)
                        delta = now - max(tf,dcds[-1]['itime']['value'])
                        #
                        # Go back further in time for more non-TEMPO events
                        try:
                            while(dcds.pop()):
                                tmpdcd.pop(0)
                        except IndexError:
                            pass
                    else:
                        break
                #
                # If TEMPO events not found in the recent past...
                if tdict:
                    events = [self.args['labels'].get(x, x) for x in tdict.keys()]
                    if delta <= 3630.0:
                        msg = '%s events did not occur for %02.0f mins' % \
                              (' '.join(events), delta/60.0)
                    else:
                        msg = '%s events did not occur for %dh %02.0fm' % \
                              (' '.join(events),delta//3600,(delta/60)%60)

                    if delta >= halflife:  # more than half of valid time
                        s = 4  # Orange
                    else:
                        s = 3  # Yellow

                    result['status']['tempo'] = Avn.Bunch(severity=s, msg=msg)
        return result
    