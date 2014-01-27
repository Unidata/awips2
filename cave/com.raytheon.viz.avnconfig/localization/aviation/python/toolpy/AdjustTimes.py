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
#       AdjustTimes.py
#       GFS1-NHD:A6829.0000-SCRIPT;13
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 13 (DELIVERED)
#         Created:  16-JUN-2009 14:28:12      OBERFIEL
#           If tafduration cannot be determined from the info.cfg file,
#           default to 24 hours
#       
#       Revision 12 (DELIVERED)
#         Created:  03-APR-2009 14:40:46      OBERFIEL
#           Added code to manage time references in AMD NOT SKED or AMD
#           LTD TO statements, if present.
#           Inattention to this causes a lot of errors in the TAF
#           Decoder.
#       
#       Revision 11 (REVIEW)
#         Created:  26-MAR-2009 20:25:24      OBERFIEL
#           Simplified keys for TAF dictionary.  Eliminated obsolete
#           argument to updateTafs function.
#       
#       Revision 10 (DELIVERED)
#         Created:  31-DEC-2008 10:14:27      OBERFIEL
#           Changes to support amending TAFs prior to valid period.
#       
#       Revision 9 (DELIVERED)
#         Created:  02-SEP-2008 13:08:49      OBERFIEL
#           Updated rule to account for 30-h length TAF and allow COR
#           on the first line
#       
#       Revision 8 (DELIVERED)
#         Created:  01-AUG-2008 15:44:45      OBERFIEL
#           Synch'd up with changes in OB8.3.1
#       
#       Revision 7 (DELIVERED)
#         Created:  19-JUN-2008 14:22:37      OBERFIEL
#           Allowed to pass tafduration for each TAF being adjusted.
#       
#       Revision 6 (DELIVERED)
#         Created:  09-SEP-2005 14:04:17      TROJAN
#           spr 7011
#       
#       Revision 5 (DELIVERED)
#         Created:  04-AUG-2005 15:27:02      TROJAN
#           spr 6962, 6963
#       
#       Revision 4 (DELIVERED)
#         Created:  24-JAN-2005 18:48:55      TROJAN
#           spr 6609
#       
#       Revision 3 (APPROVED)
#         Created:  01-OCT-2004 14:36:19      TROJAN
#           stdr 862
#       
#       Revision 2 (APPROVED)
#         Created:  01-JUL-2004 15:18:27      OBERFIEL
#           Update
#       
#       Revision 1 (DELIVERED)
#         Created:  08-JAN-2004 21:22:02      PCMS
#           Initial version
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7417
#       	Action Date:       28-AUG-2009 10:01:43
#       	Relationship Type: In Response to
#       	Status:           APPROVED
#       	Title:             AvnFPS: TUG code does not handle transition from warm to cold seasons
#       
#
import logging, re, time
import Avn, AvnLib, TafDecoder, AvnParser

_Logger = logging.getLogger(Avn.CATEGORY)
_AmdPat = re.compile(r'(AFT|TIL)\s+(\d{6})|(\d{4}/\d{4})')

###############################################################################
def updateTafs(bbb, fcsts):
    badidents = []
    decoder = TafDecoder.Decoder()

    for ident in fcsts:
        try:
            try:
                tafDuration=int(AvnParser.getTafSiteCfg(ident)['thresholds']['tafduration'])
            except:
                tafDuration=24
                
            taf = decoder(fcsts[ident], bbb) 
            if not 'group' in taf or not taf['group']:
#                _Logger.error('NIL TAF for %s', ident)
                _Logger.info('NIL TAF for %s', ident)
                continue
            
            AvnLib.adjustTimes(bbb, taf)
            evtime=taf['vtime']['str'][5:]
            lines = AvnLib.makeTafFromPeriods(ident, bbb, taf['group'],
                                              tafDuration=tafDuration,
                                              evtime=evtime)
            if 'amd' in taf:
                m = _AmdPat.search(taf['amd']['str'])
                if m:
                    vtime = taf['vtime']['from']
                    tms = list(time.gmtime(vtime))
                    #
                    # Get new valid time made by adjustTimes routine
                    vpttrn = _AmdPat.search(' '.join(lines[:2]))
                    if vpttrn:
                        s = vpttrn.group(3)
                        tms[2:4] = (int(s[:2]), int(s[2:4]))
                        TafDecoder.fix_date(tms)
                        vtime = time.mktime(tms) - time.timezone
                        #
                        # If a TIL or AFT expression
                        if m.group(1):
	                        
                            s = m.group(2)
                            tms[2:5] = (int(s[:2]), int(s[2:4]), int(s[4:6]))
                            TafDecoder.fix_date(tms)
                            amdtime = time.mktime(tms) - time.timezone
	                        
                            if m.group(1) == 'AFT' and vtime >= amdtime :
                                lines.append(taf['amd']['str'][:m.start(1)])
                            elif m.group(1) == 'TIL' and vtime < amdtime:
                                lines.append(taf['amd']['str'])
                        #
                        # Range
                        elif m.group(3):
	                        
                            s = m.group(3)
                            tms[2:4] = (int(s[5:7]), int(s[7:]))
                            TafDecoder.fix_date(tms)
                            amdtime = time.mktime(tms) - time.timezone
                            if vtime < amdtime:                        
                                tms[2:4] = (int(s[:2]), int(s[2:4]))
                                TafDecoder.fix_date(tms)
                                amdtime = time.mktime(tms) - time.timezone
                                if vtime >= amdtime:
                                    lines.append('%sTIL %s00' % (taf['amd']['str'][:m.start(3)],s[5:]))
                                else:
                                    lines.append(taf['amd']['str'])
                    else:
                        lines.append(taf['amd']['str'])
                else:
                    lines.append(taf['amd']['str'])
                
            fcsts[ident] = '\n'.join(AvnLib.indentTaf(lines)+[''])
            
        except Avn.AvnError, e:
            badidents.append(ident)
            
    if badidents:
        _Logger.warning('Could not update times for %s' % ' '.join(badidents))
        
    return fcsts


def updateTafsFromJava(bbb, fcsts):
    import JUtil
    print "fcsts entry", fcsts
    fcsts = JUtil.javaStringListToPylist(fcsts)
    ids = [Avn._getIds(f) for f in fcsts]
    #bbb = self.sp.bbb.get().strip().upper()
    fcsts = updateTafs(bbb, dict(zip(ids, fcsts)))
    print type(fcsts), len(fcsts)
    return JUtil.pyValToJavaObj(fcsts)
    return fcsts