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
#       UseMetarForPrevailing.py
#       GFS1-NHD:A6831.0000-SCRIPT;10
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 10 (DELIVERED)
#         Created:  26-MAR-2009 20:25:24      OBERFIEL
#           Simplified keys for TAF dictionary.  Eliminated obsolete
#           argument to updateTafs function.
#       
#       Revision 9 (DELIVERED)
#         Created:  31-DEC-2008 10:14:27      OBERFIEL
#           Changes to support amending TAFs prior to valid period.
#       
#       Revision 8 (DELIVERED)
#         Created:  17-SEP-2008 11:12:31      OBERFIEL
#           Fixed taf duration for this procedure.
#       
#       Revision 7 (DELIVERED)
#         Created:  02-SEP-2008 13:08:50      OBERFIEL
#           Updated rule to account for 30-h length TAF and allow COR
#           on the first line
#       
#       Revision 6 (DELIVERED)
#         Created:  09-SEP-2005 14:04:17      TROJAN
#           spr 7011
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
#         Created:  03-SEP-2004 12:40:47      OBERFIEL
#           Updated code
#       
#       Revision 2 (APPROVED)
#         Created:  01-JUL-2004 15:18:23      OBERFIEL
#           Update
#       
#       Revision 1 (DELIVERED)
#         Created:  08-JAN-2004 21:23:12      PCMS
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
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    Nov 02, 2012    15476         zhao           Retrieve latest METAR record from database
##
import logging, time
import Avn, AvnLib, AvnParser, TafDecoder

import MetarData

_Logger = logging.getLogger(Avn.CATEGORY)

###############################################################################
def updateTafs(bbb, fcsts):
    # master is the parent widget, not used here
    # fcsts is a dictionary of forecasts displayed in the editor window
    # the function returns dictionary of modified forecasts
    # may raise AvnError
    badidents = []
    decoder = TafDecoder.Decoder()

    for ident in fcsts:
        try:            
            tafDuration=int(AvnParser.getTafSiteCfg(ident)['thresholds']['tafduration'])
            taf = decoder(fcsts[ident], bbb)
            
            if not 'group' in taf or not taf['group']:
                _Logger.error('NIL TAF for %s', ident)
                continue
            
            AvnLib.adjustTimes(bbb, taf)
            evtime=taf['vtime']['str'][5:]
# For DR15476: use 'maxSize=0' to indicate that the latest record is to be retrieved
            metar = MetarData.retrieve(ident,0)[0]
            AvnLib.updateTafWithMetar(taf['group'][0]['prev'], metar.dcd)
            lines = AvnLib.makeTafFromPeriods(ident, bbb, taf['group'],
                                              tafDuration=tafDuration,
                                              evtime=evtime)

            if 'amd' in taf:
                lines.append(taf['amd']['str'])

            fcsts[ident] = '\n'.join(AvnLib.indentTaf(lines)+[''])

        except Exception, e:
            _Logger.exception(ident)
            badidents.append(ident)
            
    if badidents:
        _Logger.warning('Could not update TAFs for %s' % ' '.join(badidents))
        
    return fcsts
