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
#       EtaPlot.py
#       GFS1-NHD:A8542.0000-SCRIPT;9
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 9 (DELIVERED)
#         Created:  28-JUN-2006 08:18:45      OBERFIEL
#           Changes to bring into sync with previous workset
#       
#       Revision 8 (DELIVERED)
#         Created:  04-APR-2006 08:10:10      OBERFIEL
#           Fixed error when plotting data
#       
#       Revision 7 (DELIVERED)
#         Created:  31-JAN-2006 18:22:30      TROJAN
#           added check for nonexistent item in sites dictionary
#       
#       Revision 6 (DELIVERED)
#         Created:  06-JUL-2005 18:16:37      TROJAN
#           spr 6548
#       
#       Revision 5 (DELIVERED)
#         Created:  07-MAY-2005 11:33:01      OBERFIEL
#           Added Item Header Block
#       
#       Revision 4 (DELIVERED)
#         Created:  25-APR-2005 20:40:03      TROJAN
#           stdr917
#       
#       Revision 3 (DELIVERED)
#         Created:  18-APR-2005 17:31:48      OBERFIEL
#           Changes to support gamin
#       
#       Revision 2 (DELIVERED)
#         Created:  06-APR-2005 11:41:07      TROJAN
#           spr 6763
#       
#       Revision 1 (APPROVED)
#         Created:  02-APR-2005 17:13:11      TROJAN
#           spr 6763
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7186
#       	Action Date:       26-FEB-2007 09:50:37
#       	Relationship Type: In Response to
#       	Status:           BUILD_RELEASE
#       	Title:             AvnFPS: Fix EtaPlot.py to be consistent with AvnFPS3.2 code
#       
#
# EtaPlot.py
# George Trojan, SAIC/MDL
# last update: 06/27/06

import Avn, AvnLib, AvnParser, Globals, TafPlotP

class Plot:
    def __init__(self, **args):
        self.color = args.get('color', 'black')
        self.data = None

    def plot(self, ident, sites, tref, tticks, vticks, cticks, header):
        itime = Avn.string2time(header)
        for etaid in sites.get('eta', []):
            data = Globals.DRC.getEtaData(etaid, itime)
            if data:
                break
        else:
            data = None
        if data is None:
            raise Avn.AvnError('Cannot get WRF-NAM data')
        # simulate decoded TAF, without ocnl weather
        groups = [{'prev': g} for g in data.data['group']]
        self.data = AvnLib.TafData(groups)
        return TafPlotP.plot(self.data, tticks, vticks, cticks, self.color)
