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
#       MosPlot.py
#       GFS1-NHD:A8541.0000-SCRIPT;8
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 8 (DELIVERED)
#         Created:  23-FEB-2006 13:59:53      TROJAN
#           MOS/LAMP data not plotted
#       
#       Revision 7 (DELIVERED)
#         Created:  31-JAN-2006 18:20:45      TROJAN
#           added check for nonexisting item in sites dictionary
#       
#       Revision 6 (APPROVED)
#         Created:  31-JAN-2006 08:35:25      TROJAN
#           Change in naming convention for MOS/LAMP data, added LAMP
#           to plotting module
#       
#       Revision 5 (DELIVERED)
#         Created:  06-JUL-2005 18:16:40      TROJAN
#           spr 6548
#       
#       Revision 4 (DELIVERED)
#         Created:  07-MAY-2005 11:36:34      OBERFIEL
#           Added Item Header Block
#       
#       Revision 3 (DELIVERED)
#         Created:  25-APR-2005 20:40:04      TROJAN
#           stdr917
#       
#       Revision 2 (DELIVERED)
#         Created:  06-APR-2005 11:41:07      TROJAN
#           spr 6763
#       
#       Revision 1 (APPROVED)
#         Created:  02-APR-2005 17:11:58      TROJAN
#           spr 6763
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7093
#       	Action Date:       14-APR-2006 14:40:08
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Bugs found during STDR testing
#       
#
# MosPlot.py
# George Trojan, SAIC/MDL, March 2005
# last update: 02/23/06

import Avn, AvnLib, AvnParser, Globals, TafPlotP

class Plot:
    def __init__(self, **args):
        self.color = args.get('color', 'black')
        self.model = args['model']
        self.data = None

    def plot(self, tafid, sites, tref, tticks, vticks, cticks, header):
        itime = Avn.string2time(header)
        for mosid in sites.get(self.model, []):
            data = Globals.DRC.getMosData(mosid, self.model, itime)
            if data:
                break
        else:
            data = None
        if data is None:
            raise Avn.AvnError('Cannot get %s MOS data' % self.model.upper())
        # simulate decoded TAF, without ocnl weather
        groups = [{'prev': g} for g in data.data['group']]
        self.data = AvnLib.TafData(groups)
        return TafPlotP.plot(self.data, tticks, vticks, cticks, self.color)
