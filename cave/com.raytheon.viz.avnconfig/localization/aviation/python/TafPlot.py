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
#       TafPlot.py
#       GFS1-NHD:A8540.0000-SCRIPT;4
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 4 (DELIVERED)
#         Created:  18-AUG-2005 15:11:46      TROJAN
#           spr 6996
#       
#       Revision 3 (DELIVERED)
#         Created:  06-JUL-2005 18:16:42      TROJAN
#           spr 6548
#       
#       Revision 2 (DELIVERED)
#         Created:  07-MAY-2005 11:38:55      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1 (DELIVERED)
#         Created:  02-APR-2005 17:10:47      TROJAN
#           spr 6763
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_6996
#       	Action Date:       02-SEP-2005 12:22:11
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: WxPlot generates errors with NIL TAF
#       
#
# TafPlot.py
# George Trojan, SAIC/MDL, March 2005
# last update: 08/18/05

import Avn, AvnLib, Globals, TafPlotP

class Plot:
    def __init__(self, **args):
        self.color = args.get('color', 'black')
        self.data = None

    def plot(self, ident, sites, tref, tticks, vticks, cticks, header):
        graphics = {'cig': [], 'vsby': [], 'wind': []}
        try:
            taf = [x for x in Globals.DRC.getTafs(ident, True, tref-86400.0) \
                if x.header.split('\n')[0] == header][0]
        except IndexError:
            raise Avn.AvnError('Cannot access TAF for %s' % ident)
        if 'fatal' in taf.dcd:
            raise Avn.AvnError('Cannot decode TAF for %s' % ident)
        elif not taf.dcd['group']:
            raise Avn.AvnError('NIL TAF for %s' % ident)
        self.ident = ident
        self.data = AvnLib.TafData(taf.dcd['group'])
        return TafPlotP.plot(self.data, tticks, vticks, cticks, self.color)

    def replot(tticks, vticks, cticks):
        return TafPlot.plot(self.data, tticks, vticks, cticks)
