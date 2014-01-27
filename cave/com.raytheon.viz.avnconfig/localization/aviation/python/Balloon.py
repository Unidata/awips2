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
#       Balloon.py
#       GFS1-NHD:A6627.0000-SCRIPT;1.5
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1.5 (DELIVERED)
#         Created:  30-DEC-2008 12:46:08      OBERFIEL
#           Check to see that balloon remains below mouse pointer
#       
#       Revision 1.4 (DELIVERED)
#         Created:  06-JUL-2005 20:50:37      TROJAN
#           spr 6910
#       
#       Revision 1.3 (DELIVERED)
#         Created:  07-MAY-2005 11:31:13      OBERFIEL
#           Added Item Header Block
#       
#       Revision 1.2 (DELIVERED)
#         Created:  01-JUL-2004 14:59:15      OBERFIEL
#           Update
#       
#       Revision 1.1 (DELIVERED)
#         Created:  06-NOV-2003 16:45:24      OBERFIEL
#           date and time created -2147483647/-2147483648/-2147481748
#           -2147483648:-2147483648:-2147483648 by oberfiel
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7407
#       	Action Date:       03-JAN-2009 09:26:51
#       	Relationship Type: In Response to
#       	Status:           TEST
#       	Title:             AvnFPS: Allow WFOs to update HDF5 climate files
#       
#
import Pmw

_instance = None
def Balloon(parent=None, **kw):
    global _instance

    if not 'pinned' in kw:
        kw['yoffset'] = 1 # solves the flicker issue
    else:
        del kw['pinned']
        
    if not _instance:
        _instance = Pmw.Balloon(parent, **kw)
    elif kw:
        _instance.configure(**kw)
    return _instance

