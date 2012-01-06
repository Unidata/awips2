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
#       getRFlags.py
#       GFS1-NHD:A10130.0000-SCRIPT;2
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 2 (DELIVERED)
#         Created:  31-OCT-2008 09:29:34      OBERFIEL
#           Updated to work around awful GFESuite code
#       
#       Revision 1 (DELIVERED)
#         Created:  03-JUN-2008 12:51:20      OBERFIEL
#           New routine for generating command line arguments for
#           ifpText program.
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7420
#       	Action Date:       29-DEC-2008 15:09:10
#       	Relationship Type: In Response to
#       	Status:           NEXTRELEASE
#       	Title:             AvnFPS: Use of ifpServerText to get Edit Areas negatively impacts ifpServer
#       
#
if __name__ == '__main__':
    """Get all TAFs configured in AvnFPS"""
    import os, AvnParser    
    #
    # The temporary file contains all ifpServer edit areas.
    try:
        os.chdir(os.environ.get('TOP_DIR','/awips/adapt/avnfps'))
        for site in AvnParser.getAllSiteIds()['taf']:
            print site
    except:
        raise
