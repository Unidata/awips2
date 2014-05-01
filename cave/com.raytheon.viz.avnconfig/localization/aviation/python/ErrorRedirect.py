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
#       ErrorRedirect.py
#       GFS1-NHD:A9107.0000-SCRIPT;1
#
#    Status:
#       DELIVERED
#    
#    History:
#       Revision 1 (DELIVERED)
#         Created:  24-MAR-2006 09:18:03      TROJAN
#           spr 7103
#
#    Change Document History:
#       1:
#       	Change Document:   GFS1-NHD_SPR_7103
#       	Action Date:       06-APR-2006 07:21:59
#       	Relationship Type: In Response to
#       	Status:           CLOSED
#       	Title:             AvnFPS: Pmw code does not forward python exceptions to AvnFPS _Logger class
#       
#
# ErrorRedirect.py
# A Workaround for Pmw exception catcher
# Author: George Trojan, SAIC/MDL, March 2006
# last update: 03/09/06

import sys
import Pmw
import Busy

_pmwFile = None

def fixlogging(logger, master):
    global _pmwFile
    if _pmwFile:
        return
    _pmwFile = _PmwFile(logger, master)
    Pmw.reporterrorstofile(_pmwFile)

class _PmwFile:
    userMessage = '''Program bug - please report.
Details in the log file'''
    def __init__(self, logger, master):
        self.logger = logger
        self.master = master

    def write(self, text):
        self.logger.error(text)
        Busy.showerror(self.userMessage, self.master)
