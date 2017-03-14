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
# Sample script to show capability of running batch list of afos commands
# through localization
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    08/18/09                      rjpeter        Initial Creation.
#    
# 
#

import BaseTool

class ExecuteAFOSCommands(BaseTool.BaseTool):
    
     def process(self, pythonTextEditorToolCallback, arg):
         rval = str()
         list = arg.split(',')
         for s in list:
             products = pythonTextEditorToolCallback.executeAFOSCommand(s)
             for prod in products:
                 if len(prod.getProduct()) > 0:
                     rval += prod.getProduct() + "\n"

         if len(rval) <= 0:
             pythonTextEditorToolCallback.displayMessage('No product in the database matches your request.');

         return rval
         