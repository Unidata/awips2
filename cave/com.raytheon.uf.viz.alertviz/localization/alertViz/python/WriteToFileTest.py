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

import AlertVizProcessor

#
# A debug processor that sends messages to standard out
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/09/09                      chammack       Initial Creation.
#    
# 
#
class WriteToFileTest(AlertVizProcessor.AlertVizProcessor):
        
    def process(self, statusMessage, alertMetadata, globalConfiguration):
        testFile = open('/tmp/AlertVizPyTest.txt', 'a')
        testFile.write("AV Python Test -- %s %s %s \n" % (statusMessage.getPriority(), statusMessage.getCategory(), statusMessage.getMessage()))
        testFile.close()
         