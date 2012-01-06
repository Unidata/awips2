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
# Python logstream for logging from command line python
#   
#
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    09/19/08                      njensen       Initial Creation.
#    
# 
#

import logging

logging.basicConfig(level=logging.DEBUG,
                    format='%(levelname)s %(asctime)-8s %(message)s',
                    datefmt='%Y-%m-%d %H:%M:%S')


def logEvent(*args):
    for msg in args:        
        logging.info(msg)
    
def logProblem(*args):
    for msg in args:
        logging.error(msg)
    
def logVerbose(*args):
    for msg in args:
        logging.info(msg)
    
def logDebug(*args):
    for msg in args:
        logging.debug(msg)
