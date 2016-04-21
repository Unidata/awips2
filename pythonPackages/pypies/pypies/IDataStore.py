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
# Interface for data stores
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/16/10                      njensen       Initial Creation.
#    
# 
#

import sys
import pypies

import string, inspect, traceback

class IDataStore:
    
    def __init__(self):
        pass
    
    def store(self, request):
        raise pypies.NotImplementedException()
    
    def delete(self, request):
        raise pypies.NotImplementedException()
    
    def retrieve(self, request):
        raise pypies.NotImplementedException()
    
    def getDatasets(self, request):
        raise pypies.NotImplementedException()
    
    def retrieveDatasets(self, request):
        raise pypies.NotImplementedException()
    
    def retrieveGroups(self, request):
        raise pypies.NotImplementedException()
    
    def deleteFiles(self, request):
        raise pypies.NotImplementedException()
    
    def createDataset(self, request):
        raise pypies.NotImplementedException()
    
def _exc():
    t, v, tb = sys.exc_info()
    return string.join(traceback.format_exception(t, v, tb))
    
def _line():
    return inspect.currentframe().f_back.f_back.f_lineno
    
def _file():
    return inspect.currentframe().f_back.f_back.f_code.co_filename
