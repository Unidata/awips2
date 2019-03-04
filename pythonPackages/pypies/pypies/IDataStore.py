##
##


#
# Interface for data stores
#  
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/16/10                      njensen        Initial Creation.
#    07/30/15        1574          nabowle        Add deleteOrphanFiles
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

    def deleteOrphanFiles(self, request):
        raise pypies.NotImplementedException()

def _exc():
    t, v, tb = sys.exc_info()
    return string.join(traceback.format_exception(t, v, tb))

def _line():
    return inspect.currentframe().f_back.f_back.f_lineno

def _file():
    return inspect.currentframe().f_back.f_back.f_code.co_filename
