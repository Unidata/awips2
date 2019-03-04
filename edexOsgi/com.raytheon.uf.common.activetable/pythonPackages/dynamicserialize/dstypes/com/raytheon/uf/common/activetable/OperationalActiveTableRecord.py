##
##
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    05/22/2015       4522         randerso       Changed to inherit from ActiveTableRecord
#
##    

import ActiveTableRecord

class OperationalActiveTableRecord(ActiveTableRecord.ActiveTableRecord):

    def __init__(self):
        super(OperationalActiveTableRecord, self).__init__()
        