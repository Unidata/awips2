##
##
#    
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    06/22/2015       4573         randerso       Initial creation (hand generated)
#    08/27/2015       4812         randerso       Change __str__ to return the self.value
#                                                 instead of __repr__(self.value) to eliminate
#                                                 ''s around string. 
#
##    

class JobProgress(object):
    def __init__(self):
        self.value = None
        
    def __str__(self):
        return self.value
