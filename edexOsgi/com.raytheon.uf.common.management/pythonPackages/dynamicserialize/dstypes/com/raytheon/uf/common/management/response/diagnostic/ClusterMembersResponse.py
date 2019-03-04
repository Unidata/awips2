##
##

# File auto-generated against equivalent DynamicSerialize Java class

class ClusterMembersResponse(object):

    def __init__(self):
        self.status = None

    def getStatus(self):
        return self.status

    def setStatus(self, status):
        self.status = status
    
    def __repr__(self):
        msg = ''
        for x in self.status:
            msg += str(x) + '\n'
        return msg

