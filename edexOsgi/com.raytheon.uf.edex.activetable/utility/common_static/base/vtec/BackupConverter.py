##
##

##
# This is a base file that is not intended to be overridden.
##



import cStringIO
import gzip
import cPickle
import JUtil

class BackupConverter:
    def __init__(self):
        pass
    
    def convertToJava(self, fileName, fileContent):
        table = cPickle.loads(fileContent);
        jtable = JUtil.pyValToJavaObj(table)
        return jtable 