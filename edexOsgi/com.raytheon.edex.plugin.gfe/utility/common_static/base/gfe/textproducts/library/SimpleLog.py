
##
##

##
# This is a base file that is not intended to be overridden.
##



__version__ = "1.0"

class SimpleLog():
    """
    Log messages to standard out
    
    SOFTWARE HISTORY
    Date            Ticket#        Engineer    Description
    ------------    ----------     ----------- --------------------------
    Jul 09, 2008    1222           jelkins     Initial version
    
    @author: jelkins
    
    Class with a default method handler idea from:
    http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/307618
    """
    def __init__(self,logName):
        self.logName = logName
    
    def handlerFunctionClosure(self,name):
        def handlerFunction(message):
            
            from time import strftime
            dateTime = strftime("%Y-%m-%d %H:%M:%S,000")
            
            from string import upper
            print "%s %s %s: %s" % (upper(name),dateTime,self.logName,message)
            
        return handlerFunction
    def __getattr__(self,name):
        return self.handlerFunctionClosure(name)