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
import httplib

##############################################################################
# this class implements the HTTP Communication Handler
# 
# general usage:
#    import CommHandler as CH
#    script = "..."
#    ch = CH.CommHandler('localhost:9581','/services/pyproductxml')
#    ch.process(script)
#    if ch.isGoodStatus:
#        print ch.getContents()
##############################################################################
class CommHandler:
    # Initializer.
    #
    # args:
    #   connection:    connection string (server:port)
    #   service: service end-point for connection
    #   mode:          HTTP mode (POST or GET)
    def __init__(self,connection,service,mode='POST'):
        self.connection = connection
        self.service = service
        self.mode = mode
        self.status = -1
        self.reason = ""
        self.headers = ""
        self.contents = ""

    # Provides the string representation of the class. This method is
    # called whenever the str() function is applied to an instance.
    def __str__(self):
        return 'CommHandler[connection=' + self.connection \
               + ',service=' + self.service + ',mode=' + self.mode+']'
    
    # Performs the HTTP request. Following successful completion of
    # the request, the client may use the getters to access responses.
    #
    # args:
    #   text:  the text to include in the request
    # raises:
    #   CommError if any error occurs
    def process(self,text):
        conn = None
        try:
           # setup and send a message message
            conn = httplib.HTTP(self.connection)
            conn.putrequest(self.mode, self.service)
            conn.putheader("Content-Type", "text/plain")
            conn.putheader("Content-Length", "%d" % len(text))
            conn.endheaders()
            conn.send(text)
            
            # Success/Failure messages 
            self.status, self.reason, self.headers = conn.getreply()
            self.contents = conn.getfile().read()
        except Exception,e:
            raise CommError("Unable to process communication request",e)
        finally:
            if conn != None:
                conn.close()
    
    # combines the status token, status text and message contents
    # into a single, printable string
    def formatResponse(self):
        return str(self.status) + " " + str(self.reason) + "\n" + \
               str(self.contents)

    # Determines if the session returned a good status
    # 
    # return:
    #   True:  if the session was successful
    #   False: if the session was unsuccessful 
    def isGoodStatus(self):
        return self.status == httplib.OK
    
    # Convenience method. returns a single list containing the results
    # of the session. The list consists of the HTTP status code, the
    # text version of the HTTP status code, the headers returned from
    # the connection and the contents of the response.
    #
    # return:
    #   the full output from the session
    #
    # Note: This method is valid only after the completion of an HTTP
    #       session. at other times, the results are meaningless.
    def getFullOutput(self):
        return [self.status, self.reason, self.headers, self.contents]
    
    # return the HTTP status code.
    def getStatus(self):
        return self.status
    
    # return the HTTP status text
    def getReason(self):
        return self.reason
    
    # return the HTTP session headers
    def getHeaders(self):
        return self.headers
    
    # return the response contents
    def getContents(self):
        return self.contents

##############################################################################
# defines the Exception raised by methods in this package
##############################################################################
class CommError(Exception):
    def __init__(self,value,cause=None):
      self.value = value
      self.cause = cause
    def __str__(self):
        msg = 'CommError: ' + repr(self.value)
        if self.cause is not None:
            msg += "\n caused by " + repr(self.cause)
        return msg
    def __repr__(self):
        return self.__str__()