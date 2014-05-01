#===============================================================================
# qpidingest.py
#
# @author: Aaron Anderson
# @organization: NOAA/WDTB OU/CIMMS
# @version: 1.0 02/19/2010
# @requires: QPID Python Client available from http://qpid.apache.org/download.html
#            The Python Client is located under Single Component Package/Client
#
#            From the README.txt Installation Instructions
#                = INSTALLATION =
#                Extract the release archive into a directory of your choice and set
#                your PYTHONPATH accordingly:
#
#                tar -xzf qpid-python-<version>.tar.gz -C <install-prefix>
#                export PYTHONPATH=<install-prefix>/qpid-<version>/python
#
#       ***EDEX and QPID must be running for this module to work***
#
# DESCRIPTION:
# This module is used to connect to QPID and send messages to the external.dropbox queue
# which tells EDEX to ingest a data file from a specified path. This avoids having to copy
# a data file into an endpoint. Each message also contains a header which is used to determine
# which plugin should be used to decode the file. Each plugin has an xml file located in
# $EDEX_HOME/data/utility/edex_static/base/distribution that contains regular expressions
# that the header is compared to. When the header matches one of these regular expressions
# the file is decoded with that plugin. If you make changes to one of these xml files you
# must restart EDEX for the changes to take effect.
#
# NOTE: If the message is being sent but you do not see it being ingested in the EDEX log
# check the xml files to make sure the header you are passing matches one of the regular
# expressions. Beware of spaces, some regular expressions require spaces while others use
# a wildcard character so a space is optional. It seems you are better off having the space
# as this will be matched to both patterns. For the file in the example below,
# 20100218_185755_SAUS46KLOX.metar, I use SAUS46 KLOX as the header to make sure it matches.
#
#
# EXAMPLE:
# Simple example program:
#
#------------------------------------------------------------------------------
# import qpidingest
# #Tell EDEX to ingest a metar file from data_store. The filepath is
# #/data_store/20100218/metar/00/standard/20100218_005920_SAUS46KSEW.metar
#
# conn=qpidingest.IngestViaQPID() #defaults to localhost port 5672
#
# #If EDEX is not on the local machine you can make the connection as follows
# #conn=qpidingest.IngestViaQPID(host='<MACHINE NAME>',port=<PORT NUMBER>)
#
# conn.sendmessage('/data_store/20100218/metar/18/standard/20100218_185755_SAUS46KLOX.metar','SAUS46 KLOX')
# conn.close()
#-------------------------------------------------------------------------------
#
#    SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    ....
#    06/13/2013      DR 16242      D. Friedman    Add Qpid authentication info
#    03/06/2014      DR 17907      D. Friedman    Workaround for issue QPID-5569
#
#===============================================================================   

import qpid
from qpid.util import connect
from qpid.connection import Connection
from qpid.datatypes import Message, uuid4

QPID_USERNAME = 'guest'
QPID_PASSWORD = 'guest'

class IngestViaQPID:
    def __init__(self, host='localhost', port=5672):
        '''
        Connect to QPID and make bindings to route message to external.dropbox queue
        @param host: string hostname of computer running EDEX and QPID (default localhost)
        @param port: integer port used to connect to QPID (default 5672)
        '''
        
        try:
            #
            self.socket = connect(host, port)
            self.connection = Connection (sock=self.socket, username=QPID_USERNAME, password=QPID_PASSWORD)
            self.connection.start()
            self.session = self.connection.session(str(uuid4()))
            self.session.exchange_bind(exchange='amq.direct', queue='external.dropbox', binding_key='external.dropbox')
            print 'Connected to Qpid'
        except:
            print 'Unable to connect to Qpid'
               
    def sendmessage(self, filepath, header):
        '''
        This function sends a message to the external.dropbox queue providing the path
        to the file to be ingested and a header to determine the plugin to be used to
        decode the file.
        @param filepath: string full path to file to be ingested
        @param header: string header used to determine plugin decoder to use
        '''
        props = self.session.delivery_properties(routing_key='external.dropbox')
        head = self.session.message_properties(application_headers={'header':header},
                                               user_id=QPID_USERNAME) # For issue QPID-5569.  Fixed in Qpid 0.27
        self.session.message_transfer(destination='amq.direct', message=Message(props, head, filepath))
           
    def close(self):
        '''
        After all messages are sent call this function to close connection and make sure
        there are no threads left open
        '''
        self.session.close(timeout=10)
        print 'Connection to Qpid closed'