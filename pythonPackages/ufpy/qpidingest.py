#===============================================================================
# qpidingest.py
#
# @author: Aaron Anderson
# @organization: NOAA/WDTB OU/CIMMS
# @version: 1.0 02/19/2010
# @requires: awips2-python and awips2-qpid-proton-python RPMs
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
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer     Description
# ------------- -------- ------------ ------------------------------------------
# Jun 13, 2013  16242    D. Friedman  Add Qpid authentication info
# Mar 06, 2014  17907    D. Friedman  Workaround for issue QPID-5569
# Feb 16, 2017  6084     bsteffen     Support ssl connections
# Jun 14, 2019  7870     mrichardson  MHS env workaround
# Jul 23, 2019  7724     mrichardson  Upgrade Qpid to Qpid Proton
# Nov 06, 2019  7724     tgurney      Remove the unnecessary
#                                     QpidQueueManager
# Dec 12, 2019  7995     dgilling     Revert interface changes from #7724.
# Jul 07, 2020  8187     randerso     Added qpid connection_id
#
#===============================================================================   

import logging
import os
import pwd
import os.path
import socket

import proton
import proton.utils
import proton.reactor

log = logging.getLogger("qpidingest")


class QpidIngestException(Exception):
    """Exception subclass for broker communication exceptions."""
    pass

class IngestViaQPID:
    def __init__(self, host="localhost", port=5672, program="qpidingest"):
        '''
        Connect to QPID and make bindings to route message to external.dropbox queue
        @param host: string hostname of computer running EDEX and QPID (default localhost)
        @param port: integer port used to connect to QPID (default 5672)
        '''

        pwuid = pwd.getpwuid(os.getuid())
        certdb = os.getenv("QPID_SSL_CERT_DB", os.path.join(pwuid.pw_dir, ".qpid"))
        certname = os.getenv("QPID_SSL_CERT_NAME", "guest")
        cert_password = os.getenv("QPID_SSL_CERT_PASSWORD", "password")
        certfile = os.path.join(certdb, f"{certname}.crt")
        keyfile = os.path.join(certdb, f"{certname}.key")

        url = f"amqps://{host}:{port}"
        ADDRESS = "external.dropbox"
        ssl_domain = proton.SSLDomain(mode=proton.SSLDomain.MODE_CLIENT)
        ssl_domain.set_credentials(certfile, keyfile, cert_password)
        
        clientID = ":".join([
            socket.gethostname(), 
            pwuid.pw_name, 
            program, 
            str(os.getpid()), 
        ])

        try:
            container = proton.reactor.Container()
            container.container_id = clientID 
            self._conn = proton.utils.BlockingConnection(url, ssl_domain=ssl_domain)
            self._sender = self._conn.create_sender(ADDRESS)
            log.debug("Connected to broker [%s], endpoint [%s].", url, ADDRESS)
        except proton.ProtonException as e:
            log.exception("Failed to connect to broker [%s].", url)
            raise QpidIngestException("Failed to connect to broker [{}].".format(url)) from e

    def sendmessage(self, filepath, header):
        '''
        This function sends a message to the external.dropbox queue providing the path
        to the file to be ingested and a header to determine the plugin to be used to
        decode the file.
        @param filepath: string full path to file to be ingested
        @param header: string header used to determine plugin decoder to use
        '''
        try:
            self._sender.send(proton.Message(body=filepath, subject=header))
        except proton.ProtonException as e:
            log.exception("Failed to send file [%s] to broker.", filepath)
            raise QpidIngestException("Failed to send file [{}] to broker.".format(filepath)) from e 

    def close(self):
        '''
        After all messages are sent call this function to close connection and make sure
        there are no threads left open
        '''
        try:
            self._sender.close()
            self._conn.close()
            log.debug("Disconnected from broker.")
        except proton.ProtonException as e:
            log.warning("Failed to disconnect from broker.", exc_info=True)
            raise QpidIngestException("Failed to disconnect from broker.") from e 

