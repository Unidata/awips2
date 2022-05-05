##############################################################################
# Notifies EDEX of an ACK or NACK from MHS
#
# SOFTWARE HISTORY
#
# Date          Ticket#  Engineer       Description
# ------------- -------- -------------- -------------------------------------------
# Apr 06, 2012  10388    D. Friedman    Initial version
# Oct 09, 2012  13901    D. Friedman    Limit execution time
# Jun 13, 2013  16242    D. Friedman    Add Qpid authentication info
# Mar 06, 2014  17907    D. Friedman    Workaround for issue QPID-5569
# Jan 27, 2017  19698    D. Friedman    Changed BROKER_ADDR to BROKER_HOST
# Feb 13, 2018  20573    M. Porricelli  Support SSL connections
# Jul 23, 2019  7724     mrichardson    Upgrade Qpid to Qpid Proton
# Jul 07, 2020  7724     randerso       Rewrote to use BlockingConnection
# Jul 07, 2020  8187     randerso       Added qpid connection_id
#
##############################################################################

import getopt
from lib.Util import time_limit, TimeoutException
import logging
import os
import os.path
import pwd
import socket
import sys

import proton
import proton.reactor
import proton.utils

logging.basicConfig(level=logging.INFO, datefmt='%H:%M:%S',
                            format='[%(process)s] %(asctime)s %(levelname)s: %(message)s')
log = logging.getLogger('mhsAckNotify')


class MhsAckNotification:

    def __init__(self):
        self.messageId = None
        self.sender = None
        self.response = None

    def __str__(self):
        return f"messageId:{self.messageId} sender:{self.sender} response:{self.response}"


class MhsAckException(Exception):
    """Exception subclass for broker communication exceptions."""
    pass


class MhsAckConnection:

    def __init__(self, host, port=5672, ssl=None):

        pwuid = pwd.getpwuid(os.getuid())
        certdb = os.getenv("QPID_SSL_CERT_DB", os.path.join(pwuid.pw_dir, ".qpid"))
        certname = os.getenv("QPID_SSL_CERT_NAME", "guest")
        cert_password = os.getenv("QPID_SSL_CERT_PASSWORD", "password")
        certfile = os.path.join(certdb, f"{certname}.crt")
        keyfile = os.path.join(certdb, f"{certname}.key")

        url = f"amqps://{host}:{port}"
        ADDRESS = "amq.topic/mhs.ackmgr"
        ssl_domain = proton.SSLDomain(mode=proton.SSLDomain.MODE_CLIENT)
        ssl_domain.set_credentials(certfile, keyfile, cert_password)

        clientID = ":".join([
            socket.gethostname(),
            pwuid.pw_name,
            "mhsAckNotify",
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
            raise MhsAckException("Failed to connect to broker [{}].".format(url)) from e

    def sendmessage(self, notif):
        try:
            properties = {
                "sender" : notif.sender,
                "response": notif.response
                }
            self._sender.send(proton.Message(body=notif.messageId, properties=properties))
        except proton.ProtonException as e:
            log.exception("Failed to send notification [%s] to broker.", notif)
            raise MhsAckException("Failed to send notification [{}] to broker.".format(notif)) from e

    def close(self):
        try:
            self._sender.close()
            self._conn.close()
            log.debug("Disconnected from broker.")
        except proton.ProtonException as e:
            log.warning("Failed to disconnect from broker.", exc_info=True)
            raise QpidIngestException("Failed to disconnect from broker.") from e


def remove_file(*parts):
    path = os.path.join(*parts)
    if os.path.exists(path):
        os.remove(path)


def run():
    mhs_data_dir = os.getenv('MHS_DATA', '/data/fxa/mhs')
    notif = MhsAckNotification()

    opts, args = getopt.getopt(sys.argv[1:], 'm:r:s:')
    for k, v in opts:
        if k == '-m':
            notif.messageId = v
        elif k == '-r':
            notif.response = v
        elif k == '-s':
            notif.sender = v

    if notif.messageId:
        # Delete the .doc file
        if mhs_data_dir:
            try:
                fn = '{}-{}.doc'.format(notif.messageId, notif.sender)
                if notif.response == 'ACK':
                    remove_file(mhs_data_dir, 'ackq', fn)
                elif notif.response == 'NACK':
                    # A1 just uses message ID, but that does not look right...
                    # Do both to be safe
                    remove_file(mhs_data_dir, 'nackq', fn)
                    remove_file(mhs_data_dir, 'nackq', '{}.doc'.format(notif.messageId,))
            except:
                log.error("mhsAckNotify: error removing MHS file: %s-%s.doc\n", notif.messageId, notif.sender)

        broker_host = os.getenv('BROKER_HOST')
        if not broker_host:
            broker_host = os.getenv('DEFAULT_HOST')
            if broker_host == 'ev':
                broker_host = 'cpv1'
        if not broker_host:
            broker_host = 'localhost'

        try:
            with time_limit(10):
                mhs_ack_conn = MhsAckConnection(broker_host)

        except TimeoutException:
            log.error("Failed to connect to QPID broker within timeout period.")
            return 1
        except Exception:
            log.exception("Failed to connect to QPID broker.")
            return 1
        try:
            with time_limit(10):
                mhs_ack_conn.sendmessage(notif)
        except TimeoutException:
            log.error(f"Failed to send notification [{notif}] within timeout period.")
            return 1
        except Exception:
            log.exception(f"Failed to send notification [{notif}].")
            return 1

    return 0


if __name__ == '__main__':
    sys.exit(run())
