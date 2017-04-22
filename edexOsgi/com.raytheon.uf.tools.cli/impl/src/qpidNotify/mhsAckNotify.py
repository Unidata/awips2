##############################################################################
# Notifies EDEX of an ACK or NACK from MHS
#
#     SOFTWARE HISTORY
#    
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    04/06/2012      10388         D. Friedman    Initial version
#    10/09/12        DR 13901      D. Friedman    Limit execution time
#    06/13/2013      DR 16242      D. Friedman    Add Qpid authentication info
#    03/06/2014      DR 17907      D. Friedman    Workaround for issue QPID-5569
#    01/27/2017      DR 19698      D. Friedman    Changed BROKER_ADDR to BROKER_HOST
##############################################################################

import getopt
import os
import os.path
import sys

from lib.Util import doWithinTime

import qpid
from qpid.util import connect
from qpid.connection import Connection
from qpid.datatypes import Message, uuid4

DESTINATION = 'amq.topic'
TOPIC_NAME = 'mhs.ackmgr'
QPID_USERNAME = 'guest'
QPID_PASSWORD = 'guest'

class MhsAckNotification:
    def __init__(self):
        self.messageId = None
        self.sender = None
        self.response = None

def remove_file(*parts):
    path = os.path.join(*parts)
    if os.path.exists(path):
        os.remove(path)

def get_qpid_connection(broker_host):
    try:
        socket = connect(broker_host, 5672)
        connection = Connection (sock=socket, username=QPID_USERNAME, password=QPID_PASSWORD)
        connection.start()
        return connection
    except:
        sys.stderr.write("mhsAckNotify: connect to %s: %s\n" % (broker_host, sys.exc_info()[1],))
        return None

def send_message(connection, notif):
    session = connection.session(str(uuid4()))
    
    props = session.delivery_properties(routing_key=TOPIC_NAME)
    head = session.message_properties(application_headers={'sender':notif.sender,
                                                                'response':notif.response},
                                      user_id=QPID_USERNAME) # For issue QPID-5569.  Fixed in Qpid 0.27
    session.message_transfer(destination=DESTINATION, message=Message(props, head, notif.messageId))
    session.close(timeout=10)
    connection.close()

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
                fn = '%s-%s.doc' %(notif.messageId, notif.sender)
                if notif.response == 'ACK':
                    remove_file(mhs_data_dir, 'ackq', fn)
                elif notif.response == 'NACK':
                    # A1 just uses message ID, but that does not look right...
                    # Do both to be safe
                    remove_file(mhs_data_dir, 'nackq', fn)
                    remove_file(mhs_data_dir, 'nackq', '%s.doc' % (notif.messageId, ))
            except:
                sys.stderr.write("mhsAckNotify: error removing MHS file: %s\n" % (sys.exc_info()[1],))

        # TODO: Should have BROKER_HOST in CLI setup.env.
        broker_host = os.getenv('BROKER_HOST')
        if not broker_host:
            broker_host = os.getenv('DEFAULT_HOST')
            if broker_host == 'ec':
                broker_host = 'cp1f'
        if not broker_host:
            broker_host = 'localhost'

        try:
            connection = doWithinTime(get_qpid_connection, args=(broker_host,))
            if connection:
                doWithinTime(send_message, max_tries=1, args=(connection, notif))
        except:
            sys.stderr.write("mhsAckNotify: error sending message: %s\n" % (sys.exc_info()[1],))
            return 1
    else:
        sys.stderr.write("mhsAckNotify: message ID not specified\n")
        sys.stderr.write("usage: mhsAckNotify -m <messageID> -s <sender> -r <response>\n")
        return 1

    return 0

if __name__ == '__main__':
    sys.exit(run())
