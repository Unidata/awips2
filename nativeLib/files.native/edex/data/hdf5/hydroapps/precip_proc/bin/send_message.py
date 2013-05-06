###!$(awips_home)/bin/python

import qpid
import sys
import os
from qpid.util import connect
from qpid.connection import Connection
from qpid.datatypes import Message, RangedSet, uuid4
from qpid.queue import Empty

host="QPID_SERVER"
port=5672
user="guest"
password="guest"

#  Create a connection.
socket = connect(host, port)
connection = Connection (sock=socket)
connection.start()
session = connection.session(str(uuid4()))

props = session.delivery_properties(routing_key="edex.HPE")
session.message_transfer(destination="amq.topic", message=Message(props,"HPE Fieldgen finished run."))

