# #
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
# #

#
#   Convenience class for using the DAF's notifications feature. This is a
#   collection that, once connected to EDEX by calling start(), fills with
#   data as notifications come in. Runs on a separate thread to allow
#   non-blocking data retrieval.
#
#
#
#     SOFTWARE HISTORY
#
#    Date            Ticket#       Engineer       Description
#    ------------    ----------    -----------    --------------------------
#    07/29/16        2416          tgurney        Initial creation
#

from awips.dataaccess import DataNotificationLayer as DNL

import time
from threading import Thread
import sys


if sys.version_info.major == 2:
    from Queue import Queue, Empty
else: # Python 3 module renamed to 'queue'
    from queue import Queue, Empty


"""Used to indicate a DataQueue that will produce geometry data."""
GEOMETRY = object()


"""Used to indicate a DataQueue that will produce grid data."""
GRID = object()


"""Default maximum queue size."""
_DEFAULT_MAXSIZE = 100


class Closed(Exception):
    """Raised when attempting to get data from a closed queue."""
    pass


class DataQueue(object):

    """
    Convenience class for using the DAF's notifications feature. This is a
    collection that, once connected to EDEX by calling start(), fills with
    data as notifications come in.

    Example for getting obs data:

      from DataQueue import DataQueue, GEOMETRY
      request = DataAccessLayer.newDataRequest('obs')
      request.setParameters('temperature')
      request.setLocationNames('KOMA')
      q = DataQueue(GEOMETRY, request)
      q.start()
      for item in q:
          print(item.getNumber('temperature'))
    """

    def __init__(self, dtype, request, maxsize=_DEFAULT_MAXSIZE):
        """
        Create a new DataQueue.

        Args:
            dtype: Either GRID or GEOMETRY; must match the type of data
              requested.
            request: IDataRequest describing the data you want. It must at
              least have datatype set. All data produced will satisfy the
              constraints you specify.
            maxsize: Maximum number of data objects the queue can hold at
              one time. If the limit is reached, any data coming in after
              that will not appear until one or more items are removed using
              DataQueue.get().
        """
        assert maxsize > 0
        assert dtype in (GEOMETRY, GRID)
        self._maxsize = maxsize
        self._queue = Queue(maxsize=maxsize)
        self._thread = None
        if dtype is GEOMETRY:
            self._notifier = DNL.getGeometryDataUpdates(request)
        elif dtype is GRID:
            self._notifier = DNL.getGridDataUpdates(request)

    def start(self):
        """Start listening for notifications and requesting data."""
        if self._thread is not None:
            # Already started
            return
        kwargs = {'callback': self._data_received}
        self._thread = Thread(target=self._notifier.subscribe, kwargs=kwargs)
        self._thread.daemon = True
        self._thread.start()
        timer = 0
        while not self._notifier.subscribed:
            time.sleep(0.1)
            timer += 1
            if timer >= 100:  # ten seconds
                raise RuntimeError('timed out when attempting to subscribe')

    def _data_received(self, data):
        for d in data:
            if not isinstance(d, list):
                d = [d]
            for item in d:
                self._queue.put(item)

    def get(self, block=True, timeout=None):
        """
        Get and return the next available data object. By default, if there is
        no data yet available, this method will not return until data becomes
        available.

        Args:
            block: Specifies behavior when the queue is empty. If True, wait
              until an item is available before returning (the default). If
              False, return None immediately if the queue is empty.
            timeout: If block is True, wait this many seconds, and return None
              if data is not received in that time.
        Returns:
            IData
        """
        if self.closed:
            raise Closed
        try:
            return self._queue.get(block, timeout)
        except Empty:
            return None
        
    def get_all(self):
        """
        Get all data waiting for processing, in a single list. Always returns
        immediately. Returns an empty list if no data has arrived yet.
        
        Returns:
            List of IData
        """
        data = []
        for _ in range(self._maxsize):
            next_item = self.get(False)
            if next_item is None:
                break
            data.append(next_item)
        return data

    def close(self):
        """Close the queue. May not be re-opened after closing."""
        if not self.closed:
            self._notifier.close()
        self._thread.join()

    def qsize(self):
        """Return number of items in the queue."""
        return self._queue.qsize()

    def empty(self):
        """Return True if the queue is empty."""
        return self._queue.empty()

    def full(self):
        """Return True if the queue is full."""
        return self._queue.full()

    @property
    def closed(self):
        """True if the queue has been closed."""
        return not self._notifier.subscribed

    @property
    def maxsize(self):
        """
        Maximum number of data objects the queue can hold at one time.
        If this limit is reached, any data coming in after that will not appear
        until one or more items are removed using get().
        """
        return self._maxsize

    def __iter__(self):
        if self._thread is not None:
            while not self.closed:
                yield self.get()

    def __enter__(self):
        self.start()
        return self

    def __exit__(self, *unused):
        self.close()