# Remote objects for BSP
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 2005-9-27
#

# There is exactly one global RemoteObjectManager object, which keeps track
# of both local objects for which proxies exist and local proxies to objects
# on other processors. It is not used directly by applications.

class RemoteObjectManager(object):
    
    def __parinit__(self, pid, nprocs):
        self.pid = pid
        self.nprocs = nprocs
        self.nprocs = nprocs
        self.local_objects = {}
        self.local_proxies = {}
        self.current_id = 0

    def registerLocalObject(self, local_object):
        remote_id = self.current_id
        self.current_id += 1
        self.local_objects[remote_id] = local_object
        local_object.__ro_remote_id__ = remote_id
        return (self.pid, remote_id)

    def retrieveLocalObject(self, remote_id):
        return self.local_object[remote_id]


remote_object_manager = RemoteObjectManager()

# The remote object base class and the Proxy class take care of
# all the bookkeeping.

class RemoteObject(object):

    def isLocal(self):
        return True

    def _getROProxy(self):
        try:
            return self.__ro_proxy__
        except AttributeError:
            self.__ro_proxy__ = Proxy(remote_object_manager
                                            .registerLocalObject(self))
            return self.__ro_proxy__

    def transferTo(self, pid):
        messages = []
        if pid != processorID:
            messages.append(([pid], self))
            try:
                proxy = self.__ro_proxy__
                messages.append((range(remote_object_manager.nprocs),
                                 TransferToken(proxy.pid, proxy.remote_id,
                                 other_pid)))
            except AttributeError:
                pass
        return messages

class Proxy(object):
    
    def __init__(self, reference):
        self.pid, self.remote_id = reference

    def isLocal(self):
        return False

    def _getROLocalObject(self):
        if self.pid != remote_object_manager.pid:
            raise ValueError("wrong pid")
        return _manager.retrieveLocalObject(self.remote_id)

class TransferToken(object):

    def __init__(self, from_pid, from_id, to_pid):
        self.from_pid = from_pid
        self.from_id = from_id
        self.to_pid = to_pid
