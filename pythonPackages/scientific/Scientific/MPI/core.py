# The MPI Interface is written in C; this module only contains documentation
# and imports objects from the C module.
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
#        and Jakob Schiotz <schiotz@fysik.dtu.dk>
# last revision: 2006-11-23
#

"""
Python interface to the Message Passing Interface (MPI)

This module contains a Python interface to the Message Passing
Interface (MPI), and standardized library for message-passing parallel
computing. Please read an introduction to MPI before using this
module; some terms in the documentation do not make much sense unless
you understand the principles of MPI.

This module contains an object, 'world', which represents the
default communicator in MPI. This communicator can be used directly
for sending and receiving data, or other communicators can be
derived from it.

A number of global constants are also defined (L{max}, L{min}, L{prod},
L{sum}, L{land}, L{lor}, L{lxor}, L{band}, L{bor}, L{bxor}, L{maxloc}),
and L{minloc}). They are used to specify the desired operator in calls to
the 'reduce' and 'allreduce' methods of the communicator objects.

@undocumented: core*
"""


class MPIError(EnvironmentError):
    """
    MPI call failed
    """
    pass

import sys

if sys.modules.has_key('epydoc'):

    # Fake code just for the docstrings!

    class MPICommunicator:

        """
        MPI Communicator

        There is no constructor for MPI Communicator objects. The
        default communicator is given by Scientific.MPI.world, and
        other communicators can only be created by methods on an
        existing communicator object.

        A communicator object has two read-only attributes: 'rank' is
        an integer which indicates the rank of the current process in
        the communicator, and 'size' is an integer equal to the number
        of processes that participate in the communicator.
        """

        def duplicate(self):
            """
            @returns: a new communicator with the same properties
                      as the original one
            @rtype: L{MPICommunicator}
            """
            pass

        def subset(self, ranks):
            """
            Create a communicator for a subset of the processes
            
            The method should be called by all processes simultaneously. The
            return value will be the new communicator on those
            processes listed in C{ranks} and C{None} for the rest.

            @param ranks: a list of ranks, one for each process that should
                          belong to the new communicator
            @type ranks: C{list} of C{int}
            @returns: a new communicator containing a subset
                      of the processes participating in the original one
            """
            pass

        def send(self, data, destination, tag):
            """
            Send data to another process (blocking)

            @param data: the data to be sent
            @type data: C{str} or C{Numeric.array}. Array arguments
                        must have contiguous storage. General object arrays
                        are not allowed.
            @param destination: the rank of the destination process
            @type destination: C{int}
            @param tag: Identifier
            @type tag: C{int}
            """
            pass

        def nonblockingSend(self, data, destination, tag):
            """
            Send data to another process (non-blocking)

            @param data: the data to be sent
            @type data: C{str} or C{Numeric.array}. Array arguments
                        must have contiguous storage. General object arrays
                        are not allowed.
            @param destination: the rank of the destination process
            @type destination: C{int}
            @param tag: Identifier
            @type tag: C{int}
            @returns: MPI request object (used to wait for completion)
            @rtype: L{MPIRequest}
            """
            pass
            
        def receive(self, data, source=None, tag=None):
            """
            Receive data from another process (blocking)

            @param data: either a contiguous array object, or a one-letter
                         typecode (in practice, one would use Numeric.Int,
                         Numeric.Float, etc.). If an array, the data is
                         copied to the array, whic must have the right shape.
                         If a typecode, an array of that type is created
                         and used as the buffer for incoming data.
            @type data: C{Numeric.array} or C{str}
            @param source: the rank of the process from which data is
                           accepted. C{None} means data is accepted from
                           any process.
            @type source: C{int} or C{NoneType}
            @param tag: Identifier that acts as a filter; only messages
                        with a matching tag are received. A value of C{None}
                        means that any tag will match.
            @type tag: C{int} or C{NoneType}
            @returns: a tuple containing four elements:
                      the array containing the data, the source process rank
                      (an C{int}), the message tag (an C{int}), and the
                      number of elements that were received (an C{int}).
            @rtype: C{tuple}
            """
            pass

        def receiveString(self, source=None, tag=None):
            """
            Receive string data from another process (blocking)

            @param source: the rank of the process from which data is
                           accepted. C{None} means data is accepted from
                           any process.
            @type source: C{int} or C{NoneType}
            @param tag: Identifier that acts as a filter; only messages
                        with a matching tag are received. A value of C{None}
                        means that any tag will match.
            @type tag: C{int} or C{NoneType}
            @returns: a tuple containing three elements:
                      the string containing the data, the source process rank
                      (an C{int}), the message tag (an C{int}).
            @rtype: C{tuple}
            """
            pass
            
        def nonblockingReceive(self, data, source=None, tag=None):
            """
            Receive data from another process (non-blocking)

            @param data: a contiguous array object to which the incoming
                         data is copied. It must have the right shape.
            @type data: C{Numeric.array}
            @param source: the rank of the process from which data is
                           accepted. C{None} means data is accepted from
                           any process.
            @type source: C{int} or C{NoneType}
            @param tag: Identifier that acts as a filter; only messages
                        with a matching tag are received. A value of C{None}
                        means that any tag will match.
            @type tag: C{int} or C{NoneType}
            @returns: MPI request object (used to wait for completion and
                      obtain the received data)
            @rtype: L{MPIRequest}
            """
            pass

        def nonblockingProbe(self, source=None, tag=None):
            """
            Check for incoming messages

            @param source: the rank of the process from which messages are
                           accepted. C{None} means data is accepted from
                           any process.
            @type source: C{int} or C{NoneType}
            @param tag: Identifier that acts as a filter; only messages
                        with a matching tag are considered. A value of C{None}
                        means that any tag will match.
            @type tag: C{int} or C{NoneType}
            @returns: C{None} if no messages are available, otherwise
                      a tuple containing the source rank and the tag
            @rtype: C{NoneType} or C{tuple}
            """
            pass

        def broadcast(self, array, root):
            """
            Send data to all processes

            @param array: an array containing the data to be sent on the
                          sending process and serving as a buffer for the
                          incoming data on all processes. The shape and type
                          of the array must be the same on all processes.
            @type array: Numeric.array
            @param root: the rank of the sending process
            @type root: C{int}
            @note: The data is sent to all processes, including the sending
                   one.
            """
            pass

        def share(self, send, receive):
            """
            Distribute data from each processpr to all other processesors

            @param send: an array of identical shape and type on all processes.
                         It contains on each process the data that is sent.
            @type send: C{Numeric.array}
            @param receive: an array whose type is the same as for the send
                            array and which has an additional dimension
                            (the first one) whose length is the number of
                            processes. After the call, the value
                            of receive[i] is equal to the contents of the
                            array send in process i.
            @type receive: C{Numeric.array}
            """
            pass

        def barrier(self):
            """
            Wait until all processes in the communicator have
            called the same method, then all processes continue.
            """
            pass

        def abort(self, error_code):
            """
            Abort all processes associated with the communicator.
            For emergency use only.

            @param error_code: error code passed back to the calling
                               program (usually a shell) under most
                               Unix implementations of MPI
            @type error_code: C{int}
            """
            pass

        def reduce(self, sendbuffer, receivebuffer, operation, root):
            """
            Combine data from all processes and send result to one

            @param sendbuffer: an array holding the data that each
                               process contributes
            @type sendbuffer: C{Numeric.array}
            @param receivebuffer: an array acting as a buffer for the
                                  result of the reduction. Used only
                                  by the process whose rank is root
            @type receivebuffer: C{Numeric.array}
            @param operation: one of the operation objects: L{max},
                              L{min}, L{prod}, L{sum}, L{land}, L{lor},
                              L{lxor}, L{band}, L{bor}, L{bxor},
                              L{maxloc} and L{minloc}
            @type operation: MPIOperationObject
            @param root: the rank of the process that received the result
            @type root: C{int}
            """
            pass

        def allreduce(self, sendbuffer, receivebuffer, operation):
            """
            Combine data from all processes and send result to all

            @param sendbuffer: an array holding the data that each
                               process contributes
            @type sendbuffer: C{Numeric.array}
            @param receivebuffer: an array acting as a buffer for the
                                  result of the reduction
            @type receivebuffer: C{Numeric.array}
            @param operation: one of the operation objects: L{max},
                              L{min}, L{prod}, L{sum}, L{land}, L{lor},
                              L{lxor}, L{band}, L{bor}, L{bxor},
                              L{maxloc} and L{minloc}
            @type operation: MPIOperationObject
            """
            pass

    class MPIRequest:
        """
        MPI Request

        There is no constructor for MPI Request objects.  They are
        returned by nonblocking send and receives, and are used to
        query the status of the message.
        """

        def wait(self):
            """
            Wait till the communication has completed.

            If the operation was a nonblocking send, there is no return value.
            If the operation was a nonblocking receive, the return
            value is a tuple containing four elements: the array
            containing the data, the source process rank (an integer),
            the message tag (an integer), and the number of elements
            that were received (an integer).
            """
            pass

        def test(self):
            """
            Test if communications have completed.

            If the operation was a nonblocking send, it returns 0 if
            the operation has not completed, and 1 if it has.

            If the operation was a nonblocking receive, 0 is returned
            if the operation was not completed, and a tuple containing
            four elements if it was completed.  The four elements are:
            the array containing the data, the source process rank (an
            integer), the message tag (an integer), and the number of
            elements that were received (an integer).

            Once a test has been successful (i.e. the operation has
            completed), it is no longer possible to call wait() or
            test() on the MPI Request object.
            """
            pass
        
    world = MPICommunicator()
    world.rank = 0
    world.size = 1

    if 1:

        class MPIOperationObject:
            pass
        
        class max(MPIOperationObject):
            """The 'maximum' operation in reduce/allreduce communications."""
            pass

        class min(MPIOperationObject):
            """The 'minimum' operation in reduce/allreduce communications."""
            pass

        class prod(MPIOperationObject):
            """The 'product' operation in reduce/allreduce communications."""
            pass

        class sum(MPIOperationObject):
            """The 'sum' operation in reduce/allreduce communications."""
            pass

        class land(MPIOperationObject):
            """The 'logical and' operation in reduce/allreduce communications."""
            pass

        class lor(MPIOperationObject):
            """The 'logical or' operation in reduce/allreduce communications."""
            pass

        class lxor(MPIOperationObject):
            """The 'logical exclusive-or' operation."""
            pass

        class band(MPIOperationObject):
            """The 'bitwise and' operation in reduce/allreduce communications."""
            pass

        class bor(MPIOperationObject):
            """The 'bitwise or' operation in reduce/allreduce communications."""
            pass

        class bxor(MPIOperationObject):
            """The 'bitwise exclusive-or' operation."""
            pass

        class maxloc(MPIOperationObject):
            """The 'location of the maximum' operation."""
            pass

        class minloc(MPIOperationObject):
            """The 'location of the minimum' operation."""
            pass

        class replace(MPIOperationObject):
            """The 'replace' operation. (MPI 2.0)"""
            pass

    _C_API = None

else:

    try:
        from Scientific_mpi import *
        from Scientific_mpi import _C_API, _registerErrorObject
        _registerErrorObject(MPIError)
        del _registerErrorObject
    except ImportError:

        import Scientific.N as Numeric

        _C_API = None

        class DummyCommunicator:

            def __init__(self):
                self.size = 1
                self.rank = 0
                self.messages = []

            def duplicate(self):
                return DummyCommunicator()

            def send(self, data, destination, tag):
                if destination != 0:
                    raise MPIError("invalid MPI destination")
                self.messages.append((tag, Numeric.array(data, copy=1).ravel()))

            def nonblockingSend(self, data, destination, tag):
                self.send(data, destination, tag)
                return DummyRequest(None)

            def receive(self, array, source=None, tag=None):
                if source != 0 and source != None:
                    raise MPIError("invalid MPI source")
                for i in range(len(self.messages)):
                    data_tag, data = self.messages[i]
                    if tag is None or tag == data_tag:
                        del self.messages[i]
                        return data, 0, data_tag, len(data)
                raise MPIError("no message received")

            def receiveString(self, source=None, tag=None):
                array, source, tag, length = self.receive(source, tag)
                return array.tostring(), source, tag

            def nonblockingReceive(self, array, source=None, tag=None):
                return DummyRequest(self.receive(array, source, tag))

            def nonblockingProbe(self, source=None, tag=None):
                if source != 0 and source != None:
                    raise MPIError, "invalid MPI source"
                for i in range(len(self.messages)):
                    data_tag, data = self.messages[i]
                    if tag is None or tag == data_tag:
                        return 0, data_tag
                return None

            def broadcast(self, array, root):
                if root != 0:
                    raise MPIError("invalid MPI rank")
                return array

            def share(self, send, receive):
                receive[0] = send

            def barrier(self):
                pass

            def abort(self):
                raise MPIError("abort")

        class DummyRequest:

            def __init__(self, arg):
                self.arg = arg

            def wait(self):
                return self.arg

        world = DummyCommunicator()

del sys
