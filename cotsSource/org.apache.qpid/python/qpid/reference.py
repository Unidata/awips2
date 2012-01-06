#!/usr/bin/env python

#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
# 
#   http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

"""
Support for amqp 'reference' content (as opposed to inline content)
"""

import threading
from queue import Queue, Closed

class NotOpened(Exception): pass

class AlreadyOpened(Exception): pass

"""
A representation of a reference id; can be passed wherever amqp
content is required in place of inline data
"""
class ReferenceId:

    def __init__(self, id):
        self.id = id

"""
Holds content received through 'reference api'. Instances of this
class will be placed in the consumers queue on receiving a transfer
(assuming the reference has been opened). Data can be retrieved in
chunks (as append calls are received) or in full (after reference has
been closed signalling data s complete).
"""

class Reference:

    def __init__(self, id):
        self.id = id
        self.chunks = Queue(0)

    def close(self):
        self.chunks.close()

    def append(self, bytes):    
        self.chunks.put(bytes)

    def get_chunk(self):
        return self.chunks.get()

    def get_complete(self):
        data = ""
        for chunk in self:
            data += chunk
        return data

    def next(self):
        try:
            return self.get_chunk()
        except Closed, e:
            raise StopIteration

    def __iter__(self):
        return self

"""
Manages a set of opened references. New references can be opened and
existing references can be retrieved or closed.
"""
class References:

    def __init__(self):
        self.map = {}
        self.lock = threading.Lock()

    def get(self, id):
        self.lock.acquire()
        try:
            try:
                ref = self.map[id]
            except KeyError:
                raise NotOpened()
        finally:
            self.lock.release()
        return ref

    def open(self, id):
        self.lock.acquire()
        try:
            if id in self.map: raise AlreadyOpened()
            self.map[id] = Reference(id)
        finally:
            self.lock.release()


    def close(self, id):
        self.get(id).close()
        self.lock.acquire()
        try:
            self.map.pop(id)
        finally:
            self.lock.release()
        
