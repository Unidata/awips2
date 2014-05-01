#ifndef QPID_CLUSTER_LOCKEDCONNECTIONMAP_H
#define QPID_CLUSTER_LOCKEDCONNECTIONMAP_H

/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * 
 *   http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */

#include "qpid/cluster/types.h"
#include "qpid/sys/Mutex.h"
#include "qpid/cluster/Connection.h"

namespace qpid {
namespace cluster {

/**
 * Thread safe map of connections.
 */
class LockedConnectionMap
{
  public:
    void insert(const ConnectionPtr& c) {
        sys::Mutex::ScopedLock l(lock);
        assert(map.find(c->getId()) == map.end());
        map[c->getId()] = c;
    }
    
    ConnectionPtr getErase(const ConnectionId& c) {
        sys::Mutex::ScopedLock l(lock);
        Map::iterator i = map.find(c);
        if (i != map.end()) {
            ConnectionPtr cp = i->second;
            map.erase(i);
            return cp;
        }
        else
            return 0;
    }

    void clear() { sys::Mutex::ScopedLock l(lock); map.clear(); }

  private:
    typedef std::map<ConnectionId, ConnectionPtr> Map;
    mutable sys::Mutex lock;
    Map map;
};
}} // namespace qpid::cluster

#endif  /*!QPID_CLUSTER_LOCKEDCONNECTIONMAP_H*/
