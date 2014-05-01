#ifndef QPID_CLUSTER_QUORUM_CMAN_H
#define QPID_CLUSTER_QUORUM_CMAN_H

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

#include <qpid/sys/DispatchHandle.h>
#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>
#include <memory>

extern "C" {
#include <libcman.h>
}

namespace qpid {
namespace sys {
class Poller;
}

namespace cluster {
class Cluster;

class Quorum {
  public:
    Quorum(boost::function<void ()> onError);
    ~Quorum();
    void start(boost::shared_ptr<sys::Poller>);
    
  private:
    void dispatch(sys::DispatchHandle&);
    void disconnect(sys::DispatchHandle&);
    int getFd();
    void watch(int fd);
    
    bool enable;
    cman_handle_t cman;
    int cmanFd;
    std::auto_ptr<sys::DispatchHandleRef> dispatchHandle;
    boost::shared_ptr<sys::Poller> poller;
};


}} // namespace qpid::cluster

#endif  /*!QPID_CLUSTER_QUORUM_CMAN_H*/
