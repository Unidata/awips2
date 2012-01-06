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
#ifndef _Dispatcher_
#define _Dispatcher_

#include <map>
#include <memory>
#include <string>
#include <boost/shared_ptr.hpp>
#include "qpid/client/Session.h"
#include "qpid/client/SessionBase_0_10Access.h"
#include "qpid/sys/Mutex.h"
#include "qpid/sys/Runnable.h"
#include "qpid/sys/Thread.h"
#include "qpid/client/ClientImportExport.h"
#include "qpid/client/MessageListener.h"
#include "qpid/client/SubscriptionImpl.h"

namespace qpid {
namespace client {

class SubscriptionImpl;

///@internal
typedef framing::Handler<framing::FrameSet> FrameSetHandler;

///@internal
class Dispatcher : public sys::Runnable
{
    typedef std::map<std::string, boost::intrusive_ptr<SubscriptionImpl>  >Listeners;
    sys::Mutex lock;
    sys::Thread worker;
    Session session;
    Demux::QueuePtr queue;
    bool running;
    bool autoStop;
    Listeners listeners;
    boost::intrusive_ptr<SubscriptionImpl> defaultListener;
    std::auto_ptr<FrameSetHandler> handler;

    boost::intrusive_ptr<SubscriptionImpl> find(const std::string& name);
    bool isStopped();

    boost::function<void ()> failoverHandler;

public:
    Dispatcher(const Session& session, const std::string& queue = "");
    ~Dispatcher() {}

    void start();
    void wait();
    // As this class is marked 'internal', no extern should be made here;
    // however, some test programs rely on it.
    QPID_CLIENT_EXTERN void run();
    void stop();
    void setAutoStop(bool b);

    void registerFailoverHandler ( boost::function<void ()> fh )
    {
      failoverHandler = fh;
    }

    void listen(const boost::intrusive_ptr<SubscriptionImpl>& subscription);
    void cancel(const std::string& destination);
};

}}

#endif
