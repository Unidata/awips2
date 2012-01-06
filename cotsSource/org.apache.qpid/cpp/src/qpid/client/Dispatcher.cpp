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
#include "qpid/client/Dispatcher.h"
#include "qpid/client/SubscriptionImpl.h"
#include "qpid/client/SessionImpl.h"

#include "qpid/framing/FrameSet.h"
#include "qpid/framing/MessageTransferBody.h"
#include "qpid/log/Statement.h"
#include "qpid/sys/BlockingQueue.h"
#include "qpid/client/Message.h"
#include "qpid/client/MessageImpl.h"

#include <boost/version.hpp>
#if (BOOST_VERSION >= 104000)
#  include <boost/serialization/state_saver.hpp>
  using boost::serialization::state_saver;
#else
#  include <boost/state_saver.hpp>
  using boost::state_saver;
#endif /* BOOST_VERSION */

using qpid::framing::FrameSet;
using qpid::framing::MessageTransferBody;
using qpid::sys::Mutex;
using qpid::sys::ScopedLock;
using qpid::sys::Thread;

namespace qpid {
namespace client {

Dispatcher::Dispatcher(const Session& s, const std::string& q)
    : session(s), 
      running(false), 
      autoStop(true),
      failoverHandler(0)
{
    Demux& demux = SessionBase_0_10Access(session).get()->getDemux();
    queue = q.empty() ? demux.getDefault() : demux.get(q); 
}    

void Dispatcher::start()
{
    worker = Thread(this);
}

void Dispatcher::wait()
{
    worker.join();
}

void Dispatcher::run()
{
    Mutex::ScopedLock l(lock);
    if (running) 
        throw Exception("Dispatcher is already running.");
    state_saver<bool>  reset(running); // Reset to false on exit.
    running = true;
    try {
        while (!queue->isClosed()) {
            Mutex::ScopedUnlock u(lock);
            FrameSet::shared_ptr content = queue->pop();
            if (content->isA<MessageTransferBody>()) {
                Message msg(new MessageImpl(*content));
                boost::intrusive_ptr<SubscriptionImpl> listener = find(msg.getDestination());
                if (!listener) {
                    QPID_LOG(error, "No listener found for destination " << msg.getDestination());
                } else {
                    assert(listener);
                    listener->received(msg);
                }
            } else {
                if (handler.get()) {
                    handler->handle(*content);
                } else {
                    QPID_LOG(warning, "No handler found for " << *(content->getMethod()));
                }
            }
        }
        session.sync(); // Make sure all our acks are received before returning.
    }
    catch (const ClosedException&) {
        QPID_LOG(debug, QPID_MSG(session.getId() << ": closed by peer"));
    } 
    catch (const TransportFailure&) {
        QPID_LOG(info, QPID_MSG(session.getId() << ": transport failure"));
        throw;
    }
    catch (const std::exception& e) {
        if ( failoverHandler ) {
            QPID_LOG(debug, QPID_MSG(session.getId() << " failover: " << e.what()));
            failoverHandler();
        } else {
            QPID_LOG(error, session.getId() << " error: " << e.what());
            throw;
        }
    }
}

void Dispatcher::stop()
{
    ScopedLock<Mutex> l(lock);
    queue->close();             // Will interrupt thread blocked in pop()
}

void Dispatcher::setAutoStop(bool b)
{
    ScopedLock<Mutex> l(lock);
    autoStop = b;
}

boost::intrusive_ptr<SubscriptionImpl> Dispatcher::find(const std::string& name)
{
    ScopedLock<Mutex> l(lock);
    Listeners::iterator i = listeners.find(name);
    if (i == listeners.end()) {
        return defaultListener;
    }
    return i->second;
}

void Dispatcher::listen(const boost::intrusive_ptr<SubscriptionImpl>& subscription) {
    ScopedLock<Mutex> l(lock);
    listeners[subscription->getName()] = subscription;
}

void Dispatcher::cancel(const std::string& destination) {
    ScopedLock<Mutex> l(lock);
    if (listeners.erase(destination) && running && autoStop && listeners.empty())
        queue->close();
}

}}
