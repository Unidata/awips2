/*
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
 */

#include "qmf/engine/ResilientConnection.h"
#include "qmf/engine/MessageImpl.h"
#include "qmf/engine/ConnectionSettingsImpl.h"
#include <qpid/client/Connection.h>
#include <qpid/client/Session.h>
#include <qpid/client/MessageListener.h>
#include <qpid/client/SubscriptionManager.h>
#include <qpid/client/Message.h>
#include <qpid/sys/Thread.h>
#include <qpid/sys/Runnable.h>
#include <qpid/sys/Mutex.h>
#include <qpid/sys/Condition.h>
#include <qpid/sys/Time.h>
#include <qpid/log/Statement.h>
#include <qpid/RefCounted.h>
#include <boost/bind.hpp>
#include <string>
#include <deque>
#include <vector>
#include <set>
#include <boost/intrusive_ptr.hpp>
#include <boost/noncopyable.hpp>

using namespace std;
using namespace qmf::engine;
using namespace qpid;
using qpid::sys::Mutex;

namespace qmf {
namespace engine {
    struct ResilientConnectionEventImpl {
        ResilientConnectionEvent::EventKind  kind;
        void*       sessionContext;
        string      errorText;
        MessageImpl message;

        ResilientConnectionEventImpl(ResilientConnectionEvent::EventKind k,
                                     const MessageImpl& m = MessageImpl()) :
            kind(k), sessionContext(0), message(m) {}
        ResilientConnectionEvent copy();
    };

    struct RCSession : public client::MessageListener, public qpid::sys::Runnable, public qpid::RefCounted {
        typedef boost::intrusive_ptr<RCSession> Ptr;
        ResilientConnectionImpl& connImpl;
        string name;
        client::Connection& connection;
        client::Session session;
        client::SubscriptionManager* subscriptions;
        string userId;
        void* userContext;
        vector<string> dests;
        qpid::sys::Thread thread;

        RCSession(ResilientConnectionImpl& ci, const string& n, client::Connection& c, void* uc);
        ~RCSession();
        void received(client::Message& msg);
        void run();
        void stop();
    };

    class ResilientConnectionImpl : public qpid::sys::Runnable, public boost::noncopyable {
    public:
        ResilientConnectionImpl(const ConnectionSettings& settings);
        ~ResilientConnectionImpl();

        bool isConnected() const;
        bool getEvent(ResilientConnectionEvent& event);
        void popEvent();
        bool createSession(const char* name, void* sessionContext, SessionHandle& handle);
        void destroySession(SessionHandle handle);
        void sendMessage(SessionHandle handle, qmf::engine::Message& message);
        void declareQueue(SessionHandle handle, char* queue);
        void deleteQueue(SessionHandle handle, char* queue);
        void bind(SessionHandle handle, char* exchange, char* queue, char* key);
        void unbind(SessionHandle handle, char* exchange, char* queue, char* key);
        void setNotifyFd(int fd);

        void run();
        void failure();
        void sessionClosed(RCSession* sess);

        void EnqueueEvent(ResilientConnectionEvent::EventKind kind,
                          void*              sessionContext = 0,
                          const MessageImpl& message = MessageImpl(),
                          const string&      errorText = "");

    private:
        int  notifyFd;
        bool connected;
        bool shutdown;
        string lastError;
        const ConnectionSettings settings;
        client::Connection connection;
        mutable qpid::sys::Mutex lock;
        int delayMin;
        int delayMax;
        int delayFactor;
        qpid::sys::Condition cond;
        deque<ResilientConnectionEventImpl> eventQueue;
        set<RCSession::Ptr> sessions;
        qpid::sys::Thread connThread;
    };
}
}

ResilientConnectionEvent ResilientConnectionEventImpl::copy()
{
    ResilientConnectionEvent item;

    ::memset(&item, 0, sizeof(ResilientConnectionEvent));
    item.kind = kind;
    item.sessionContext = sessionContext;
    item.message        = message.copy();
    item.errorText      = const_cast<char*>(errorText.c_str());

    return item;
}

RCSession::RCSession(ResilientConnectionImpl& ci, const string& n, client::Connection& c, void* uc) :
            connImpl(ci), name(n), connection(c), session(connection.newSession(name)),
            subscriptions(new client::SubscriptionManager(session)), userContext(uc), thread(*this)
{
    const qpid::client::ConnectionSettings& operSettings = connection.getNegotiatedSettings();
    userId = operSettings.username;
}

RCSession::~RCSession()
{
    subscriptions->stop();
    thread.join();
    session.close();
    delete subscriptions;
}

void RCSession::run()
{
    try {
        subscriptions->run();
    } catch (exception& /*e*/) {
        connImpl.sessionClosed(this);
    }
}

void RCSession::stop()
{
    subscriptions->stop();
}

void RCSession::received(client::Message& msg)
{
    MessageImpl qmsg;
    qmsg.body = msg.getData();

    qpid::framing::DeliveryProperties dp = msg.getDeliveryProperties();
    if (dp.hasRoutingKey()) {
        qmsg.routingKey = dp.getRoutingKey();
    }

    qpid::framing::MessageProperties mp = msg.getMessageProperties();
    if (mp.hasReplyTo()) {
        const qpid::framing::ReplyTo& rt = mp.getReplyTo();
        qmsg.replyExchange = rt.getExchange();
        qmsg.replyKey = rt.getRoutingKey();
    }

    if (mp.hasUserId()) {
        qmsg.userId = mp.getUserId();
    }

    connImpl.EnqueueEvent(ResilientConnectionEvent::RECV, userContext, qmsg);
}

ResilientConnectionImpl::ResilientConnectionImpl(const ConnectionSettings& _settings) :
    notifyFd(-1), connected(false), shutdown(false), settings(_settings), delayMin(1), connThread(*this)
{
    connection.registerFailureCallback(boost::bind(&ResilientConnectionImpl::failure, this));
    settings.impl->getRetrySettings(&delayMin, &delayMax, &delayFactor);
}

ResilientConnectionImpl::~ResilientConnectionImpl()
{
    shutdown = true;
    connected = false;
    cond.notify();
    connThread.join();
    connection.close();
}

bool ResilientConnectionImpl::isConnected() const
{
    Mutex::ScopedLock _lock(lock);
    return connected;
}

bool ResilientConnectionImpl::getEvent(ResilientConnectionEvent& event)
{
    Mutex::ScopedLock _lock(lock);
    if (eventQueue.empty())
        return false;
    event = eventQueue.front().copy();
    return true;
}

void ResilientConnectionImpl::popEvent()
{
    Mutex::ScopedLock _lock(lock);
    if (!eventQueue.empty())
        eventQueue.pop_front();
}

bool ResilientConnectionImpl::createSession(const char* name, void* sessionContext,
                                            SessionHandle& handle)
{
    Mutex::ScopedLock _lock(lock);
    if (!connected)
        return false;

    RCSession::Ptr sess = RCSession::Ptr(new RCSession(*this, name, connection, sessionContext));

    handle.impl = (void*) sess.get();
    sessions.insert(sess);

    return true;
}

void ResilientConnectionImpl::destroySession(SessionHandle handle)
{
    Mutex::ScopedLock _lock(lock);
    RCSession::Ptr sess = RCSession::Ptr((RCSession*) handle.impl);
    set<RCSession::Ptr>::iterator iter = sessions.find(sess);
    if (iter != sessions.end()) {
        for (vector<string>::iterator dIter = sess->dests.begin(); dIter != sess->dests.end(); dIter++)
            sess->subscriptions->cancel(dIter->c_str());
        sess->subscriptions->stop();
        sess->subscriptions->wait();

        sessions.erase(iter);
        return;
    }
}

void ResilientConnectionImpl::sendMessage(SessionHandle handle, qmf::engine::Message& message)
{
    Mutex::ScopedLock _lock(lock);
    RCSession::Ptr sess = RCSession::Ptr((RCSession*) handle.impl);
    set<RCSession::Ptr>::iterator iter = sessions.find(sess);
    qpid::client::Message msg;
    string data(message.body, message.length);
    msg.getDeliveryProperties().setRoutingKey(message.routingKey);
    msg.getMessageProperties().setReplyTo(qpid::framing::ReplyTo(message.replyExchange, message.replyKey));
    if (settings.impl->getSendUserId())
        msg.getMessageProperties().setUserId(sess->userId);
    msg.setData(data);

    try {
        sess->session.messageTransfer(client::arg::content=msg, client::arg::destination=message.destination);
    } catch(exception& e) {
        QPID_LOG(error, "Session Exception during message-transfer: " << e.what());
        sessions.erase(iter);
        EnqueueEvent(ResilientConnectionEvent::SESSION_CLOSED, (*iter)->userContext);
    }
}

void ResilientConnectionImpl::declareQueue(SessionHandle handle, char* queue)
{
    Mutex::ScopedLock _lock(lock);
    RCSession* sess = (RCSession*) handle.impl;

    sess->session.queueDeclare(client::arg::queue=queue, client::arg::autoDelete=true, client::arg::exclusive=true);
	sess->subscriptions->setAcceptMode(client::ACCEPT_MODE_NONE);
	sess->subscriptions->setAcquireMode(client::ACQUIRE_MODE_PRE_ACQUIRED);
    sess->subscriptions->subscribe(*sess, queue, queue);
	sess->subscriptions->setFlowControl(queue, client::FlowControl::unlimited());
    sess->dests.push_back(string(queue));
}

void ResilientConnectionImpl::deleteQueue(SessionHandle handle, char* queue)
{
    Mutex::ScopedLock _lock(lock);
    RCSession* sess = (RCSession*) handle.impl;

    sess->session.queueDelete(client::arg::queue=queue);
    for (vector<string>::iterator iter = sess->dests.begin();
         iter != sess->dests.end(); iter++)
        if (*iter == queue) {
            sess->subscriptions->cancel(queue);
            sess->dests.erase(iter);
            break;
        }
}

void ResilientConnectionImpl::bind(SessionHandle handle,
                                   char* exchange, char* queue, char* key)
{
    Mutex::ScopedLock _lock(lock);
    RCSession* sess = (RCSession*) handle.impl;

    sess->session.exchangeBind(client::arg::exchange=exchange, client::arg::queue=queue, client::arg::bindingKey=key);
}

void ResilientConnectionImpl::unbind(SessionHandle handle,
                                     char* exchange, char* queue, char* key)
{
    Mutex::ScopedLock _lock(lock);
    RCSession* sess = (RCSession*) handle.impl;

    sess->session.exchangeUnbind(client::arg::exchange=exchange, client::arg::queue=queue, client::arg::bindingKey=key);
}

void ResilientConnectionImpl::setNotifyFd(int fd)
{
    notifyFd = fd;
}

void ResilientConnectionImpl::run()
{
    int delay(delayMin);

    while (true) {
        try {
            QPID_LOG(trace, "Trying to open connection...");
            connection.open(settings.impl->getClientSettings());
            {
                Mutex::ScopedLock _lock(lock);
                connected = true;
                EnqueueEvent(ResilientConnectionEvent::CONNECTED);

                while (connected)
                    cond.wait(lock);
                delay = delayMin;

                while (!sessions.empty()) {
                    set<RCSession::Ptr>::iterator iter = sessions.begin();
                    RCSession::Ptr sess = *iter;
                    sessions.erase(iter);
                    EnqueueEvent(ResilientConnectionEvent::SESSION_CLOSED, sess->userContext);
                    Mutex::ScopedUnlock _u(lock);
                    sess->stop();

                    // Nullify the intrusive pointer within the scoped unlock, otherwise,
                    // the reference is held until overwritted above (under lock) which causes
                    // the session destructor to be called with the lock held.
                    sess = 0;
                }

                EnqueueEvent(ResilientConnectionEvent::DISCONNECTED);

                if (shutdown)
                    return;
            }
            connection.close();
        } catch (exception &e) {
            QPID_LOG(debug, "connection.open exception: " << e.what());
            Mutex::ScopedLock _lock(lock);
            lastError = e.what();
            if (delay < delayMax)
                delay *= delayFactor;
        }

        ::qpid::sys::sleep(delay);
    }
}

void ResilientConnectionImpl::failure()
{
    Mutex::ScopedLock _lock(lock);

    connected = false;
    lastError = "Closed by Peer";
    cond.notify();
}

void ResilientConnectionImpl::sessionClosed(RCSession*)
{
    Mutex::ScopedLock _lock(lock);
    connected = false;
    lastError = "Closed due to Session failure";
    cond.notify();
}

void ResilientConnectionImpl::EnqueueEvent(ResilientConnectionEvent::EventKind kind,
                                           void* sessionContext,
                                           const MessageImpl& message,
                                           const string& errorText)
{
    Mutex::ScopedLock _lock(lock);
    ResilientConnectionEventImpl event(kind, message);

    event.sessionContext = sessionContext;
    event.errorText      = errorText;

    eventQueue.push_back(event);
    if (notifyFd != -1)
    {
        int unused_ret;    //Suppress warnings about ignoring return value.
        unused_ret = ::write(notifyFd, ".", 1);
    }
}


//==================================================================
// Wrappers
//==================================================================

ResilientConnection::ResilientConnection(const ConnectionSettings& settings)
{
    impl = new ResilientConnectionImpl(settings);
}

ResilientConnection::~ResilientConnection()
{
    delete impl;
}

bool ResilientConnection::isConnected() const
{
    return impl->isConnected();
}

bool ResilientConnection::getEvent(ResilientConnectionEvent& event)
{
    return impl->getEvent(event);
}

void ResilientConnection::popEvent()
{
    impl->popEvent();
}

bool ResilientConnection::createSession(const char* name, void* sessionContext, SessionHandle& handle)
{
    return impl->createSession(name, sessionContext, handle);
}

void ResilientConnection::destroySession(SessionHandle handle)
{
    impl->destroySession(handle);
}

void ResilientConnection::sendMessage(SessionHandle handle, qmf::engine::Message& message)
{
    impl->sendMessage(handle, message);
}

void ResilientConnection::declareQueue(SessionHandle handle, char* queue)
{
    impl->declareQueue(handle, queue);
}

void ResilientConnection::deleteQueue(SessionHandle handle, char* queue)
{
    impl->deleteQueue(handle, queue);
}

void ResilientConnection::bind(SessionHandle handle, char* exchange, char* queue, char* key)
{
    impl->bind(handle, exchange, queue, key);
}

void ResilientConnection::unbind(SessionHandle handle, char* exchange, char* queue, char* key)
{
    impl->unbind(handle, exchange, queue, key);
}

void ResilientConnection::setNotifyFd(int fd)
{
    impl->setNotifyFd(fd);
}

