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
#include "unit_test.h"
#include "test_tools.h"
#include "BrokerFixture.h"
#include "qpid/client/QueueOptions.h"
#include "qpid/client/MessageListener.h"
#include "qpid/client/SubscriptionManager.h"
#include "qpid/client/AsyncSession.h"
#include "qpid/sys/Monitor.h"
#include "qpid/sys/Thread.h"
#include "qpid/sys/Runnable.h"
#include "qpid/sys/Time.h"
#include "qpid/client/Session.h"
#include "qpid/client/Message.h"
#include "qpid/framing/reply_exceptions.h"

#include <boost/optional.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/bind.hpp>
#include <boost/ptr_container/ptr_vector.hpp>

#include <vector>

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(ClientSessionTest)

using namespace qpid::client;
using namespace qpid::framing;
using namespace qpid;
using qpid::sys::Monitor;
using qpid::sys::Thread;
using qpid::sys::TIME_SEC;
using qpid::broker::Broker;
using std::string;
using std::cout;
using std::endl;


struct DummyListener : public sys::Runnable, public MessageListener {
    std::vector<Message> messages;
    string name;
    uint expected;
    SubscriptionManager submgr;

    DummyListener(Session& session, const string& n, uint ex) :
        name(n), expected(ex), submgr(session) {}

    void run()
    {
        submgr.subscribe(*this, name);
        submgr.run();
    }

    void received(Message& msg)
    {
        messages.push_back(msg);
        if (--expected == 0) {
            submgr.stop();
        }
    }
};

struct SimpleListener : public MessageListener
{
    Monitor lock;
    std::vector<Message> messages;

    void received(Message& msg)
    {
        Monitor::ScopedLock l(lock);
        messages.push_back(msg);
        lock.notifyAll();
    }

    void waitFor(const uint n)
    {
        Monitor::ScopedLock l(lock);
        while (messages.size() < n) {
            lock.wait();
        }
    }
};

struct ClientSessionFixture : public ProxySessionFixture
{
    ClientSessionFixture(Broker::Options opts = Broker::Options()) : ProxySessionFixture(opts) {
        session.queueDeclare(arg::queue="my-queue");
    }
};

QPID_AUTO_TEST_CASE(testQueueQuery) {
    ClientSessionFixture fix;
    fix.session = fix.connection.newSession();
    fix.session.queueDeclare(arg::queue="q", arg::alternateExchange="amq.fanout",
                             arg::exclusive=true, arg::autoDelete=true);
    QueueQueryResult result = fix.session.queueQuery("q");
    BOOST_CHECK_EQUAL(false, result.getDurable());
    BOOST_CHECK_EQUAL(true, result.getExclusive());
    BOOST_CHECK_EQUAL("amq.fanout", result.getAlternateExchange());
}

QPID_AUTO_TEST_CASE(testDispatcher)
{
    ClientSessionFixture fix;
    fix.session =fix.connection.newSession();
    size_t count = 100;
    for (size_t i = 0; i < count; ++i)
        fix.session.messageTransfer(arg::content=Message(boost::lexical_cast<string>(i), "my-queue"));
    DummyListener listener(fix.session, "my-queue", count);
    listener.run();
    BOOST_CHECK_EQUAL(count, listener.messages.size());
    for (size_t i = 0; i < count; ++i)
        BOOST_CHECK_EQUAL(boost::lexical_cast<string>(i), listener.messages[i].getData());
}

QPID_AUTO_TEST_CASE(testDispatcherThread)
{
    ClientSessionFixture fix;
    fix.session =fix.connection.newSession();
    size_t count = 10;
    DummyListener listener(fix.session, "my-queue", count);
    sys::Thread t(listener);
    for (size_t i = 0; i < count; ++i) {
        fix.session.messageTransfer(arg::content=Message(boost::lexical_cast<string>(i), "my-queue"));
    }
    t.join();
    BOOST_CHECK_EQUAL(count, listener.messages.size());
    for (size_t i = 0; i < count; ++i)
        BOOST_CHECK_EQUAL(boost::lexical_cast<string>(i), listener.messages[i].getData());
}

// FIXME aconway 2009-06-17: test for unimplemented feature, enable when implemented.
void testSuspend0Timeout() {
    ClientSessionFixture fix;
    fix.session.suspend();  // session has 0 timeout.
    try {
        fix.connection.resume(fix.session);
        BOOST_FAIL("Expected InvalidArgumentException.");
    } catch(const InternalErrorException&) {}
}

QPID_AUTO_TEST_CASE(testUseSuspendedError)
{
    ClientSessionFixture fix;
    fix.session.timeout(60);
    fix.session.suspend();
    try {
        fix.session.exchangeQuery(arg::exchange="amq.fanout");
        BOOST_FAIL("Expected session suspended exception");
    } catch(const NotAttachedException&) {}
}

// FIXME aconway 2009-06-17: test for unimplemented feature, enable when implemented.
void testSuspendResume() {
    ClientSessionFixture fix;
    fix.session.timeout(60);
    fix.session.suspend();
    // Make sure we are still subscribed after resume.
    fix.connection.resume(fix.session);
    fix.session.messageTransfer(arg::content=Message("my-message", "my-queue"));
    BOOST_CHECK_EQUAL("my-message", fix.subs.get("my-queue", TIME_SEC).getData());
}


QPID_AUTO_TEST_CASE(testSendToSelf) {
    ClientSessionFixture fix;
    SimpleListener mylistener;
    fix.session.queueDeclare(arg::queue="myq", arg::exclusive=true, arg::autoDelete=true);
    fix.subs.subscribe(mylistener, "myq");
    sys::Thread runner(fix.subs);//start dispatcher thread
    string data("msg");
    Message msg(data, "myq");
    const uint count=10;
    for (uint i = 0; i < count; ++i) {
        fix.session.messageTransfer(arg::content=msg);
    }
    mylistener.waitFor(count);
    fix.subs.cancel("myq");
    fix.subs.stop();
    runner.join();
    fix.session.close();
    BOOST_CHECK_EQUAL(mylistener.messages.size(), count);
    for (uint j = 0; j < count; ++j) {
        BOOST_CHECK_EQUAL(mylistener.messages[j].getData(), data);
    }
}

QPID_AUTO_TEST_CASE(testLocalQueue) {
    ClientSessionFixture fix;
    fix.session.queueDeclare(arg::queue="lq", arg::exclusive=true, arg::autoDelete=true);
    LocalQueue lq;
    fix.subs.subscribe(lq, "lq", FlowControl(2, FlowControl::UNLIMITED, false));
    fix.session.messageTransfer(arg::content=Message("foo0", "lq"));
    fix.session.messageTransfer(arg::content=Message("foo1", "lq"));
    fix.session.messageTransfer(arg::content=Message("foo2", "lq"));
    BOOST_CHECK_EQUAL("foo0", lq.pop().getData());
    BOOST_CHECK_EQUAL("foo1", lq.pop().getData());
    BOOST_CHECK(lq.empty());    // Credit exhausted.
    fix.subs.getSubscription("lq").setFlowControl(FlowControl::unlimited());
    BOOST_CHECK_EQUAL("foo2", lq.pop().getData());
}

struct DelayedTransfer : sys::Runnable
{
    ClientSessionFixture& fixture;

    DelayedTransfer(ClientSessionFixture& f) : fixture(f) {}

    void run()
    {
        qpid::sys::sleep(1);
        fixture.session.messageTransfer(arg::content=Message("foo2", "getq"));
    }
};

QPID_AUTO_TEST_CASE(testGet) {
    ClientSessionFixture fix;
    fix.session.queueDeclare(arg::queue="getq", arg::exclusive=true, arg::autoDelete=true);
    fix.session.messageTransfer(arg::content=Message("foo0", "getq"));
    fix.session.messageTransfer(arg::content=Message("foo1", "getq"));
    Message got;
    BOOST_CHECK(fix.subs.get(got, "getq", TIME_SEC));
    BOOST_CHECK_EQUAL("foo0", got.getData());
    BOOST_CHECK(fix.subs.get(got, "getq", TIME_SEC));
    BOOST_CHECK_EQUAL("foo1", got.getData());
    BOOST_CHECK(!fix.subs.get(got, "getq"));
    DelayedTransfer sender(fix);
    Thread t(sender);
    //test timed get where message shows up after a short delay
    BOOST_CHECK(fix.subs.get(got, "getq", 5*TIME_SEC));
    BOOST_CHECK_EQUAL("foo2", got.getData());
    t.join();
}

QPID_AUTO_TEST_CASE(testOpenFailure) {
    BrokerFixture b;
    Connection c;
    string host("unknowable-host");
    try {
        c.open(host);
    } catch (const Exception&) {
        BOOST_CHECK(!c.isOpen());
    }
    b.open(c);
    BOOST_CHECK(c.isOpen());
    c.close();
    BOOST_CHECK(!c.isOpen());
}

QPID_AUTO_TEST_CASE(testPeriodicExpiration) {
    Broker::Options opts;
    opts.queueCleanInterval = 1;
    ClientSessionFixture fix(opts);
    fix.session.queueDeclare(arg::queue="my-queue", arg::exclusive=true, arg::autoDelete=true);

    for (uint i = 0; i < 10; i++) {
        Message m((boost::format("Message_%1%") % (i+1)).str(), "my-queue");
        if (i % 2) m.getDeliveryProperties().setTtl(500);
        fix.session.messageTransfer(arg::content=m);
    }

    BOOST_CHECK_EQUAL(fix.session.queueQuery(string("my-queue")).getMessageCount(), 10u);
    qpid::sys::sleep(2);
    BOOST_CHECK_EQUAL(fix.session.queueQuery(string("my-queue")).getMessageCount(), 5u);
}

QPID_AUTO_TEST_CASE(testExpirationOnPop) {
    ClientSessionFixture fix;
    fix.session.queueDeclare(arg::queue="my-queue", arg::exclusive=true, arg::autoDelete=true);

    for (uint i = 0; i < 10; i++) {
        Message m((boost::format("Message_%1%") % (i+1)).str(), "my-queue");
        if (i % 2) m.getDeliveryProperties().setTtl(200);
        fix.session.messageTransfer(arg::content=m);
    }

    qpid::sys::usleep(300* 1000);

    for (uint i = 0; i < 10; i++) {
        if (i % 2) continue;
        Message m;
        BOOST_CHECK(fix.subs.get(m, "my-queue", TIME_SEC));
        BOOST_CHECK_EQUAL((boost::format("Message_%1%") % (i+1)).str(), m.getData());
    }
}

QPID_AUTO_TEST_CASE(testRelease) {
    ClientSessionFixture fix;

    const uint count=10;
    for (uint i = 0; i < count; i++) {
        Message m((boost::format("Message_%1%") % (i+1)).str(), "my-queue");
        fix.session.messageTransfer(arg::content=m);
    }

    fix.subs.setAutoStop(false);
    fix.subs.start();
    SubscriptionSettings settings;
    settings.autoAck = 0;

    SimpleListener l1;
    Subscription s1 = fix.subs.subscribe(l1, "my-queue", settings);
    l1.waitFor(count);
    s1.cancel();

    for (uint i = 0; i < count; i++) {
        BOOST_CHECK_EQUAL((boost::format("Message_%1%") % (i+1)).str(), l1.messages[i].getData());
    }
    s1.release(s1.getUnaccepted());

    //check that released messages are redelivered
    settings.autoAck = 1;
    SimpleListener l2;
    Subscription s2 = fix.subs.subscribe(l2, "my-queue", settings);
    l2.waitFor(count);
    for (uint i = 0; i < count; i++) {
        BOOST_CHECK_EQUAL((boost::format("Message_%1%") % (i+1)).str(), l2.messages[i].getData());
    }

    fix.subs.stop();
    fix.subs.wait();
    fix.session.close();
}

QPID_AUTO_TEST_CASE(testCompleteOnAccept) {
    ClientSessionFixture fix;
    const uint count = 8;
    const uint chunk = 4;
    for (uint i = 0; i < count; i++) {
        Message m((boost::format("Message_%1%") % (i+1)).str(), "my-queue");
        fix.session.messageTransfer(arg::content=m);
    }

    SubscriptionSettings settings;
    settings.autoAck = 0;
    settings.completionMode = COMPLETE_ON_ACCEPT;
    settings.flowControl = FlowControl::messageWindow(chunk);

    LocalQueue q;
    Subscription s = fix.subs.subscribe(q, "my-queue", settings);
    fix.session.messageFlush(arg::destination=s.getName());
    SequenceSet accepted;
    for (uint i = 0; i < chunk; i++) {
        Message m;
        BOOST_CHECK(q.get(m));
        BOOST_CHECK_EQUAL((boost::format("Message_%1%") % (i+1)).str(), m.getData());
        accepted.add(m.getId());
    }
    Message m;
    BOOST_CHECK(!q.get(m));

    s.accept(accepted);
    fix.session.messageFlush(arg::destination=s.getName());
    accepted.clear();

    for (uint i = chunk; i < count; i++) {
        Message m;
        BOOST_CHECK(q.get(m));
        BOOST_CHECK_EQUAL((boost::format("Message_%1%") % (i+1)).str(), m.getData());
        accepted.add(m.getId());
    }
    fix.session.messageAccept(accepted);
}

namespace
{
struct Publisher : qpid::sys::Runnable
{
    AsyncSession session;
    Message message;
    uint count;
    Thread thread;

    Publisher(Connection& con, Message m, uint c) : session(con.newSession()), message(m), count(c) {}

    void start()
    {
        thread = Thread(*this);
    }

    void join()
    {
        thread.join();
    }

    void run()
    {
        for (uint i = 0; i < count; i++) {
            session.messageTransfer(arg::content=message);
        }
        session.sync();
        session.close();
    }
};
}

QPID_AUTO_TEST_CASE(testConcurrentSenders)
{
    //Ensure concurrent publishing sessions on a connection don't
    //cause assertions, deadlocks or other undesirables:
    BrokerFixture fix;
    Connection connection;
    ConnectionSettings settings;
    settings.maxFrameSize = 1024;
    settings.port = fix.broker->getPort(qpid::broker::Broker::TCP_TRANSPORT);
    connection.open(settings);
    AsyncSession session = connection.newSession();
    Message message(string(512, 'X'));

    boost::ptr_vector<Publisher> publishers;
    for (size_t i = 0; i < 5; i++) {
        publishers.push_back(new Publisher(connection, message, 100));
    }
    std::for_each(publishers.begin(), publishers.end(), boost::bind(&Publisher::start, _1));
    std::for_each(publishers.begin(), publishers.end(), boost::bind(&Publisher::join, _1));
    connection.close();
}


QPID_AUTO_TEST_CASE(testExclusiveSubscribe)
{
    ClientSessionFixture fix;
    fix.session.queueDeclare(arg::queue="myq", arg::exclusive=true, arg::autoDelete=true);
    SubscriptionSettings settings;
    settings.exclusive = true;
    LocalQueue q;
    fix.subs.subscribe(q, "myq", settings, "first");
    //attempt to create new subscriber should fail
    ScopedSuppressLogging sl;
    BOOST_CHECK_THROW(fix.subs.subscribe(q, "myq", "second"), ResourceLockedException);
    ;

}

QPID_AUTO_TEST_CASE(testExclusiveBinding) {
    FieldTable options;
    options.setString("qpid.exclusive-binding", "anything");
    ClientSessionFixture fix;
    fix.session.queueDeclare(arg::queue="queue-1", arg::exclusive=true, arg::autoDelete=true);
    fix.session.queueDeclare(arg::queue="queue-2", arg::exclusive=true, arg::autoDelete=true);
    fix.session.exchangeBind(arg::exchange="amq.direct", arg::queue="queue-1", arg::bindingKey="my-key", arg::arguments=options);
    fix.session.messageTransfer(arg::destination="amq.direct", arg::content=Message("message1", "my-key"));
    fix.session.exchangeBind(arg::exchange="amq.direct", arg::queue="queue-2", arg::bindingKey="my-key", arg::arguments=options);
    fix.session.messageTransfer(arg::destination="amq.direct", arg::content=Message("message2", "my-key"));

    Message got;
    BOOST_CHECK(fix.subs.get(got, "queue-1"));
    BOOST_CHECK_EQUAL("message1", got.getData());
    BOOST_CHECK(!fix.subs.get(got, "queue-1"));

    BOOST_CHECK(fix.subs.get(got, "queue-2"));
    BOOST_CHECK_EQUAL("message2", got.getData());
    BOOST_CHECK(!fix.subs.get(got, "queue-2"));
}

QPID_AUTO_TEST_CASE(testResubscribeWithLocalQueue) {
    ClientSessionFixture fix;
    fix.session.queueDeclare(arg::queue="some-queue", arg::exclusive=true, arg::autoDelete=true);
    LocalQueue p, q;
    fix.subs.subscribe(p, "some-queue");
    fix.subs.cancel("some-queue");
    fix.subs.subscribe(q, "some-queue");

    fix.session.messageTransfer(arg::content=Message("some-data", "some-queue"));
    fix.session.messageFlush(arg::destination="some-queue");

    Message got;
    BOOST_CHECK(!p.get(got));

    BOOST_CHECK(q.get(got));
    BOOST_CHECK_EQUAL("some-data", got.getData());
    BOOST_CHECK(!q.get(got));
}

QPID_AUTO_TEST_CASE(testReliableDispatch) {
    ClientSessionFixture fix;
    std::string queue("a-queue");
    fix.session.queueDeclare(arg::queue=queue, arg::autoDelete=true);

    ConnectionSettings settings;
    settings.port = fix.broker->getPort(qpid::broker::Broker::TCP_TRANSPORT);

    Connection c1;
    c1.open(settings);
    Session s1 = c1.newSession();
    SubscriptionManager subs1(s1);
    LocalQueue q1;
    subs1.subscribe(q1, queue, FlowControl());//first subscriber has no credit

    Connection c2;
    c2.open(settings);
    Session s2 = c2.newSession();
    SubscriptionManager subs2(s2);
    LocalQueue q2;
    subs2.subscribe(q2, queue);//second subscriber has credit

    fix.session.messageTransfer(arg::content=Message("my-message", queue));

    //check that the second consumer gets the message
    Message got;
    BOOST_CHECK(q2.get(got, 1*TIME_SEC));
    BOOST_CHECK_EQUAL("my-message", got.getData());

    c1.close();
    c2.close();
}

QPID_AUTO_TEST_CASE(testSessionCloseOnInvalidSession) {
    Session session;
    session.close();
}

QPID_AUTO_TEST_CASE(testLVQVariedSize) {
    ClientSessionFixture fix;
    std::string queue("my-lvq");
    QueueOptions args;
    args.setOrdering(LVQ_NO_BROWSE);
    fix.session.queueDeclare(arg::queue=queue, arg::exclusive=true, arg::autoDelete=true, arg::arguments=args);

    std::string key;
    args.getLVQKey(key);

    for (size_t i = 0; i < 10; i++) {
        std::ostringstream data;
        size_t size = 100 - ((i % 10) * 10);
        data << std::string(size, 'x');

        Message m(data.str(), queue);
        m.getHeaders().setString(key, "abc");
        fix.session.messageTransfer(arg::content=m);
    }
}

QPID_AUTO_TEST_CASE(testSessionManagerSetFlowControl) {
    ClientSessionFixture fix;
    std::string name("dummy");
    LocalQueue queue;
    SubscriptionSettings settings;
    settings.flowControl = FlowControl();
    fix.session.queueDeclare(arg::queue=name, arg::exclusive=true, arg::autoDelete=true);
    fix.subs.subscribe(queue, name, settings);
    fix.session.messageTransfer(arg::content=Message("my-message", name));
    fix.subs.setFlowControl(name, 1, FlowControl::UNLIMITED, false);
    fix.session.messageFlush(name);
    Message got;
    BOOST_CHECK(queue.get(got, 0));
    BOOST_CHECK_EQUAL("my-message", got.getData());
}

QPID_AUTO_TEST_CASE(testGetThenSubscribe) {
    ClientSessionFixture fix;
    std::string name("myqueue");
    fix.session.queueDeclare(arg::queue=name, arg::exclusive=true, arg::autoDelete=true);
    fix.session.messageTransfer(arg::content=Message("one", name));
    fix.session.messageTransfer(arg::content=Message("two", name));
    Message got;
    BOOST_CHECK(fix.subs.get(got, name));
    BOOST_CHECK_EQUAL("one", got.getData());

    DummyListener listener(fix.session, name, 1);
    listener.run();
    BOOST_CHECK_EQUAL(1u, listener.messages.size());
    if (!listener.messages.empty()) {
        BOOST_CHECK_EQUAL("two", listener.messages[0].getData());
    }
}

QPID_AUTO_TEST_CASE(testSessionIsValid) {
    ClientSessionFixture fix;
    BOOST_CHECK(fix.session.isValid());
    Session session;
    BOOST_CHECK(!session.isValid());
}

QPID_AUTO_TEST_CASE(testExpirationNotAltered) {
    ClientSessionFixture fix;
    fix.session.queueDeclare(arg::queue="my-queue", arg::exclusive=true, arg::autoDelete=true);

    Message m("my-message", "my-queue");
    m.getDeliveryProperties().setTtl(60000);
    m.getDeliveryProperties().setExpiration(12345);
    fix.session.messageTransfer(arg::content=m);
    Message got;
    BOOST_CHECK(fix.subs.get(got, "my-queue"));
    BOOST_CHECK_EQUAL("my-message", got.getData());
    BOOST_CHECK_EQUAL(12345u, got.getDeliveryProperties().getExpiration());
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
