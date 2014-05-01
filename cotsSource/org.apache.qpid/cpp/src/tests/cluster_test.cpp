/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "test_tools.h"
#include "unit_test.h"
#include "ForkedBroker.h"
#include "BrokerFixture.h"
#include "ClusterFixture.h"

#include "qpid/client/Connection.h"
#include "qpid/client/ConnectionSettings.h"
#include "qpid/client/ConnectionAccess.h"
#include "qpid/client/Session.h"
#include "qpid/client/FailoverListener.h"
#include "qpid/client/FailoverManager.h"
#include "qpid/client/QueueOptions.h"
#include "qpid/cluster/Cluster.h"
#include "qpid/cluster/Cpg.h"
#include "qpid/cluster/UpdateClient.h"
#include "qpid/framing/AMQBody.h"
#include "qpid/framing/Uuid.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/framing/enum.h"
#include "qpid/framing/MessageTransferBody.h"
#include "qpid/log/Logger.h"
#include "qpid/sys/Monitor.h"
#include "qpid/sys/Thread.h"

#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/assign.hpp>

#include <string>
#include <iostream>
#include <fstream>
#include <iterator>
#include <vector>
#include <set>
#include <algorithm>
#include <iterator>

using namespace std;
using namespace qpid;
using namespace qpid::cluster;
using namespace qpid::framing;
using namespace qpid::client;
using namespace boost::assign;
using broker::Broker;
using boost::shared_ptr;

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(cluster_test)

bool durableFlag = std::getenv("STORE_LIB") != 0;

void prepareArgs(ClusterFixture::Args& args, const bool durableFlag = false) {
    ostringstream clusterLib;
    clusterLib << getLibPath("CLUSTER_LIB");
    args += "--auth", "no", "--no-module-dir", "--load-module", clusterLib.str();
    if (durableFlag)
        args += "--load-module", getLibPath("STORE_LIB"), "TMP_DATA_DIR";
    else
        args += "--no-data-dir";
}

ClusterFixture::Args prepareArgs(const bool durableFlag = false) {
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    return args;
}

// Timeout for tests that wait for messages
const sys::Duration TIMEOUT=sys::TIME_SEC/2;


ostream& operator<<(ostream& o, const cpg_name* n) {
    return o << Cpg::str(*n);
}

ostream& operator<<(ostream& o, const cpg_address& a) {
    return o << "(" << a.nodeid <<","<<a.pid<<","<<a.reason<<")";
}

template <class T>
ostream& operator<<(ostream& o, const pair<T*, int>& array) {
    o << "{ ";
    ostream_iterator<cpg_address> i(o, " ");
    copy(array.first, array.first+array.second, i);
    o << "}";
    return o;
}

template <class C> set<int> makeSet(const C& c) {
    set<int> s;
    copy(c.begin(), c.end(), inserter(s, s.begin()));
    return s;
}

class Sender {
  public:
    Sender(boost::shared_ptr<ConnectionImpl> ci, uint16_t ch) : connection(ci), channel(ch) {}
    void send(const AMQBody& body, bool firstSeg, bool lastSeg, bool firstFrame, bool lastFrame) {
        AMQFrame f(body);
        f.setChannel(channel);
        f.setFirstSegment(firstSeg);
        f.setLastSegment(lastSeg);
        f.setFirstFrame(firstFrame);
        f.setLastFrame(lastFrame);
        connection->handle(f);
    }

  private:
    boost::shared_ptr<ConnectionImpl> connection;
    uint16_t channel;
};

int64_t getMsgSequence(const Message& m) {
    return m.getMessageProperties().getApplicationHeaders().getAsInt64("qpid.msg_sequence");
}

Message ttlMessage(const string& data, const string& key, uint64_t ttl, bool durable = false) {
    Message m(data, key);
    m.getDeliveryProperties().setTtl(ttl);
    if (durable) m.getDeliveryProperties().setDeliveryMode(framing::PERSISTENT);
    return m;
}

Message makeMessage(const string& data, const string& key, bool durable = false) {
    Message m(data, key);
    if (durable) m.getDeliveryProperties().setDeliveryMode(framing::PERSISTENT);
    return m;
}

vector<string> browse(Client& c, const string& q, int n) {
    SubscriptionSettings browseSettings(
        FlowControl::messageCredit(n),
        ACCEPT_MODE_NONE,
        ACQUIRE_MODE_NOT_ACQUIRED,
        0                       // No auto-ack.
    );
    LocalQueue lq;
    c.subs.subscribe(lq, q, browseSettings);
    vector<string> result;
    for (int i = 0; i < n; ++i) {
        Message m;
        if (!lq.get(m, TIMEOUT))
            break;
        result.push_back(m.getData());
    }
    c.subs.getSubscription(q).cancel();
    return result;
}

ConnectionSettings aclSettings(int port, const std::string& id) {
    ConnectionSettings settings;
    settings.port = port;
    settings.mechanism = "PLAIN";
    settings.username = id;
    settings.password = id;
    return settings;
}

// An illegal frame body
struct PoisonPill : public AMQBody {
    virtual uint8_t type() const { return 0xFF; }
    virtual void encode(Buffer& ) const {}
    virtual void decode(Buffer& , uint32_t=0) {}
    virtual uint32_t encodedSize() const { return 0; }

    virtual void print(std::ostream&) const {};
    virtual void accept(AMQBodyConstVisitor&) const {};

    virtual AMQMethodBody* getMethod() { return 0; }
    virtual const AMQMethodBody* getMethod() const { return 0; }

    /** Match if same type and same class/method ID for methods */
    static bool match(const AMQBody& , const AMQBody& ) { return false; }
    virtual boost::intrusive_ptr<AMQBody> clone() const { return new PoisonPill; }
};

QPID_AUTO_TEST_CASE(testBadClientData) {
    // Ensure that bad data on a client connection closes the
    // connection but does not stop the broker.
    ClusterFixture::Args args;
    prepareArgs(args, false);
    args += "--log-enable=critical"; // Supress expected errors
    ClusterFixture cluster(2, args, -1);
    Client c0(cluster[0]);
    Client c1(cluster[1]);
    boost::shared_ptr<client::ConnectionImpl> ci =
        client::ConnectionAccess::getImpl(c0.connection);
    AMQFrame poison(boost::intrusive_ptr<AMQBody>(new PoisonPill));
    ci->handle(poison);
    {
        ScopedSuppressLogging sl;
        BOOST_CHECK_THROW(c0.session.queueQuery("q0"), TransportFailure);
    }
    Client c00(cluster[0]);
    BOOST_CHECK_EQUAL(c00.session.queueQuery("q00").getQueue(), "");
    BOOST_CHECK_EQUAL(c1.session.queueQuery("q1").getQueue(), "");
}

QPID_AUTO_TEST_CASE(testAcl) {
    ofstream policyFile("cluster_test.acl");
    policyFile << "acl allow foo@QPID create queue name=foo" << endl
               << "acl allow foo@QPID create queue name=foo2" << endl
               << "acl deny foo@QPID create queue name=bar" << endl
               << "acl allow all all" << endl;
    policyFile.close();
    char cwd[1024];
    BOOST_CHECK(::getcwd(cwd, sizeof(cwd)));
    ostringstream aclLib;
    aclLib << getLibPath("ACL_LIB");
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    args += "--log-enable=critical"; // Supress expected errors
    args += "--acl-file", string(cwd) + "/cluster_test.acl",
            "--cluster-mechanism", "PLAIN",
            "--cluster-username", "cluster",
            "--cluster-password", "cluster",
            "--load-module", aclLib.str();
    ClusterFixture cluster(2, args, -1);

    Client c0(aclSettings(cluster[0], "c0"), "c0");
    Client c1(aclSettings(cluster[1], "c1"), "c1");
    Client foo(aclSettings(cluster[1], "foo"), "foo");

    foo.session.queueDeclare("foo", arg::durable=durableFlag);
    BOOST_CHECK_EQUAL(c0.session.queueQuery("foo").getQueue(), "foo");

    { 
        ScopedSuppressLogging sl;
        BOOST_CHECK_THROW(foo.session.queueDeclare("bar", arg::durable=durableFlag), framing::NotAllowedException);
    }
    BOOST_CHECK(c0.session.queueQuery("bar").getQueue().empty());
    BOOST_CHECK(c1.session.queueQuery("bar").getQueue().empty());

    cluster.add();
    Client c2(aclSettings(cluster[2], "c2"), "c2");
    { 
        ScopedSuppressLogging sl;
        BOOST_CHECK_THROW(foo.session.queueDeclare("bar", arg::durable=durableFlag), framing::NotAllowedException);
    }
    BOOST_CHECK(c2.session.queueQuery("bar").getQueue().empty());
}

QPID_AUTO_TEST_CASE(testMessageTimeToLive) {
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(2, args, -1);
    Client c0(cluster[0], "c0");
    Client c1(cluster[1], "c1");
    c0.session.queueDeclare("p", arg::durable=durableFlag);
    c0.session.queueDeclare("q", arg::durable=durableFlag);
    c0.session.messageTransfer(arg::content=ttlMessage("a", "q", 200, durableFlag));
    c0.session.messageTransfer(arg::content=makeMessage("b", "q", durableFlag));
    c0.session.messageTransfer(arg::content=ttlMessage("x", "p", 10000, durableFlag));
    c0.session.messageTransfer(arg::content=makeMessage("y", "p", durableFlag));
    cluster.add();
    Client c2(cluster[1], "c2");

    BOOST_CHECK_EQUAL(browse(c0, "p", 1), list_of<string>("x"));
    BOOST_CHECK_EQUAL(browse(c1, "p", 1), list_of<string>("x"));
    BOOST_CHECK_EQUAL(browse(c2, "p", 1), list_of<string>("x"));

    sys::usleep(200*1000);
    BOOST_CHECK_EQUAL(browse(c0, "q", 1), list_of<string>("b"));
    BOOST_CHECK_EQUAL(browse(c1, "q", 1), list_of<string>("b"));
    BOOST_CHECK_EQUAL(browse(c2, "q", 1), list_of<string>("b"));
}

QPID_AUTO_TEST_CASE(testSequenceOptions) {
    // Make sure the exchange qpid.msg_sequence property is properly replicated.
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c0(cluster[0], "c0");
    FieldTable ftargs;
    ftargs.setInt("qpid.msg_sequence", 1);
    c0.session.queueDeclare(arg::queue="q", arg::durable=durableFlag);
    c0.session.exchangeDeclare(arg::exchange="ex", arg::type="direct", arg::arguments=ftargs);
    c0.session.exchangeBind(arg::exchange="ex", arg::queue="q", arg::bindingKey="k");
    c0.session.messageTransfer(arg::content=makeMessage("1", "k", durableFlag), arg::destination="ex");
    c0.session.messageTransfer(arg::content=makeMessage("2", "k", durableFlag), arg::destination="ex");
    BOOST_CHECK_EQUAL(1, getMsgSequence(c0.subs.get("q", TIMEOUT)));
    BOOST_CHECK_EQUAL(2, getMsgSequence(c0.subs.get("q", TIMEOUT)));

    cluster.add();
    Client c1(cluster[1]);
    c1.session.messageTransfer(arg::content=makeMessage("3", "k", durableFlag), arg::destination="ex");
    BOOST_CHECK_EQUAL(3, getMsgSequence(c1.subs.get("q", TIMEOUT)));
}

QPID_AUTO_TEST_CASE(testTxTransaction) {
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c0(cluster[0], "c0");
    c0.session.queueDeclare(arg::queue="q", arg::durable=durableFlag);
    c0.session.messageTransfer(arg::content=makeMessage("A", "q", durableFlag));
    c0.session.messageTransfer(arg::content=makeMessage("B", "q", durableFlag));

    // Start a transaction that will commit.
    Session commitSession = c0.connection.newSession("commit");
    SubscriptionManager commitSubs(commitSession);
    commitSession.txSelect();
    commitSession.messageTransfer(arg::content=makeMessage("a", "q", durableFlag));
    commitSession.messageTransfer(arg::content=makeMessage("b", "q", durableFlag));
    BOOST_CHECK_EQUAL(commitSubs.get("q", TIMEOUT).getData(), "A");

    // Start a transaction that will roll back.
    Session rollbackSession = c0.connection.newSession("rollback");
    SubscriptionManager rollbackSubs(rollbackSession);
    rollbackSession.txSelect();
    rollbackSession.messageTransfer(arg::content=makeMessage("1", "q", durableFlag));
    Message rollbackMessage = rollbackSubs.get("q", TIMEOUT);
    BOOST_CHECK_EQUAL(rollbackMessage.getData(), "B");

    BOOST_CHECK_EQUAL(c0.session.queueQuery("q").getMessageCount(), 0u);
    // Add new member mid transaction.
    cluster.add();
    Client c1(cluster[1], "c1");

    // More transactional work
    BOOST_CHECK_EQUAL(c1.session.queueQuery("q").getMessageCount(), 0u);
    rollbackSession.messageTransfer(arg::content=makeMessage("2", "q", durableFlag));
    commitSession.messageTransfer(arg::content=makeMessage("c", "q", durableFlag));
    rollbackSession.messageTransfer(arg::content=makeMessage("3", "q", durableFlag));

    BOOST_CHECK_EQUAL(c1.session.queueQuery("q").getMessageCount(), 0u);

    // Commit/roll back.
    commitSession.txCommit();
    rollbackSession.txRollback();
    rollbackSession.messageRelease(rollbackMessage.getId());

    // Verify queue status: just the comitted messages and dequeues should remain.
    BOOST_CHECK_EQUAL(c1.session.queueQuery("q").getMessageCount(), 4u);
    BOOST_CHECK_EQUAL(c1.subs.get("q", TIMEOUT).getData(), "B");
    BOOST_CHECK_EQUAL(c1.subs.get("q", TIMEOUT).getData(), "a");
    BOOST_CHECK_EQUAL(c1.subs.get("q", TIMEOUT).getData(), "b");
    BOOST_CHECK_EQUAL(c1.subs.get("q", TIMEOUT).getData(), "c");

    commitSession.close();
    rollbackSession.close();
}

QPID_AUTO_TEST_CASE(testUnacked) {
    // Verify replication of unacknowledged messages.
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c0(cluster[0], "c0");

    Message m;

    // Create unacked message: acquired but not accepted.
    SubscriptionSettings manualAccept(FlowControl::unlimited(), ACCEPT_MODE_EXPLICIT, ACQUIRE_MODE_PRE_ACQUIRED, 0);
    c0.session.queueDeclare("q1", arg::durable=durableFlag);
    c0.session.messageTransfer(arg::content=makeMessage("11","q1", durableFlag));
    LocalQueue q1;
    c0.subs.subscribe(q1, "q1", manualAccept);
    BOOST_CHECK_EQUAL(q1.get(TIMEOUT).getData(), "11"); // Acquired but not accepted
    BOOST_CHECK_EQUAL(c0.session.queueQuery("q1").getMessageCount(), 0u); // Gone from queue

    // Create unacked message: not acquired, accepted or completeed.
    SubscriptionSettings manualAcquire(FlowControl::unlimited(), ACCEPT_MODE_EXPLICIT, ACQUIRE_MODE_NOT_ACQUIRED, 0);
    c0.session.queueDeclare("q2", arg::durable=durableFlag);
    c0.session.messageTransfer(arg::content=makeMessage("21","q2", durableFlag));
    c0.session.messageTransfer(arg::content=makeMessage("22","q2", durableFlag));
    LocalQueue q2;
    c0.subs.subscribe(q2, "q2", manualAcquire);
    m = q2.get(TIMEOUT);  // Not acquired or accepted, still on queue
    BOOST_CHECK_EQUAL(m.getData(), "21");
    BOOST_CHECK_EQUAL(c0.session.queueQuery("q2").getMessageCount(), 2u); // Not removed
    c0.subs.getSubscription("q2").acquire(m); // Acquire manually
    BOOST_CHECK_EQUAL(c0.session.queueQuery("q2").getMessageCount(), 1u); // Removed
    BOOST_CHECK_EQUAL(q2.get(TIMEOUT).getData(), "22"); // Not acquired or accepted, still on queue
    BOOST_CHECK_EQUAL(c0.session.queueQuery("q2").getMessageCount(), 1u); // 1 not acquired.

    // Create empty credit record: acquire and accept but don't complete.
    SubscriptionSettings manualComplete(FlowControl::messageWindow(1), ACCEPT_MODE_EXPLICIT, ACQUIRE_MODE_PRE_ACQUIRED, 1, MANUAL_COMPLETION);
    c0.session.queueDeclare("q3", arg::durable=durableFlag);
    c0.session.messageTransfer(arg::content=makeMessage("31", "q3", durableFlag));
    c0.session.messageTransfer(arg::content=makeMessage("32", "q3", durableFlag));
    LocalQueue q3;
    c0.subs.subscribe(q3, "q3", manualComplete);
    Message m31=q3.get(TIMEOUT);
    BOOST_CHECK_EQUAL(m31.getData(), "31"); // Automatically acquired & accepted but not completed.
    BOOST_CHECK_EQUAL(c0.session.queueQuery("q3").getMessageCount(), 1u);

    // Add new member while there are unacked messages.
    cluster.add();
    Client c1(cluster[1], "c1");

    // Check queue counts
    BOOST_CHECK_EQUAL(c1.session.queueQuery("q1").getMessageCount(), 0u);
    BOOST_CHECK_EQUAL(c1.session.queueQuery("q2").getMessageCount(), 1u);
    BOOST_CHECK_EQUAL(c1.session.queueQuery("q3").getMessageCount(), 1u);

    // Complete the empty credit message, should unblock the message behind it.
    BOOST_CHECK_THROW(q3.get(0), Exception);
    c0.session.markCompleted(SequenceSet(m31.getId()), true);
    BOOST_CHECK_EQUAL(q3.get(TIMEOUT).getData(), "32");
    BOOST_CHECK_EQUAL(c0.session.queueQuery("q3").getMessageCount(), 0u);
    BOOST_CHECK_EQUAL(c1.session.queueQuery("q3").getMessageCount(), 0u);

    // Close the original session - unacked messages should be requeued.
    c0.session.close();
    BOOST_CHECK_EQUAL(c1.session.queueQuery("q1").getMessageCount(), 1u);
    BOOST_CHECK_EQUAL(c1.session.queueQuery("q2").getMessageCount(), 2u);

    BOOST_CHECK_EQUAL(c1.subs.get("q1", TIMEOUT).getData(), "11");
    BOOST_CHECK_EQUAL(c1.subs.get("q2", TIMEOUT).getData(), "21");
    BOOST_CHECK_EQUAL(c1.subs.get("q2", TIMEOUT).getData(), "22");
}

// FIXME aconway 2009-06-17: test for unimplemented feature, enable when implemented.
void testUpdateTxState() {
    // Verify that we update transaction state correctly to new members.
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c0(cluster[0], "c0");

    // Do work in a transaction.
    c0.session.txSelect();
    c0.session.queueDeclare("q", arg::durable=durableFlag);
    c0.session.messageTransfer(arg::content=makeMessage("1","q", durableFlag));
    c0.session.messageTransfer(arg::content=makeMessage("2","q", durableFlag));
    Message m;
    BOOST_CHECK(c0.subs.get(m, "q", TIMEOUT));
    BOOST_CHECK_EQUAL(m.getData(), "1");

    // New member, TX not comitted, c1 should see nothing.
    cluster.add();
    Client c1(cluster[1], "c1");
    BOOST_CHECK_EQUAL(c1.session.queueQuery(arg::queue="q").getMessageCount(), 0u);

    // After commit c1 shoudl see results of tx.
    c0.session.txCommit();
    BOOST_CHECK_EQUAL(c1.session.queueQuery(arg::queue="q").getMessageCount(), 1u);
    BOOST_CHECK(c1.subs.get(m, "q", TIMEOUT));
    BOOST_CHECK_EQUAL(m.getData(), "2");

    // Another transaction with both members active.
    c0.session.messageTransfer(arg::content=makeMessage("3","q", durableFlag));
    BOOST_CHECK_EQUAL(c1.session.queueQuery(arg::queue="q").getMessageCount(), 0u);
    c0.session.txCommit();
    BOOST_CHECK_EQUAL(c1.session.queueQuery(arg::queue="q").getMessageCount(), 1u);
    BOOST_CHECK(c1.subs.get(m, "q", TIMEOUT));
    BOOST_CHECK_EQUAL(m.getData(), "3");
}

QPID_AUTO_TEST_CASE(testUpdateMessageBuilder) {
    // Verify that we update a partially recieved message to a new member.
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c0(cluster[0], "c0");
    c0.session.queueDeclare("q", arg::durable=durableFlag);
    Sender sender(ConnectionAccess::getImpl(c0.connection), c0.session.getChannel());

    // Send first 2 frames of message.
    MessageTransferBody transfer(
        ProtocolVersion(), string(), // default exchange.
        framing::message::ACCEPT_MODE_NONE,
        framing::message::ACQUIRE_MODE_PRE_ACQUIRED);
    sender.send(transfer, true, false, true, true);
    AMQHeaderBody header;
    header.get<DeliveryProperties>(true)->setRoutingKey("q");
    if (durableFlag)
        header.get<DeliveryProperties>(true)->setDeliveryMode(DELIVERY_MODE_PERSISTENT);
    else
        header.get<DeliveryProperties>(true)->setDeliveryMode(DELIVERY_MODE_NON_PERSISTENT);
    sender.send(header, false, false, true, true);

    // No reliable way to ensure the partial message has arrived
    // before we start the new broker, so we sleep.
    sys::usleep(2500);
    cluster.add();

    // Send final 2 frames of message.
    sender.send(AMQContentBody("ab"), false, true, true, false);
    sender.send(AMQContentBody("cd"), false, true, false, true);

    // Verify message is enqued correctly on second member.
    Message m;
    Client c1(cluster[1], "c1");
    BOOST_CHECK(c1.subs.get(m, "q", TIMEOUT));
    BOOST_CHECK_EQUAL(m.getData(), "abcd");
    BOOST_CHECK_EQUAL(2u, knownBrokerPorts(c1.connection).size());
}

QPID_AUTO_TEST_CASE(testConnectionKnownHosts) {
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c0(cluster[0], "c0");
    set<int> kb0 = knownBrokerPorts(c0.connection);
    BOOST_CHECK_EQUAL(kb0.size(), 1u);
    BOOST_CHECK_EQUAL(kb0, makeSet(cluster));

    cluster.add();
    Client c1(cluster[1], "c1");
    set<int> kb1 = knownBrokerPorts(c1.connection);
    kb0 = knownBrokerPorts(c0.connection, 2);
    BOOST_CHECK_EQUAL(kb1.size(), 2u);
    BOOST_CHECK_EQUAL(kb1, makeSet(cluster));
    BOOST_CHECK_EQUAL(kb1,kb0);

    cluster.add();
    Client c2(cluster[2], "c2");
    set<int> kb2 = knownBrokerPorts(c2.connection);
    kb1 = knownBrokerPorts(c1.connection, 3);
    kb0 = knownBrokerPorts(c0.connection, 3);
    BOOST_CHECK_EQUAL(kb2.size(), 3u);
    BOOST_CHECK_EQUAL(kb2, makeSet(cluster));
    BOOST_CHECK_EQUAL(kb2,kb0);
    BOOST_CHECK_EQUAL(kb2,kb1);

    cluster.killWithSilencer(1,c1.connection,9);
    kb0 = knownBrokerPorts(c0.connection, 2);
    kb2 = knownBrokerPorts(c2.connection, 2);
    BOOST_CHECK_EQUAL(kb0.size(), 2u);
    BOOST_CHECK_EQUAL(kb0, kb2);
}

QPID_AUTO_TEST_CASE(testUpdateConsumers) {
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);

    Client c0(cluster[0], "c0");
    c0.session.queueDeclare("p", arg::durable=durableFlag);
    c0.session.queueDeclare("q", arg::durable=durableFlag);
    c0.subs.subscribe(c0.lq, "q", FlowControl::zero());
    LocalQueue lp;
    c0.subs.subscribe(lp, "p", FlowControl::messageCredit(1));
    c0.session.sync();

    // Start new members
    cluster.add();              // Local
    Client c1(cluster[1], "c1");
    cluster.add();
    Client c2(cluster[2], "c2");

    // Transfer messages
    c0.session.messageTransfer(arg::content=makeMessage("aaa", "q", durableFlag));

    c0.session.messageTransfer(arg::content=makeMessage("bbb", "p", durableFlag));
    c0.session.messageTransfer(arg::content=makeMessage("ccc", "p", durableFlag));

    // Activate the subscription, ensure message removed on all queues.
    c0.subs.setFlowControl("q", FlowControl::unlimited());
    Message m;
    BOOST_CHECK(c0.lq.get(m, TIMEOUT));
    BOOST_CHECK_EQUAL(m.getData(), "aaa");
    BOOST_CHECK_EQUAL(c0.session.queueQuery("q").getMessageCount(), 0u);
    BOOST_CHECK_EQUAL(c1.session.queueQuery("q").getMessageCount(), 0u);
    BOOST_CHECK_EQUAL(c2.session.queueQuery("q").getMessageCount(), 0u);

    // Check second subscription's flow control: gets first message, not second.
    BOOST_CHECK(lp.get(m, TIMEOUT));
    BOOST_CHECK_EQUAL(m.getData(), "bbb");
    BOOST_CHECK_EQUAL(c0.session.queueQuery("p").getMessageCount(), 1u);
    BOOST_CHECK_EQUAL(c1.session.queueQuery("p").getMessageCount(), 1u);
    BOOST_CHECK_EQUAL(c2.session.queueQuery("p").getMessageCount(), 1u);

    BOOST_CHECK(c0.subs.get(m, "p", TIMEOUT));
    BOOST_CHECK_EQUAL(m.getData(), "ccc");

    // Kill the subscribing member, ensure further messages are not removed.
    cluster.killWithSilencer(0,c0.connection,9);
    BOOST_REQUIRE_EQUAL(knownBrokerPorts(c1.connection, 2).size(), 2u);
    for (int i = 0; i < 10; ++i) {
        c1.session.messageTransfer(arg::content=makeMessage("xxx", "q", durableFlag));
        BOOST_REQUIRE(c1.subs.get(m, "q", TIMEOUT));
        BOOST_REQUIRE_EQUAL(m.getData(), "xxx");
    }
}

// Test that message data and delivery properties are updated properly.
QPID_AUTO_TEST_CASE(testUpdateMessages) {
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c0(cluster[0], "c0");

    // Create messages with different delivery properties
    c0.session.queueDeclare("q", arg::durable=durableFlag);
    c0.session.exchangeBind(arg::exchange="amq.fanout", arg::queue="q");
    c0.session.messageTransfer(arg::content=makeMessage("foo","q", durableFlag));
    c0.session.messageTransfer(arg::content=makeMessage("bar","q", durableFlag),
                               arg::destination="amq.fanout");

    while (c0.session.queueQuery("q").getMessageCount() != 2)
        sys::usleep(1000);    // Wait for message to show up on broker 0.

    // Add a new broker, it will catch up.
    cluster.add();

    // Do some work post-add
    c0.session.queueDeclare("p", arg::durable=durableFlag);
    c0.session.messageTransfer(arg::content=makeMessage("pfoo","p", durableFlag));

    // Do some work post-join
    BOOST_REQUIRE_EQUAL(knownBrokerPorts(c0.connection, 2).size(), 2u);
    c0.session.messageTransfer(arg::content=makeMessage("pbar","p", durableFlag));

    // Verify new brokers have state.
    Message m;

    Client c1(cluster[1], "c1");

    BOOST_CHECK(c1.subs.get(m, "q", TIMEOUT));
    BOOST_CHECK_EQUAL(m.getData(), "foo");
    BOOST_CHECK(m.getDeliveryProperties().hasExchange());
    BOOST_CHECK_EQUAL(m.getDeliveryProperties().getExchange(), "");
    BOOST_CHECK(c1.subs.get(m, "q", TIMEOUT));
    BOOST_CHECK_EQUAL(m.getData(), "bar");
    BOOST_CHECK(m.getDeliveryProperties().hasExchange());
    BOOST_CHECK_EQUAL(m.getDeliveryProperties().getExchange(), "amq.fanout");
    BOOST_CHECK_EQUAL(c1.session.queueQuery("q").getMessageCount(), 0u);

    // Add another broker, don't wait for join - should be stalled till ready.
    cluster.add();
    Client c2(cluster[2], "c2");
    BOOST_CHECK(c2.subs.get(m, "p", TIMEOUT));
    BOOST_CHECK_EQUAL(m.getData(), "pfoo");
    BOOST_CHECK(c2.subs.get(m, "p", TIMEOUT));
    BOOST_CHECK_EQUAL(m.getData(), "pbar");
    BOOST_CHECK_EQUAL(c2.session.queueQuery("p").getMessageCount(), 0u);
}

QPID_AUTO_TEST_CASE(testWiringReplication) {
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(3, args, -1);
    Client c0(cluster[0]);
    BOOST_CHECK(c0.session.queueQuery("q").getQueue().empty());
    BOOST_CHECK(c0.session.exchangeQuery("ex").getType().empty());
    c0.session.queueDeclare("q", arg::durable=durableFlag);
    c0.session.exchangeDeclare("ex", arg::type="direct");
    c0.session.close();
    c0.connection.close();
    // Verify all brokers get wiring update.
    for (size_t i = 0; i < cluster.size(); ++i) {
        BOOST_MESSAGE("i == "<< i);
        Client c(cluster[i]);
        BOOST_CHECK_EQUAL("q", c.session.queueQuery("q").getQueue());
        BOOST_CHECK_EQUAL("direct", c.session.exchangeQuery("ex").getType());
    }
}

QPID_AUTO_TEST_CASE(testMessageEnqueue) {
    // Enqueue on one broker, dequeue on another.
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(2, args, -1);
    Client c0(cluster[0]);
    c0.session.queueDeclare("q", arg::durable=durableFlag);
    c0.session.messageTransfer(arg::content=makeMessage("foo", "q", durableFlag));
    c0.session.messageTransfer(arg::content=makeMessage("bar", "q", durableFlag));
    c0.session.close();
    Client c1(cluster[1]);
    Message msg;
    BOOST_CHECK(c1.subs.get(msg, "q", TIMEOUT));
    BOOST_CHECK_EQUAL(string("foo"), msg.getData());
    BOOST_CHECK(c1.subs.get(msg, "q", TIMEOUT));
    BOOST_CHECK_EQUAL(string("bar"), msg.getData());
}

QPID_AUTO_TEST_CASE(testMessageDequeue) {
    // Enqueue on one broker, dequeue on two others.
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(3, args, -1);
    Client c0(cluster[0], "c0");
    c0.session.queueDeclare("q", arg::durable=durableFlag);
    c0.session.messageTransfer(arg::content=makeMessage("foo", "q", durableFlag));
    c0.session.messageTransfer(arg::content=makeMessage("bar", "q", durableFlag));

    Message msg;

    // Dequeue on 2 others, ensure correct order.
    Client c1(cluster[1], "c1");
    BOOST_CHECK(c1.subs.get(msg, "q"));
    BOOST_CHECK_EQUAL("foo", msg.getData());

    Client c2(cluster[2], "c2");
    BOOST_CHECK(c1.subs.get(msg, "q"));
    BOOST_CHECK_EQUAL("bar", msg.getData());

    // Queue should be empty on all cluster members.
    BOOST_CHECK_EQUAL(0u, c0.session.queueQuery("q").getMessageCount());
    BOOST_CHECK_EQUAL(0u, c1.session.queueQuery("q").getMessageCount());
    BOOST_CHECK_EQUAL(0u, c2.session.queueQuery("q").getMessageCount());
}

QPID_AUTO_TEST_CASE(testDequeueWaitingSubscription) {
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(3, args, -1);
    Client c0(cluster[0]);
    BOOST_REQUIRE_EQUAL(knownBrokerPorts(c0.connection, 3).size(), 3u); // Wait for brokers.

    // First start a subscription.
    c0.session.queueDeclare("q", arg::durable=durableFlag);
    c0.subs.subscribe(c0.lq, "q", FlowControl::messageCredit(2));

    // Now send messages
    Client c1(cluster[1]);
    c1.session.messageTransfer(arg::content=makeMessage("foo", "q", durableFlag));
    c1.session.messageTransfer(arg::content=makeMessage("bar", "q", durableFlag));

    // Check they arrived
    Message m;
    BOOST_CHECK(c0.lq.get(m, TIMEOUT));
    BOOST_CHECK_EQUAL("foo", m.getData());
    BOOST_CHECK(c0.lq.get(m, TIMEOUT));
    BOOST_CHECK_EQUAL("bar", m.getData());

    // Queue should be empty on all cluster members.
    Client c2(cluster[2]);
    BOOST_CHECK_EQUAL(0u, c0.session.queueQuery("q").getMessageCount());
    BOOST_CHECK_EQUAL(0u, c1.session.queueQuery("q").getMessageCount());
    BOOST_CHECK_EQUAL(0u, c2.session.queueQuery("q").getMessageCount());
}

QPID_AUTO_TEST_CASE(queueDurabilityPropagationToNewbie)
{
    /*
      Start with a single broker.
      Set up two queues: one durable, and one not.
      Add a new broker to the cluster.
      Make sure it has one durable and one non-durable queue.
    */
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c0(cluster[0]);
    c0.session.queueDeclare("durable_queue",     arg::durable=true);
    c0.session.queueDeclare("non_durable_queue", arg::durable=false);
    cluster.add();
    Client c1(cluster[1]);
    QueueQueryResult durable_query     = c1.session.queueQuery ( "durable_queue" );
    QueueQueryResult non_durable_query = c1.session.queueQuery ( "non_durable_queue" );
    BOOST_CHECK_EQUAL(durable_query.getQueue(), std::string("durable_queue"));
    BOOST_CHECK_EQUAL(non_durable_query.getQueue(), std::string("non_durable_queue"));

    BOOST_CHECK_EQUAL ( durable_query.getDurable(),     true  );
    BOOST_CHECK_EQUAL ( non_durable_query.getDurable(), false );
}


QPID_AUTO_TEST_CASE(testHeartbeatCancelledOnFailover)
{

    struct Sender : FailoverManager::Command
    {
        std::string queue;
        std::string content;

        Sender(const std::string& q, const std::string& c) : queue(q), content(c) {}

        void execute(AsyncSession& session, bool)
        {
            session.messageTransfer(arg::content=makeMessage(content, queue, durableFlag));
        }
    };

    struct Receiver : FailoverManager::Command, MessageListener, qpid::sys::Runnable
    {
        FailoverManager& mgr;
        std::string queue;
        std::string expectedContent;
        qpid::client::Subscription subscription;
        qpid::sys::Monitor lock;
        bool ready, failed;

        Receiver(FailoverManager& m, const std::string& q, const std::string& c) : mgr(m), queue(q), expectedContent(c), ready(false), failed(false) {}

        void received(Message& message)
        {
            BOOST_CHECK_EQUAL(expectedContent, message.getData());
            subscription.cancel();
        }

        void execute(AsyncSession& session, bool)
        {
            session.queueDeclare(arg::queue=queue, arg::durable=durableFlag);
            SubscriptionManager subs(session);
            subscription = subs.subscribe(*this, queue);
            session.sync();
            setReady();
            subs.run();
            //cleanup:
            session.queueDelete(arg::queue=queue);
        }

        void run()
        {
            try {
            mgr.execute(*this);
        }
            catch (const std::exception& e) {
                BOOST_MESSAGE("Exception in mgr.execute: " << e.what());
                failed = true;
            }
        }

        void waitForReady()
        {
            qpid::sys::Monitor::ScopedLock l(lock);
            while (!ready) {
                lock.wait();
            }
        }

        void setReady()
        {
            qpid::sys::Monitor::ScopedLock l(lock);
            ready = true;
            lock.notify();
        }
    };

    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(2, args, -1);
    ConnectionSettings settings;
    settings.port = cluster[1];
    settings.heartbeat = 1;
    FailoverManager fmgr(settings);
    Sender sender("my-queue", "my-data");
    Receiver receiver(fmgr, "my-queue", "my-data");
    qpid::sys::Thread runner(receiver);
    receiver.waitForReady();
    {
        ScopedSuppressLogging allQuiet; // suppress connection closed messages
        cluster.kill(1);
        //sleep for 2 secs to allow the heartbeat task to fire on the now dead connection:
        ::usleep(2*1000*1000);
    }
    fmgr.execute(sender);
    runner.join();
    BOOST_CHECK(!receiver.failed);
    fmgr.close();
}

QPID_AUTO_TEST_CASE(testPolicyUpdate) {
    //tests that the policys internal state is accurate on newly
    //joined nodes
    ClusterFixture::Args args;
    args += "--log-enable", "critical";
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c1(cluster[0], "c1");
    {
        ScopedSuppressLogging allQuiet;
        QueueOptions options;
        options.setSizePolicy(REJECT, 0, 2);
        c1.session.queueDeclare("q", arg::arguments=options, arg::durable=durableFlag);
        c1.session.messageTransfer(arg::content=makeMessage("one", "q", durableFlag));
        cluster.add();
        Client c2(cluster[1], "c2");
        c2.session.messageTransfer(arg::content=makeMessage("two", "q", durableFlag));

        BOOST_CHECK_THROW(c2.session.messageTransfer(arg::content=makeMessage("three", "q", durableFlag)), framing::ResourceLimitExceededException);

        Message received;
        BOOST_CHECK(c1.subs.get(received, "q"));
        BOOST_CHECK_EQUAL(received.getData(), std::string("one"));
        BOOST_CHECK(c1.subs.get(received, "q"));
        BOOST_CHECK_EQUAL(received.getData(), std::string("two"));
        BOOST_CHECK(!c1.subs.get(received, "q"));
    }
}

QPID_AUTO_TEST_CASE(testExclusiveQueueUpdate) {
    //tests that exclusive queues are accurately replicated on newly
    //joined nodes
    ClusterFixture::Args args;
    args += "--log-enable", "critical";
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c1(cluster[0], "c1");
    {
        ScopedSuppressLogging allQuiet;
        c1.session.queueDeclare("q", arg::exclusive=true, arg::autoDelete=true, arg::alternateExchange="amq.fanout");
        cluster.add();
        Client c2(cluster[1], "c2");
        QueueQueryResult result = c2.session.queueQuery("q");
        BOOST_CHECK_EQUAL(result.getQueue(), std::string("q"));
        BOOST_CHECK(result.getExclusive());
        BOOST_CHECK(result.getAutoDelete());
        BOOST_CHECK(!result.getDurable());
        BOOST_CHECK_EQUAL(result.getAlternateExchange(), std::string("amq.fanout"));
        BOOST_CHECK_THROW(c2.session.queueDeclare(arg::queue="q", arg::exclusive=true, arg::passive=true), framing::ResourceLockedException);
        c1.session.close();
        c1.connection.close();
        c2.session = c2.connection.newSession();
        BOOST_CHECK_THROW(c2.session.queueDeclare(arg::queue="q", arg::passive=true), framing::NotFoundException);
    }
}

/**
 * Subscribes to specified queue and acquires up to the specified
 * number of message but does not accept or release them. These
 * message are therefore 'locked' by the clients session.
 */
Subscription lockMessages(Client& client, const std::string& queue, int count)
{
    LocalQueue q;
    SubscriptionSettings settings(FlowControl::messageCredit(count));
    settings.autoAck = 0;
    Subscription sub = client.subs.subscribe(q, queue, settings);
    client.session.messageFlush(sub.getName());
    return sub;
}

/**
 * check that the specified queue contains the expected set of
 * messages (matched on content) for all nodes in the cluster
 */
void checkQueue(ClusterFixture& cluster, const std::string& queue, const std::vector<std::string>& messages)
{
    for (size_t i = 0; i < cluster.size(); i++) {
        Client client(cluster[i], (boost::format("%1%_%2%") % "c" % (i+1)).str());
        BOOST_CHECK_EQUAL(browse(client, queue, messages.size()), messages);
        client.close();
    }
}

void send(Client& client, const std::string& queue, int count, int start=1, const std::string& base="m",
          const std::string& lvqKey="")
{
    for (int i = 0; i < count; i++) {
        Message message = makeMessage((boost::format("%1%_%2%") % base % (i+start)).str(), queue, durableFlag);
        if (!lvqKey.empty()) message.getHeaders().setString(QueueOptions::strLVQMatchProperty, lvqKey);
        client.session.messageTransfer(arg::content=message);
    }
}

QPID_AUTO_TEST_CASE(testRingQueueUpdate) {
    //tests that ring queues are accurately replicated on newly
    //joined nodes
    ClusterFixture::Args args;
    args += "--log-enable", "critical";
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c1(cluster[0], "c1");
    {
        ScopedSuppressLogging allQuiet;
        QueueOptions options;
        options.setSizePolicy(RING, 0, 5);
        c1.session.queueDeclare("q", arg::arguments=options, arg::durable=durableFlag);
        send(c1, "q", 5);
        lockMessages(c1, "q", 1);
        //add new node
        cluster.add();
        BOOST_CHECK_EQUAL(2u, knownBrokerPorts(c1.connection, 2).size());//wait till joined
        //send one more message
        send(c1, "q", 1, 6);
        //release locked message
        c1.close();
        //check state of queue on both nodes
        checkQueue(cluster, "q", list_of<string>("m_2")("m_3")("m_4")("m_5")("m_6"));
    }
}

QPID_AUTO_TEST_CASE(testRingQueueUpdate2) {
    //tests that ring queues are accurately replicated on newly joined
    //nodes; just like testRingQueueUpdate, but new node joins after
    //the sixth message has been sent.
    ClusterFixture::Args args;
    args += "--log-enable", "critical";
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c1(cluster[0], "c1");
    {
        ScopedSuppressLogging allQuiet;
        QueueOptions options;
        options.setSizePolicy(RING, 0, 5);
        c1.session.queueDeclare("q", arg::arguments=options, arg::durable=durableFlag);
        send(c1, "q", 5);
        lockMessages(c1, "q", 1);
        //send sixth message
        send(c1, "q", 1, 6);
        //add new node
        cluster.add();
        BOOST_CHECK_EQUAL(2u, knownBrokerPorts(c1.connection, 2).size());//wait till joined
        //release locked message
        c1.close();
        //check state of queue on both nodes
        checkQueue(cluster, "q", list_of<string>("m_2")("m_3")("m_4")("m_5")("m_6"));
    }
}

QPID_AUTO_TEST_CASE(testLvqUpdate) {
    //tests that lvqs are accurately replicated on newly joined nodes
    ClusterFixture::Args args;
    args += "--log-enable", "critical";
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c1(cluster[0], "c1");
    {
        ScopedSuppressLogging allQuiet;
        QueueOptions options;
        options.setOrdering(LVQ);
        c1.session.queueDeclare("q", arg::arguments=options, arg::durable=durableFlag);

        send(c1, "q", 5, 1, "a", "a");
        send(c1, "q", 2, 1, "b", "b");
        send(c1, "q", 1, 1, "c", "c");
        send(c1, "q", 1, 3, "b", "b");

        //add new node
        cluster.add();
        BOOST_CHECK_EQUAL(2u, knownBrokerPorts(c1.connection, 2).size());//wait till joined

        //check state of queue on both nodes
        checkQueue(cluster, "q", list_of<string>("a_5")("b_3")("c_1"));
    }
}


QPID_AUTO_TEST_CASE(testBrowsedLvqUpdate) {
    //tests that lvqs are accurately replicated on newly joined nodes
    //if the lvq state has been affected by browsers
    ClusterFixture::Args args;
    args += "--log-enable", "critical";
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c1(cluster[0], "c1");
    {
        ScopedSuppressLogging allQuiet;
        QueueOptions options;
        options.setOrdering(LVQ);
        c1.session.queueDeclare("q", arg::arguments=options, arg::durable=durableFlag);

        send(c1, "q", 1, 1, "a", "a");
        send(c1, "q", 2, 1, "b", "b");
        send(c1, "q", 1, 1, "c", "c");
        checkQueue(cluster, "q", list_of<string>("a_1")("b_2")("c_1"));
        send(c1, "q", 4, 2, "a", "a");
        send(c1, "q", 1, 3, "b", "b");

        //add new node
        cluster.add();
        BOOST_CHECK_EQUAL(2u, knownBrokerPorts(c1.connection, 2).size());//wait till joined

        //check state of queue on both nodes
        checkQueue(cluster, "q", list_of<string>("a_1")("b_2")("c_1")("a_5")("b_3"));
    }
}

QPID_AUTO_TEST_CASE(testRelease) {
    //tests that releasing a messages that was unacked when one node
    //joined works correctly
    ClusterFixture::Args args;
    args += "--log-enable", "critical";
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c1(cluster[0], "c1");
    {
        ScopedSuppressLogging allQuiet;
        c1.session.queueDeclare("q", arg::durable=durableFlag);
        for (int i = 0; i < 5; i++) {
            c1.session.messageTransfer(arg::content=makeMessage((boost::format("%1%_%2%") % "m" % (i+1)).str(), "q", durableFlag));
        }
        //receive but don't ack a message
        LocalQueue lq;
        SubscriptionSettings lqSettings(FlowControl::messageCredit(1));
        lqSettings.autoAck = 0;
        Subscription lqSub = c1.subs.subscribe(lq, "q", lqSettings);
        c1.session.messageFlush("q");
        Message received;
        BOOST_CHECK(lq.get(received));
        BOOST_CHECK_EQUAL(received.getData(), std::string("m_1"));

        //add new node
        cluster.add();

        lqSub.release(lqSub.getUnaccepted());

        //check state of queue on both nodes
        vector<string> expected = list_of<string>("m_1")("m_2")("m_3")("m_4")("m_5");
        Client c3(cluster[0], "c3");
        BOOST_CHECK_EQUAL(browse(c3, "q", 5), expected);
        Client c2(cluster[1], "c2");
        BOOST_CHECK_EQUAL(browse(c2, "q", 5), expected);
    }
}


// Browse for 1 message with byte credit, return true if a message was
// received false if not.
bool browseByteCredit(Client& c, const string& q, int n, Message& m) {
    SubscriptionSettings browseSettings(
        FlowControl(1, n, false), // 1 message, n bytes credit, no window
        ACCEPT_MODE_NONE,
        ACQUIRE_MODE_NOT_ACQUIRED,
        0                       // No auto-ack.
    );
    LocalQueue lq;
    Subscription s = c.subs.subscribe(lq, q, browseSettings);
    c.session.messageFlush(arg::destination=q, arg::sync=true);
    c.session.sync();
    c.subs.getSubscription(q).cancel();
    return lq.get(m, 0);        // No timeout, flush should push message thru.
}

// Ensure cluster update preserves exact message size, use byte credt as test.
QPID_AUTO_TEST_CASE(testExactByteCredit) {
    ClusterFixture cluster(1, prepareArgs(), -1);
    Client c0(cluster[0], "c0");
    c0.session.queueDeclare("q");
    c0.session.messageTransfer(arg::content=Message("MyMessage", "q"));
    cluster.add();

    int size=36;                // Size of message on broker: headers+body
    Client c1(cluster[1], "c1");
    Message m;

    // Ensure we get the message with exact credit.
    BOOST_CHECK(browseByteCredit(c0, "q", size, m));
    BOOST_CHECK(browseByteCredit(c1, "q", size, m));
    // and not with one byte less.
    BOOST_CHECK(!browseByteCredit(c0, "q", size-1, m));
    BOOST_CHECK(!browseByteCredit(c1, "q", size-1, m));
}

// Test that consumer positions are updated correctly.
// Regression test for https://bugzilla.redhat.com/show_bug.cgi?id=541927
//
QPID_AUTO_TEST_CASE(testUpdateConsumerPosition) {
    ClusterFixture::Args args;
    prepareArgs(args, durableFlag);
    ClusterFixture cluster(1, args, -1);
    Client c0(cluster[0], "c0");

    c0.session.queueDeclare("q", arg::durable=durableFlag);
    SubscriptionSettings settings;
    settings.autoAck = 0;
    // Set the acquire mode to 'not-acquired' the consumer moves along the queue
    // but does not acquire (remove) messages.
    settings.acquireMode = ACQUIRE_MODE_NOT_ACQUIRED;
    Subscription s = c0.subs.subscribe(c0.lq, "q", settings);
    c0.session.messageTransfer(arg::content=makeMessage("1", "q", durableFlag));
    BOOST_CHECK_EQUAL("1", c0.lq.get(TIMEOUT).getData());

    // Add another member, send/receive another message and acquire
    // the messages.  With the bug, this creates an inconsistency
    // because the browse position was not updated to the new member.
    cluster.add();
    c0.session.messageTransfer(arg::content=makeMessage("2", "q", durableFlag));
    BOOST_CHECK_EQUAL("2", c0.lq.get(TIMEOUT).getData());
    s.acquire(s.getUnacquired());
    s.accept(s.getUnaccepted());

    // In the bug we now have 0 messages on cluster[0] and 1 message on cluster[1]
    // Subscribing on cluster[1] provokes an error that shuts down cluster[0]
    Client c1(cluster[1], "c1");
    Subscription s1 = c1.subs.subscribe(c1.lq, "q"); // Default auto-ack=1
    Message m;
    BOOST_CHECK(!c1.lq.get(m, TIMEOUT/10));
    BOOST_CHECK_EQUAL(c1.session.queueQuery("q").getMessageCount(), 0u);
    BOOST_CHECK_EQUAL(c0.session.queueQuery("q").getMessageCount(), 0u);
}

QPID_AUTO_TEST_SUITE_END()
}} // namespace qpid::tests
