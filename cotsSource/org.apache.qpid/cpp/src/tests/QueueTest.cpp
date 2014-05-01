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
#include "MessageUtils.h"
#include "unit_test.h"
#include "test_tools.h"
#include "qpid/Exception.h"
#include "qpid/broker/Broker.h"
#include "qpid/broker/DeliverableMessage.h"
#include "qpid/broker/FanOutExchange.h"
#include "qpid/broker/Queue.h"
#include "qpid/broker/Deliverable.h"
#include "qpid/broker/ExchangeRegistry.h"
#include "qpid/broker/QueueRegistry.h"
#include "qpid/broker/NullMessageStore.h"
#include "qpid/broker/ExpiryPolicy.h"
#include "qpid/framing/MessageTransferBody.h"
#include "qpid/client/QueueOptions.h"
#include "qpid/framing/AMQFrame.h"
#include "qpid/framing/MessageTransferBody.h"
#include "qpid/framing/reply_exceptions.h"
#include <iostream>
#include "boost/format.hpp"

using boost::intrusive_ptr;
using namespace qpid;
using namespace qpid::broker;
using namespace qpid::client;
using namespace qpid::framing;
using namespace qpid::sys;

namespace qpid {
namespace tests {

class TestConsumer : public virtual Consumer{
public:
    typedef boost::shared_ptr<TestConsumer> shared_ptr;

    intrusive_ptr<Message> last;
    bool received;
    TestConsumer(bool acquire = true):Consumer(acquire), received(false) {};

    virtual bool deliver(QueuedMessage& msg){
        last = msg.payload;
        received = true;
        return true;
    };
    void notify() {}
    OwnershipToken* getSession() { return 0; }
};

class FailOnDeliver : public Deliverable
{
    boost::intrusive_ptr<Message> msg;
public:
    FailOnDeliver() : msg(MessageUtils::createMessage()) {}
    void deliverTo(const boost::shared_ptr<Queue>& queue)
    {
        throw Exception(QPID_MSG("Invalid delivery to " << queue->getName()));
    }
    Message& getMessage() { return *(msg.get()); }
};

intrusive_ptr<Message> create_message(std::string exchange, std::string routingKey) {
    intrusive_ptr<Message> msg(new Message());
    AMQFrame method((MessageTransferBody(ProtocolVersion(), exchange, 0, 0)));
    AMQFrame header((AMQHeaderBody()));
    msg->getFrames().append(method);
    msg->getFrames().append(header);
    msg->getFrames().getHeaders()->get<DeliveryProperties>(true)->setRoutingKey(routingKey);
    return msg;
}

QPID_AUTO_TEST_SUITE(QueueTestSuite)

QPID_AUTO_TEST_CASE(testAsyncMessage) {
    Queue::shared_ptr queue(new Queue("my_test_queue", true));
    intrusive_ptr<Message> received;

    TestConsumer::shared_ptr c1(new TestConsumer());
    queue->consume(c1);


    //Test basic delivery:
    intrusive_ptr<Message> msg1 = create_message("e", "A");
    msg1->enqueueAsync(queue, 0);//this is done on enqueue which is not called from process
    queue->process(msg1);
    sleep(2);

    BOOST_CHECK(!c1->received);
    msg1->enqueueComplete();

    received = queue->get().payload;
    BOOST_CHECK_EQUAL(msg1.get(), received.get());
}


QPID_AUTO_TEST_CASE(testAsyncMessageCount){
    Queue::shared_ptr queue(new Queue("my_test_queue", true));
    intrusive_ptr<Message> msg1 = create_message("e", "A");
    msg1->enqueueAsync(queue, 0);//this is done on enqueue which is not called from process

    queue->process(msg1);
    sleep(2);
    uint32_t compval=0;
    BOOST_CHECK_EQUAL(compval, queue->getEnqueueCompleteMessageCount());
    msg1->enqueueComplete();
    compval=1;
    BOOST_CHECK_EQUAL(compval, queue->getEnqueueCompleteMessageCount());
    BOOST_CHECK_EQUAL(compval, queue->getMessageCount());
}

QPID_AUTO_TEST_CASE(testConsumers){
    Queue::shared_ptr queue(new Queue("my_queue", true));

    //Test adding consumers:
    TestConsumer::shared_ptr c1(new TestConsumer());
    TestConsumer::shared_ptr c2(new TestConsumer());
    queue->consume(c1);
    queue->consume(c2);

    BOOST_CHECK_EQUAL(uint32_t(2), queue->getConsumerCount());

    //Test basic delivery:
    intrusive_ptr<Message> msg1 = create_message("e", "A");
    intrusive_ptr<Message> msg2 = create_message("e", "B");
    intrusive_ptr<Message> msg3 = create_message("e", "C");

    queue->deliver(msg1);
    BOOST_CHECK(queue->dispatch(c1));
    BOOST_CHECK_EQUAL(msg1.get(), c1->last.get());

    queue->deliver(msg2);
    BOOST_CHECK(queue->dispatch(c2));
    BOOST_CHECK_EQUAL(msg2.get(), c2->last.get());

    c1->received = false;
    queue->deliver(msg3);
    BOOST_CHECK(queue->dispatch(c1));
    BOOST_CHECK_EQUAL(msg3.get(), c1->last.get());

    //Test cancellation:
    queue->cancel(c1);
    BOOST_CHECK_EQUAL(uint32_t(1), queue->getConsumerCount());
    queue->cancel(c2);
    BOOST_CHECK_EQUAL(uint32_t(0), queue->getConsumerCount());
}

QPID_AUTO_TEST_CASE(testRegistry){
    //Test use of queues in registry:
    QueueRegistry registry;
    registry.declare("queue1", true, true);
    registry.declare("queue2", true, true);
    registry.declare("queue3", true, true);

    BOOST_CHECK(registry.find("queue1"));
    BOOST_CHECK(registry.find("queue2"));
    BOOST_CHECK(registry.find("queue3"));

    registry.destroy("queue1");
    registry.destroy("queue2");
    registry.destroy("queue3");

    BOOST_CHECK(!registry.find("queue1"));
    BOOST_CHECK(!registry.find("queue2"));
    BOOST_CHECK(!registry.find("queue3"));
}

QPID_AUTO_TEST_CASE(testDequeue){
    Queue::shared_ptr queue(new Queue("my_queue", true));
    intrusive_ptr<Message> msg1 = create_message("e", "A");
    intrusive_ptr<Message> msg2 = create_message("e", "B");
    intrusive_ptr<Message> msg3 = create_message("e", "C");
    intrusive_ptr<Message> received;

    queue->deliver(msg1);
    queue->deliver(msg2);
    queue->deliver(msg3);

    BOOST_CHECK_EQUAL(uint32_t(3), queue->getMessageCount());

    received = queue->get().payload;
    BOOST_CHECK_EQUAL(msg1.get(), received.get());
    BOOST_CHECK_EQUAL(uint32_t(2), queue->getMessageCount());

    received = queue->get().payload;
    BOOST_CHECK_EQUAL(msg2.get(), received.get());
    BOOST_CHECK_EQUAL(uint32_t(1), queue->getMessageCount());

    TestConsumer::shared_ptr consumer(new TestConsumer());
    queue->consume(consumer);
    queue->dispatch(consumer);
    if (!consumer->received)
        sleep(2);

    BOOST_CHECK_EQUAL(msg3.get(), consumer->last.get());
    BOOST_CHECK_EQUAL(uint32_t(0), queue->getMessageCount());

    received = queue->get().payload;
    BOOST_CHECK(!received);
    BOOST_CHECK_EQUAL(uint32_t(0), queue->getMessageCount());

}

QPID_AUTO_TEST_CASE(testBound){
    //test the recording of bindings, and use of those to allow a queue to be unbound
    string key("my-key");
    FieldTable args;

    Queue::shared_ptr queue(new Queue("my-queue", true));
    ExchangeRegistry exchanges;
    //establish bindings from exchange->queue and notify the queue as it is bound:
    Exchange::shared_ptr exchange1 = exchanges.declare("my-exchange-1", "direct").first;
    exchange1->bind(queue, key, &args);
    queue->bound(exchange1->getName(), key, args);

    Exchange::shared_ptr exchange2 = exchanges.declare("my-exchange-2", "fanout").first;
    exchange2->bind(queue, key, &args);
    queue->bound(exchange2->getName(), key, args);

    Exchange::shared_ptr exchange3 = exchanges.declare("my-exchange-3", "topic").first;
    exchange3->bind(queue, key, &args);
    queue->bound(exchange3->getName(), key, args);

    //delete one of the exchanges:
    exchanges.destroy(exchange2->getName());
    exchange2.reset();

    //unbind the queue from all exchanges it knows it has been bound to:
    queue->unbind(exchanges, queue);

    //ensure the remaining exchanges don't still have the queue bound to them:
    FailOnDeliver deliverable;
    exchange1->route(deliverable, key, &args);
    exchange3->route(deliverable, key, &args);
}

QPID_AUTO_TEST_CASE(testPersistLastNodeStanding){
    client::QueueOptions args;
	args.setPersistLastNode();

	Queue::shared_ptr queue(new Queue("my-queue", true));
    queue->configure(args);

    intrusive_ptr<Message> msg1 = create_message("e", "A");
    intrusive_ptr<Message> msg2 = create_message("e", "B");
    intrusive_ptr<Message> msg3 = create_message("e", "C");

	//enqueue 2 messages
    queue->deliver(msg1);
    queue->deliver(msg2);

	//change mode
	queue->setLastNodeFailure();

	//enqueue 1 message
    queue->deliver(msg3);

	//check all have persistent ids.
    BOOST_CHECK(msg1->isPersistent());
    BOOST_CHECK(msg2->isPersistent());
    BOOST_CHECK(msg3->isPersistent());

}


QPID_AUTO_TEST_CASE(testSeek){

	Queue::shared_ptr queue(new Queue("my-queue", true));

    intrusive_ptr<Message> msg1 = create_message("e", "A");
    intrusive_ptr<Message> msg2 = create_message("e", "B");
    intrusive_ptr<Message> msg3 = create_message("e", "C");

	//enqueue 2 messages
    queue->deliver(msg1);
    queue->deliver(msg2);
    queue->deliver(msg3);

    TestConsumer::shared_ptr consumer(new TestConsumer(false));
    SequenceNumber seq(2);
    consumer->position = seq;

    QueuedMessage qm;
    queue->dispatch(consumer);
    
    BOOST_CHECK_EQUAL(msg3.get(), consumer->last.get());
    queue->dispatch(consumer);
    queue->dispatch(consumer); // make sure over-run is safe
 
}

QPID_AUTO_TEST_CASE(testSearch){

	Queue::shared_ptr queue(new Queue("my-queue", true));

    intrusive_ptr<Message> msg1 = create_message("e", "A");
    intrusive_ptr<Message> msg2 = create_message("e", "B");
    intrusive_ptr<Message> msg3 = create_message("e", "C");

	//enqueue 2 messages
    queue->deliver(msg1);
    queue->deliver(msg2);
    queue->deliver(msg3);

    SequenceNumber seq(2);
    QueuedMessage qm = queue->find(seq);
    
    BOOST_CHECK_EQUAL(seq.getValue(), qm.position.getValue());
    
    queue->acquire(qm);
    BOOST_CHECK_EQUAL(queue->getMessageCount(), 2u);
    SequenceNumber seq1(3);
    QueuedMessage qm1 = queue->find(seq1);
    BOOST_CHECK_EQUAL(seq1.getValue(), qm1.position.getValue());
    
}
const std::string nullxid = "";

class SimpleDummyCtxt : public TransactionContext {};

class DummyCtxt : public TPCTransactionContext
{
    const std::string xid;
  public:
    DummyCtxt(const std::string& _xid) : xid(_xid) {}
    static std::string getXid(TransactionContext& ctxt)
    {
        DummyCtxt* c(dynamic_cast<DummyCtxt*>(&ctxt));
        return c ? c->xid : nullxid;
    }
};

class TestMessageStoreOC : public MessageStore
{
    std::set<std::string> prepared;
    uint64_t nextPersistenceId;
  public:

    uint enqCnt;
    uint deqCnt;
    bool error;

    TestMessageStoreOC() : MessageStore(),nextPersistenceId(1),enqCnt(0),deqCnt(0),error(false) {}
    ~TestMessageStoreOC(){}

    virtual void dequeue(TransactionContext*,
                 const boost::intrusive_ptr<PersistableMessage>& /*msg*/,
                 const PersistableQueue& /*queue*/)
    {
        if (error) throw Exception("Dequeue error test");
        deqCnt++;
    }

    virtual void enqueue(TransactionContext*,
                 const boost::intrusive_ptr<PersistableMessage>& msg,
                 const PersistableQueue& /* queue */)
    {
        if (error) throw Exception("Enqueue error test");
        enqCnt++;
        msg->enqueueComplete();
    }

    void createError()
    {
        error=true;
    }

    bool init(const Options*) { return true; }
    void truncateInit(const bool) {}
    void create(PersistableQueue& queue, const framing::FieldTable&) { queue.setPersistenceId(nextPersistenceId++); }
    void destroy(PersistableQueue&) {}
    void create(const PersistableExchange& exchange, const framing::FieldTable&) { exchange.setPersistenceId(nextPersistenceId++); }
    void destroy(const PersistableExchange&) {}
    void bind(const PersistableExchange&, const PersistableQueue&, const std::string&, const framing::FieldTable&) {}
    void unbind(const PersistableExchange&, const PersistableQueue&, const std::string&, const framing::FieldTable&) {}
    void create(const PersistableConfig& config) { config.setPersistenceId(nextPersistenceId++); }
    void destroy(const PersistableConfig&) {}
    void stage(const boost::intrusive_ptr<PersistableMessage>&) {}
    void destroy(PersistableMessage&) {}
    void appendContent(const boost::intrusive_ptr<const PersistableMessage>&, const std::string&) {}
    void loadContent(const qpid::broker::PersistableQueue&, const boost::intrusive_ptr<const PersistableMessage>&,
                    std::string&, uint64_t, uint32_t) { throw qpid::framing::InternalErrorException("Can't load content; persistence not enabled"); }
    void flush(const qpid::broker::PersistableQueue&) {}
    uint32_t outstandingQueueAIO(const PersistableQueue&) { return 0; }

    std::auto_ptr<TransactionContext> begin() { return std::auto_ptr<TransactionContext>(new SimpleDummyCtxt()); }
    std::auto_ptr<TPCTransactionContext> begin(const std::string& xid) { return std::auto_ptr<TPCTransactionContext>(new DummyCtxt(xid)); }
    void prepare(TPCTransactionContext& ctxt) { prepared.insert(DummyCtxt::getXid(ctxt)); }
    void commit(TransactionContext& ctxt) { prepared.erase(DummyCtxt::getXid(ctxt)); }
    void abort(TransactionContext& ctxt) { prepared.erase(DummyCtxt::getXid(ctxt)); }
    void collectPreparedXids(std::set<std::string>& out) { out.insert(prepared.begin(), prepared.end()); }

    void recover(RecoveryManager&) {}
};


QPID_AUTO_TEST_CASE(testLVQOrdering){

    client::QueueOptions args;
    // set queue mode
	args.setOrdering(client::LVQ);

	Queue::shared_ptr queue(new Queue("my-queue", true ));
	queue->configure(args);

    intrusive_ptr<Message> msg1 = create_message("e", "A");
    intrusive_ptr<Message> msg2 = create_message("e", "B");
    intrusive_ptr<Message> msg3 = create_message("e", "C");
    intrusive_ptr<Message> msg4 = create_message("e", "D");
    intrusive_ptr<Message> received;

    //set deliever match for LVQ a,b,c,a

    string key;
	args.getLVQKey(key);
    BOOST_CHECK_EQUAL(key, "qpid.LVQ_key");


	msg1->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"a");
	msg2->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"b");
	msg3->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"c");
	msg4->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"a");

	//enqueue 4 message
    queue->deliver(msg1);
    queue->deliver(msg2);
    queue->deliver(msg3);
    queue->deliver(msg4);

    BOOST_CHECK_EQUAL(queue->getMessageCount(), 3u);

    received = queue->get().payload;
    BOOST_CHECK_EQUAL(msg4.get(), received.get());

    received = queue->get().payload;
    BOOST_CHECK_EQUAL(msg2.get(), received.get());

    received = queue->get().payload;
    BOOST_CHECK_EQUAL(msg3.get(), received.get());

    intrusive_ptr<Message> msg5 = create_message("e", "A");
    intrusive_ptr<Message> msg6 = create_message("e", "B");
    intrusive_ptr<Message> msg7 = create_message("e", "C");
	msg5->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"a");
	msg6->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"b");
	msg7->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"c");
    queue->deliver(msg5);
    queue->deliver(msg6);
    queue->deliver(msg7);

    BOOST_CHECK_EQUAL(queue->getMessageCount(), 3u);

    received = queue->get().payload;
    BOOST_CHECK_EQUAL(msg5.get(), received.get());

    received = queue->get().payload;
    BOOST_CHECK_EQUAL(msg6.get(), received.get());

    received = queue->get().payload;
    BOOST_CHECK_EQUAL(msg7.get(), received.get());

}

QPID_AUTO_TEST_CASE(testLVQEmptyKey){

    client::QueueOptions args;
    // set queue mode
    args.setOrdering(client::LVQ);

    Queue::shared_ptr queue(new Queue("my-queue", true ));
    queue->configure(args);

    intrusive_ptr<Message> msg1 = create_message("e", "A");
    intrusive_ptr<Message> msg2 = create_message("e", "B");

    string key;
    args.getLVQKey(key);
    BOOST_CHECK_EQUAL(key, "qpid.LVQ_key");


    msg1->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"a");
    queue->deliver(msg1);
    queue->deliver(msg2);
    BOOST_CHECK_EQUAL(queue->getMessageCount(), 2u);

}

QPID_AUTO_TEST_CASE(testLVQAcquire){

    client::QueueOptions args;
    // set queue mode
    args.setOrdering(client::LVQ);

    Queue::shared_ptr queue(new Queue("my-queue", true ));
    queue->configure(args);

    intrusive_ptr<Message> msg1 = create_message("e", "A");
    intrusive_ptr<Message> msg2 = create_message("e", "B");
    intrusive_ptr<Message> msg3 = create_message("e", "C");
    intrusive_ptr<Message> msg4 = create_message("e", "D");
    intrusive_ptr<Message> msg5 = create_message("e", "F");
    intrusive_ptr<Message> msg6 = create_message("e", "G");

    //set deliever match for LVQ a,b,c,a

    string key;
    args.getLVQKey(key);
    BOOST_CHECK_EQUAL(key, "qpid.LVQ_key");


    msg1->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"a");
    msg2->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"b");
    msg3->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"c");
    msg4->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"a");
    msg5->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"b");
    msg6->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"c");

    //enqueue 4 message
    queue->deliver(msg1);
    queue->deliver(msg2);
    queue->deliver(msg3);
    queue->deliver(msg4);

    BOOST_CHECK_EQUAL(queue->getMessageCount(), 3u);

    framing::SequenceNumber sequence(1);
    QueuedMessage qmsg(queue.get(), msg1, sequence);
    QueuedMessage qmsg2(queue.get(), msg2, ++sequence);
    framing::SequenceNumber sequence1(10);
    QueuedMessage qmsg3(queue.get(), 0, sequence1);

    BOOST_CHECK(!queue->acquire(qmsg));
    BOOST_CHECK(queue->acquire(qmsg2));
    // Acquire the massage again to test failure case.
    BOOST_CHECK(!queue->acquire(qmsg2));
    BOOST_CHECK(!queue->acquire(qmsg3));

    BOOST_CHECK_EQUAL(queue->getMessageCount(), 2u);

    queue->deliver(msg5);
    BOOST_CHECK_EQUAL(queue->getMessageCount(), 3u);

    // set mode to no browse and check
    args.setOrdering(client::LVQ_NO_BROWSE);
    queue->configure(args);
    TestConsumer::shared_ptr c1(new TestConsumer(false));

    queue->dispatch(c1);
    queue->dispatch(c1);
    queue->dispatch(c1);

    queue->deliver(msg6);
    BOOST_CHECK_EQUAL(queue->getMessageCount(), 3u);

    intrusive_ptr<Message> received;
    received = queue->get().payload;
    BOOST_CHECK_EQUAL(msg4.get(), received.get());

}

QPID_AUTO_TEST_CASE(testLVQMultiQueue){

    client::QueueOptions args;
    // set queue mode
    args.setOrdering(client::LVQ);

    Queue::shared_ptr queue1(new Queue("my-queue", true ));
    Queue::shared_ptr queue2(new Queue("my-queue", true ));
    intrusive_ptr<Message> received;
    queue1->configure(args);
    queue2->configure(args);

    intrusive_ptr<Message> msg1 = create_message("e", "A");
    intrusive_ptr<Message> msg2 = create_message("e", "A");

    string key;
    args.getLVQKey(key);
    BOOST_CHECK_EQUAL(key, "qpid.LVQ_key");

    msg1->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"a");
    msg2->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"a");

    queue1->deliver(msg1);
    queue2->deliver(msg1);
    queue1->deliver(msg2);

    received = queue1->get().payload;
    BOOST_CHECK_EQUAL(msg2.get(), received.get());

    received = queue2->get().payload;
    BOOST_CHECK_EQUAL(msg1.get(), received.get());

}

QPID_AUTO_TEST_CASE(testLVQRecover){

/* simulate this
  1. start 2 nodes
  2. create cluster durable lvq
  3. send a transient message to the queue
  4. kill one of the nodes (to trigger force persistent behaviour)...
  5. then restart it (to turn off force persistent behaviour)
  6. send another transient message with same lvq key as in 3
  7. kill the second node again (retrigger force persistent)
  8. stop and recover the first node
*/
    TestMessageStoreOC  testStore;
    client::QueueOptions args;
    // set queue mode
    args.setOrdering(client::LVQ);
    args.setPersistLastNode();

    Queue::shared_ptr queue1(new Queue("my-queue", true, &testStore));
    intrusive_ptr<Message> received;
    queue1->configure(args);

    intrusive_ptr<Message> msg1 = create_message("e", "A");
    intrusive_ptr<Message> msg2 = create_message("e", "A");
    // 2
    string key;
    args.getLVQKey(key);
    BOOST_CHECK_EQUAL(key, "qpid.LVQ_key");

    msg1->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"a");
    msg2->getProperties<MessageProperties>()->getApplicationHeaders().setString(key,"a");
	// 3
    queue1->deliver(msg1);
    // 4
    queue1->setLastNodeFailure();
    BOOST_CHECK_EQUAL(testStore.enqCnt, 1u);
    // 5
    queue1->clearLastNodeFailure();
    BOOST_CHECK_EQUAL(testStore.enqCnt, 1u);
    // 6
    queue1->deliver(msg2);
    BOOST_CHECK_EQUAL(testStore.enqCnt, 1u);
    queue1->setLastNodeFailure();
    BOOST_CHECK_EQUAL(testStore.enqCnt, 2u);
    BOOST_CHECK_EQUAL(testStore.deqCnt, 1u);
}

void addMessagesToQueue(uint count, Queue& queue, uint oddTtl = 200, uint evenTtl = 0)
{
    for (uint i = 0; i < count; i++) {
        intrusive_ptr<Message> m = create_message("exchange", "key");
        if (i % 2) {
            if (oddTtl) m->getProperties<DeliveryProperties>()->setTtl(oddTtl);
        } else {
            if (evenTtl) m->getProperties<DeliveryProperties>()->setTtl(evenTtl);
        }
        m->setTimestamp(new broker::ExpiryPolicy);
        queue.deliver(m);
    }
}

QPID_AUTO_TEST_CASE(testPurgeExpired) {
    Queue queue("my-queue");
    addMessagesToQueue(10, queue);
    BOOST_CHECK_EQUAL(queue.getMessageCount(), 10u);
    ::usleep(300*1000);
    queue.purgeExpired();
    BOOST_CHECK_EQUAL(queue.getMessageCount(), 5u);
}

QPID_AUTO_TEST_CASE(testQueueCleaner) {
    Timer timer;
    QueueRegistry queues;
    Queue::shared_ptr queue = queues.declare("my-queue").first;
    addMessagesToQueue(10, *queue, 200, 400);
    BOOST_CHECK_EQUAL(queue->getMessageCount(), 10u);

    QueueCleaner cleaner(queues, timer);
    cleaner.start(100 * qpid::sys::TIME_MSEC);
    ::usleep(300*1000);
    BOOST_CHECK_EQUAL(queue->getMessageCount(), 5u);
    ::usleep(300*1000);
    BOOST_CHECK_EQUAL(queue->getMessageCount(), 0u);
}

QPID_AUTO_TEST_CASE(testMultiQueueLastNode){

    TestMessageStoreOC  testStore;
    client::QueueOptions args;
    args.setPersistLastNode();

    Queue::shared_ptr queue1(new Queue("queue1", true, &testStore ));
    queue1->configure(args);
    Queue::shared_ptr queue2(new Queue("queue2", true, &testStore ));
    queue2->configure(args);

    intrusive_ptr<Message> msg1 = create_message("e", "A");

    queue1->deliver(msg1);
    queue2->deliver(msg1);

    //change mode
    queue1->setLastNodeFailure();
    BOOST_CHECK_EQUAL(testStore.enqCnt, 1u);
    queue2->setLastNodeFailure();
    BOOST_CHECK_EQUAL(testStore.enqCnt, 2u);

    // check they don't get stored twice
    queue1->setLastNodeFailure();
    queue2->setLastNodeFailure();
    BOOST_CHECK_EQUAL(testStore.enqCnt, 2u);

    intrusive_ptr<Message> msg2 = create_message("e", "B");
    queue1->deliver(msg2);
    queue2->deliver(msg2);

    queue1->clearLastNodeFailure();
    queue2->clearLastNodeFailure();
    // check only new messages get forced
    queue1->setLastNodeFailure();
    queue2->setLastNodeFailure();
    BOOST_CHECK_EQUAL(testStore.enqCnt, 4u);

    // check no failure messages are stored
    queue1->clearLastNodeFailure();
    queue2->clearLastNodeFailure();

    intrusive_ptr<Message> msg3 = create_message("e", "B");
    queue1->deliver(msg3);
    queue2->deliver(msg3);
    BOOST_CHECK_EQUAL(testStore.enqCnt, 4u);
    queue1->setLastNodeFailure();
    queue2->setLastNodeFailure();
    BOOST_CHECK_EQUAL(testStore.enqCnt, 6u);

    // check requeue 1
    intrusive_ptr<Message> msg4 = create_message("e", "C");
    intrusive_ptr<Message> msg5 = create_message("e", "D");

    framing::SequenceNumber sequence(1);
    QueuedMessage qmsg1(queue1.get(), msg4, sequence);
    QueuedMessage qmsg2(queue2.get(), msg5, ++sequence);

    queue1->requeue(qmsg1);
    BOOST_CHECK_EQUAL(testStore.enqCnt, 7u);

    // check requeue 2
    queue2->clearLastNodeFailure();
    queue2->requeue(qmsg2);
    BOOST_CHECK_EQUAL(testStore.enqCnt, 7u);
    queue2->setLastNodeFailure();
    BOOST_CHECK_EQUAL(testStore.enqCnt, 8u);

    queue2->clearLastNodeFailure();
    queue2->setLastNodeFailure();
    BOOST_CHECK_EQUAL(testStore.enqCnt, 8u);
}

QPID_AUTO_TEST_CASE(testLastNodeRecoverAndFail){
/*
simulate this:
  1. start two nodes
  2. create cluster durable queue and add some messages
  3. kill one node (trigger force-persistent behaviour)
  4. stop and recover remaining node
  5. add another node
  6. kill that new node again
make sure that an attempt to re-enqueue a message does not happen which will
result in the last man standing exiting with an error.

we need to make sure that recover is safe, i.e. messages are
not requeued to the store.
*/
    TestMessageStoreOC  testStore;
    client::QueueOptions args;
    // set queue mode
    args.setPersistLastNode();

    Queue::shared_ptr queue1(new Queue("my-queue", true, &testStore));
    intrusive_ptr<Message> received;
    queue1->configure(args);

    // check requeue 1
    intrusive_ptr<Message> msg1 = create_message("e", "C");
    intrusive_ptr<Message> msg2 = create_message("e", "D");

    queue1->recover(msg1);

    queue1->setLastNodeFailure();
    BOOST_CHECK_EQUAL(testStore.enqCnt, 0u);

    queue1->clearLastNodeFailure();
    BOOST_CHECK_EQUAL(testStore.enqCnt, 0u);

    queue1->deliver(msg2);
    BOOST_CHECK_EQUAL(testStore.enqCnt, 0u);
    queue1->setLastNodeFailure();
    BOOST_CHECK_EQUAL(testStore.enqCnt, 1u);

}

QPID_AUTO_TEST_CASE(testLastNodeJournalError){
/*
simulate store exception going into last node standing

*/
    TestMessageStoreOC  testStore;
    client::QueueOptions args;
    // set queue mode
    args.setPersistLastNode();

    Queue::shared_ptr queue1(new Queue("my-queue", true, &testStore));
    intrusive_ptr<Message> received;
    queue1->configure(args);

    // check requeue 1
    intrusive_ptr<Message> msg1 = create_message("e", "C");

    queue1->deliver(msg1);
    testStore.createError();

    ScopedSuppressLogging sl; // Suppress messages for expected errors.
    queue1->setLastNodeFailure();
    BOOST_CHECK_EQUAL(testStore.enqCnt, 0u);

}

intrusive_ptr<Message> mkMsg(MessageStore& store, std::string content = "", bool durable = false)
{
    intrusive_ptr<Message> msg = MessageUtils::createMessage("", "", durable);
    if (content.size()) MessageUtils::addContent(msg, content);
    msg->setStore(&store);
    return msg;
}

QPID_AUTO_TEST_CASE(testFlowToDiskBlocking){

    TestMessageStoreOC  testStore;
    client::QueueOptions args0; // No size policy
    client::QueueOptions args1;
    args1.setSizePolicy(FLOW_TO_DISK, 0, 1);
    client::QueueOptions args2;
    args2.setSizePolicy(FLOW_TO_DISK, 0, 2);

    // --- Fanout exchange bound to single transient queue -------------------------------------------------------------

    FanOutExchange sbtFanout1("sbtFanout1", false, args0); // single binding to transient queue
    Queue::shared_ptr tq1(new Queue("tq1", true)); // transient w/ limit
    tq1->configure(args1);
    sbtFanout1.bind(tq1, "", 0);

    intrusive_ptr<Message> msg01 = mkMsg(testStore, std::string(5, 'X'));  // transient w/ content
    DeliverableMessage dmsg01(msg01);
    sbtFanout1.route(dmsg01, "", 0); // Brings queue 1 to capacity limit
    msg01->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg01->isContentReleased(), false);
    BOOST_CHECK_EQUAL(1u, tq1->getMessageCount());

    intrusive_ptr<Message> msg02 = mkMsg(testStore, std::string(5, 'X'));  // transient w/ content
    DeliverableMessage dmsg02(msg02);
    BOOST_CHECK_THROW(sbtFanout1.route(dmsg02, "", 0), ResourceLimitExceededException);
    msg02->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg02->isContentReleased(), false);
    BOOST_CHECK_EQUAL(1u, tq1->getMessageCount());

    intrusive_ptr<Message> msg03 = mkMsg(testStore, std::string(5, 'X'), true);  // durable w/ content
    DeliverableMessage dmsg03(msg03);
    BOOST_CHECK_THROW(sbtFanout1.route(dmsg03, "", 0), ResourceLimitExceededException);
    msg03->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg03->isContentReleased(), false);
    BOOST_CHECK_EQUAL(1u, tq1->getMessageCount());

    intrusive_ptr<Message> msg04 = mkMsg(testStore); // transient no content
    DeliverableMessage dmsg04(msg04);
    BOOST_CHECK_THROW(sbtFanout1.route(dmsg04, "", 0), ResourceLimitExceededException);
    msg04->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg04->isContentReleased(), false);
    BOOST_CHECK_EQUAL(1u, tq1->getMessageCount());

    intrusive_ptr<Message> msg05 = mkMsg(testStore, "", true); // durable no content
    DeliverableMessage dmsg05(msg05);
    BOOST_CHECK_THROW(sbtFanout1.route(dmsg05, "", 0), ResourceLimitExceededException);
    msg05->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg05->isContentReleased(), false);
    BOOST_CHECK_EQUAL(1u, tq1->getMessageCount());

    // --- Fanout exchange bound to single durable queue ---------------------------------------------------------------

    FanOutExchange sbdFanout2("sbdFanout2", false, args0); // single binding to durable queue
    Queue::shared_ptr dq2(new Queue("dq2", true, &testStore)); // durable w/ limit
    dq2->configure(args1);
    sbdFanout2.bind(dq2, "", 0);

    intrusive_ptr<Message> msg06 = mkMsg(testStore, std::string(5, 'X'));  // transient w/ content
    DeliverableMessage dmsg06(msg06);
    sbdFanout2.route(dmsg06, "", 0); // Brings queue 2 to capacity limit
    msg06->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg06->isContentReleased(), false);
    BOOST_CHECK_EQUAL(1u, dq2->getMessageCount());

    intrusive_ptr<Message> msg07 = mkMsg(testStore, std::string(5, 'X'));  // transient w/ content
    DeliverableMessage dmsg07(msg07);
    sbdFanout2.route(dmsg07, "", 0);
    msg07->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg07->isContentReleased(), true);
    BOOST_CHECK_EQUAL(2u, dq2->getMessageCount());

    intrusive_ptr<Message> msg08 = mkMsg(testStore, std::string(5, 'X'), true);  // durable w/ content
    DeliverableMessage dmsg08(msg08);
    sbdFanout2.route(dmsg08, "", 0);
    msg08->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg08->isContentReleased(), true);
    BOOST_CHECK_EQUAL(3u, dq2->getMessageCount());

    intrusive_ptr<Message> msg09 = mkMsg(testStore);  // transient no content
    DeliverableMessage dmsg09(msg09);
    sbdFanout2.route(dmsg09, "", 0);
    msg09->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg09->isContentReleased(), true);
    BOOST_CHECK_EQUAL(4u, dq2->getMessageCount());

    intrusive_ptr<Message> msg10 = mkMsg(testStore, "", true);  // durable no content
    DeliverableMessage dmsg10(msg10);
    sbdFanout2.route(dmsg10, "", 0);
    msg10->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg10->isContentReleased(), true);
    BOOST_CHECK_EQUAL(5u, dq2->getMessageCount());

    // --- Fanout exchange bound to multiple durable queues ------------------------------------------------------------

    FanOutExchange mbdFanout3("mbdFanout3", false, args0); // multiple bindings to durable queues
    Queue::shared_ptr dq3(new Queue("dq3", true, &testStore)); // durable w/ limit 2
    dq3->configure(args2);
    mbdFanout3.bind(dq3, "", 0);
    Queue::shared_ptr dq4(new Queue("dq4", true, &testStore)); // durable w/ limit 1
    dq4->configure(args1);
    mbdFanout3.bind(dq4, "", 0);
    Queue::shared_ptr dq5(new Queue("dq5", true, &testStore)); // durable no limit
    dq5->configure(args0);
    mbdFanout3.bind(dq5, "", 0);

    intrusive_ptr<Message> msg11 = mkMsg(testStore, std::string(5, 'X'));  // transient w/ content
    DeliverableMessage dmsg11(msg11);
    mbdFanout3.route(dmsg11, "", 0); // Brings queues 3 and 4 to capacity limit
    msg11->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg11->isContentReleased(), false);
    BOOST_CHECK_EQUAL(1u, dq3->getMessageCount());
    BOOST_CHECK_EQUAL(1u, dq4->getMessageCount());
    BOOST_CHECK_EQUAL(1u, dq5->getMessageCount());

    intrusive_ptr<Message> msg12 = mkMsg(testStore, std::string(5, 'X'));  // transient w/ content
    DeliverableMessage dmsg12(msg12);
    mbdFanout3.route(dmsg12, "", 0);
    msg12->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg12->isContentReleased(), false); // XXXX - consequence of transient msg multi-queue ftd policy-handling limitations, fix in broker at some point!
    BOOST_CHECK_EQUAL(2u, dq3->getMessageCount());
    BOOST_CHECK_EQUAL(2u, dq4->getMessageCount());
    BOOST_CHECK_EQUAL(2u, dq5->getMessageCount());

    intrusive_ptr<Message> msg13 = mkMsg(testStore, std::string(5, 'X'), true);  // durable w/ content
    DeliverableMessage dmsg13(msg13);
    mbdFanout3.route(dmsg13, "", 0);
    msg13->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg13->isContentReleased(), true);
    BOOST_CHECK_EQUAL(3u, dq3->getMessageCount());
    BOOST_CHECK_EQUAL(3u, dq4->getMessageCount());
    BOOST_CHECK_EQUAL(3u, dq5->getMessageCount());

    intrusive_ptr<Message> msg14 = mkMsg(testStore);  // transient no content
    DeliverableMessage dmsg14(msg14);
    mbdFanout3.route(dmsg14, "", 0);
    msg14->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg14->isContentReleased(), false); // XXXX - consequence of transient msg multi-queue ftd policy-handling limitations, fix in broker at some point!
    BOOST_CHECK_EQUAL(4u, dq3->getMessageCount());
    BOOST_CHECK_EQUAL(4u, dq4->getMessageCount());
    BOOST_CHECK_EQUAL(4u, dq5->getMessageCount());

    intrusive_ptr<Message> msg15 = mkMsg(testStore, "", true);  // durable no content
    DeliverableMessage dmsg15(msg15);
    mbdFanout3.route(dmsg15, "", 0);
    msg15->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg15->isContentReleased(), true);
    BOOST_CHECK_EQUAL(5u, dq3->getMessageCount());
    BOOST_CHECK_EQUAL(5u, dq4->getMessageCount());
    BOOST_CHECK_EQUAL(5u, dq5->getMessageCount());

    // Bind a transient queue, this should block the release of any further messages.
    // Note: this will result in a violation of the count policy of dq3 and dq4 - but this
    // is expected until a better overall multi-queue design is implemented. Similarly
    // for the other tests in this section.

    Queue::shared_ptr tq6(new Queue("tq6", true)); // transient no limit
    tq6->configure(args0);
    mbdFanout3.bind(tq6, "", 0);

    intrusive_ptr<Message> msg16 = mkMsg(testStore, std::string(5, 'X'));  // transient w/ content
    DeliverableMessage dmsg16(msg16);
    mbdFanout3.route(dmsg16, "", 0);
    msg16->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg16->isContentReleased(), false);
    BOOST_CHECK_EQUAL(6u, dq3->getMessageCount());
    BOOST_CHECK_EQUAL(6u, dq4->getMessageCount());
    BOOST_CHECK_EQUAL(6u, dq5->getMessageCount());

    intrusive_ptr<Message> msg17 = mkMsg(testStore, std::string(5, 'X'), true);  // durable w/ content
    DeliverableMessage dmsg17(msg17);
    mbdFanout3.route(dmsg17, "", 0);
    msg17->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg17->isContentReleased(), false);
    BOOST_CHECK_EQUAL(7u, dq3->getMessageCount());
    BOOST_CHECK_EQUAL(7u, dq4->getMessageCount());
    BOOST_CHECK_EQUAL(7u, dq5->getMessageCount());

    intrusive_ptr<Message> msg18 = mkMsg(testStore);  // transient no content
    DeliverableMessage dmsg18(msg18);
    mbdFanout3.route(dmsg18, "", 0);
    msg18->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg18->isContentReleased(), false);
    BOOST_CHECK_EQUAL(8u, dq3->getMessageCount());
    BOOST_CHECK_EQUAL(8u, dq4->getMessageCount());
    BOOST_CHECK_EQUAL(8u, dq5->getMessageCount());

    intrusive_ptr<Message> msg19 = mkMsg(testStore, "", true);  // durable no content
    DeliverableMessage dmsg19(msg19);
    mbdFanout3.route(dmsg19, "", 0);
    msg19->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg19->isContentReleased(), false);
    BOOST_CHECK_EQUAL(9u, dq3->getMessageCount());
    BOOST_CHECK_EQUAL(9u, dq4->getMessageCount());
    BOOST_CHECK_EQUAL(9u, dq5->getMessageCount());


    // --- Fanout exchange bound to multiple durable and transient queues ----------------------------------------------

    FanOutExchange mbmFanout4("mbmFanout4", false, args0); // multiple bindings to durable/transient queues
    Queue::shared_ptr dq7(new Queue("dq7", true, &testStore)); // durable no limit
    dq7->configure(args0);
    mbmFanout4.bind(dq7, "", 0);
    Queue::shared_ptr dq8(new Queue("dq8", true, &testStore)); // durable w/ limit
    dq8->configure(args1);
    mbmFanout4.bind(dq8, "", 0);
    Queue::shared_ptr tq9(new Queue("tq9", true)); // transient no limit
    tq9->configure(args0);
    mbmFanout4.bind(tq9, "", 0);

    intrusive_ptr<Message> msg20 = mkMsg(testStore, std::string(5, 'X'));  // transient w/ content
    DeliverableMessage dmsg20(msg20);
    mbmFanout4.route(dmsg20, "", 0); // Brings queue 7 to capacity limit
    msg20->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg20->isContentReleased(), false);
    BOOST_CHECK_EQUAL(1u, dq7->getMessageCount());
    BOOST_CHECK_EQUAL(1u, dq8->getMessageCount());
    BOOST_CHECK_EQUAL(1u, tq9->getMessageCount());

    intrusive_ptr<Message> msg21 = mkMsg(testStore, std::string(5, 'X'));  // transient w/ content
    DeliverableMessage dmsg21(msg21);
    mbmFanout4.route(dmsg21, "", 0);
    msg21->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg21->isContentReleased(), false);
    BOOST_CHECK_EQUAL(2u, dq7->getMessageCount()); // over limit
    BOOST_CHECK_EQUAL(2u, dq8->getMessageCount());
    BOOST_CHECK_EQUAL(2u, tq9->getMessageCount());

    intrusive_ptr<Message> msg22 = mkMsg(testStore, std::string(5, 'X'), true);  // durable w/ content
    DeliverableMessage dmsg22(msg22);
    mbmFanout4.route(dmsg22, "", 0);
    msg22->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg22->isContentReleased(), false);
    BOOST_CHECK_EQUAL(3u, dq7->getMessageCount()); // over limit
    BOOST_CHECK_EQUAL(3u, dq8->getMessageCount()); // over limit
    BOOST_CHECK_EQUAL(3u, tq9->getMessageCount());

    intrusive_ptr<Message> msg23 = mkMsg(testStore);  // transient no content
    DeliverableMessage dmsg23(msg23);
    mbmFanout4.route(dmsg23, "", 0);
    msg23->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg23->isContentReleased(), false);
    BOOST_CHECK_EQUAL(4u, dq7->getMessageCount()); // over limit
    BOOST_CHECK_EQUAL(4u, dq8->getMessageCount()); // over limit
    BOOST_CHECK_EQUAL(4u, tq9->getMessageCount());

    intrusive_ptr<Message> msg24 = mkMsg(testStore, "", true);  // durable no content
    DeliverableMessage dmsg24(msg24);
    mbmFanout4.route(dmsg24, "", 0);
    msg24->tryReleaseContent();
    BOOST_CHECK_EQUAL(msg24->isContentReleased(), false);
    BOOST_CHECK_EQUAL(5u, dq7->getMessageCount()); // over limit
    BOOST_CHECK_EQUAL(5u, dq8->getMessageCount()); // over limit
    BOOST_CHECK_EQUAL(5u, tq9->getMessageCount());
}


QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
