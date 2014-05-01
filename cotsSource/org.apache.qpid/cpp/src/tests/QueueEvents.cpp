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
#include "BrokerFixture.h"
#include "qpid/broker/Message.h"
#include "qpid/broker/Queue.h"
#include "qpid/broker/QueueEvents.h"
#include "qpid/client/QueueOptions.h"
#include "qpid/framing/SequenceNumber.h"
#include "qpid/sys/Dispatcher.h"
#include <boost/bind.hpp>
#include <boost/format.hpp>

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(QueueEventsSuite)

using namespace qpid::client;
using namespace qpid::broker;
using namespace qpid::sys;
using qpid::framing::SequenceNumber;

struct EventChecker
{
    typedef std::deque<QueueEvents::Event> Events;

    Events events;
    boost::shared_ptr<Poller> poller;

    void handle(QueueEvents::Event e)
    {
        if (events.empty()) {
            BOOST_FAIL("Unexpected event received");
        } else {
            BOOST_CHECK_EQUAL(events.front().type, e.type);
            BOOST_CHECK_EQUAL(events.front().msg.queue, e.msg.queue);
            BOOST_CHECK_EQUAL(events.front().msg.payload, e.msg.payload);
            BOOST_CHECK_EQUAL(events.front().msg.position, e.msg.position);
            events.pop_front();
        }
        if (events.empty() && poller) poller->shutdown();
    }

    void expect(QueueEvents::Event e)
    {
        events.push_back(e);
    }
};

QPID_AUTO_TEST_CASE(testBasicEventProcessing)
{
    boost::shared_ptr<Poller> poller(new Poller());
    sys::Dispatcher dispatcher(poller);
    Thread dispatchThread(dispatcher);
    QueueEvents events(poller);
    EventChecker listener;
    listener.poller = poller;
    events.registerListener("dummy", boost::bind(&EventChecker::handle, &listener, _1));
    //signal occurence of some events:
    Queue queue("queue1");
    SequenceNumber id;
    QueuedMessage event1(&queue, MessageUtils::createMessage(), id);
    QueuedMessage event2(&queue, MessageUtils::createMessage(), ++id);

    //define events expected by listener:
    listener.expect(QueueEvents::Event(QueueEvents::ENQUEUE, event1));
    listener.expect(QueueEvents::Event(QueueEvents::ENQUEUE, event2));
    listener.expect(QueueEvents::Event(QueueEvents::DEQUEUE, event1));

    events.enqueued(event1);
    events.enqueued(event2);
    events.dequeued(event1);

    dispatchThread.join();
    events.shutdown();
    events.unregisterListener("dummy");
}


struct EventRecorder
{
    struct EventRecord
    {
        QueueEvents::EventType type;
        std::string queue;
        std::string content;
        SequenceNumber position;
    };

    typedef std::deque<EventRecord> Events;

    Events events;

    void handle(QueueEvents::Event event)
    {
        EventRecord record;
        record.type = event.type;
        record.queue = event.msg.queue->getName();
        event.msg.payload->getFrames().getContent(record.content);
        record.position = event.msg.position;
        events.push_back(record);
    }

    void check(QueueEvents::EventType type, const std::string& queue, const std::string& content, const SequenceNumber& position)
    {
        if (events.empty()) {
            BOOST_FAIL("Missed event");
        } else {
            BOOST_CHECK_EQUAL(events.front().type, type);
            BOOST_CHECK_EQUAL(events.front().queue, queue);
            BOOST_CHECK_EQUAL(events.front().content, content);
            BOOST_CHECK_EQUAL(events.front().position, position);
            events.pop_front();
        }
    }
    void checkEnqueue(const std::string& queue, const std::string& data, const SequenceNumber& position)
    {
        check(QueueEvents::ENQUEUE, queue, data, position);
    }

    void checkDequeue(const std::string& queue, const std::string& data, const SequenceNumber& position)
    {
        check(QueueEvents::DEQUEUE, queue, data, position);
    }
};

QPID_AUTO_TEST_CASE(testSystemLevelEventProcessing)
{
    ProxySessionFixture fixture;
    //register dummy event listener to broker
    EventRecorder listener;
    fixture.broker->getQueueEvents().registerListener("recorder", boost::bind(&EventRecorder::handle, &listener, _1));

    //declare queue with event options specified
    QueueOptions options;
    options.enableQueueEvents(false);
    std::string q("queue-events-test");
    fixture.session.queueDeclare(arg::queue=q, arg::arguments=options);
    //send and consume some messages
    LocalQueue incoming;
    Subscription sub = fixture.subs.subscribe(incoming, q);
    for (int i = 0; i < 5; i++) {
        fixture.session.messageTransfer(arg::content=client::Message((boost::format("%1%_%2%") % "Message" % (i+1)).str(), q));
    }
    for (int i = 0; i < 3; i++) {
        BOOST_CHECK_EQUAL(incoming.pop().getData(), (boost::format("%1%_%2%") % "Message" % (i+1)).str());
    }
    for (int i = 5; i < 10; i++) {
        fixture.session.messageTransfer(arg::content=client::Message((boost::format("%1%_%2%") % "Message" % (i+1)).str(), q));
    }
    for (int i = 3; i < 10; i++) {
        BOOST_CHECK_EQUAL(incoming.pop().getData(), (boost::format("%1%_%2%") % "Message" % (i+1)).str());
    }
    fixture.connection.close();
    fixture.broker->getQueueEvents().shutdown();

    //check listener was notified of all events, and in correct order
    SequenceNumber enqueueId(1);
    SequenceNumber dequeueId(1);
    for (int i = 0; i < 5; i++) {
        listener.checkEnqueue(q, (boost::format("%1%_%2%") % "Message" % (i+1)).str(), enqueueId++);
    }
    for (int i = 0; i < 3; i++) {
        listener.checkDequeue(q, (boost::format("%1%_%2%") % "Message" % (i+1)).str(), dequeueId++);
    }
    for (int i = 5; i < 10; i++) {
        listener.checkEnqueue(q, (boost::format("%1%_%2%") % "Message" % (i+1)).str(), enqueueId++);
    }
    for (int i = 3; i < 10; i++) {
        listener.checkDequeue(q, (boost::format("%1%_%2%") % "Message" % (i+1)).str(), dequeueId++);
    }
}

QPID_AUTO_TEST_CASE(testSystemLevelEventProcessing_enqueuesOnly)
{
    ProxySessionFixture fixture;
    //register dummy event listener to broker
    EventRecorder listener;
    fixture.broker->getQueueEvents().registerListener("recorder", boost::bind(&EventRecorder::handle, &listener, _1));

    //declare queue with event options specified
    QueueOptions options;
    options.enableQueueEvents(true);
    std::string q("queue-events-test");
    fixture.session.queueDeclare(arg::queue=q, arg::arguments=options);
    //send and consume some messages
    LocalQueue incoming;
    Subscription sub = fixture.subs.subscribe(incoming, q);
    for (int i = 0; i < 5; i++) {
        fixture.session.messageTransfer(arg::content=client::Message((boost::format("%1%_%2%") % "Message" % (i+1)).str(), q));
    }
    for (int i = 0; i < 3; i++) {
        BOOST_CHECK_EQUAL(incoming.pop().getData(), (boost::format("%1%_%2%") % "Message" % (i+1)).str());
    }
    for (int i = 5; i < 10; i++) {
        fixture.session.messageTransfer(arg::content=client::Message((boost::format("%1%_%2%") % "Message" % (i+1)).str(), q));
    }
    for (int i = 3; i < 10; i++) {
        BOOST_CHECK_EQUAL(incoming.pop().getData(), (boost::format("%1%_%2%") % "Message" % (i+1)).str());
    }
    fixture.connection.close();
    fixture.broker->getQueueEvents().shutdown();

    //check listener was notified of all events, and in correct order
    SequenceNumber enqueueId(1);
    SequenceNumber dequeueId(1);
    for (int i = 0; i < 5; i++) {
        listener.checkEnqueue(q, (boost::format("%1%_%2%") % "Message" % (i+1)).str(), enqueueId++);
    }
    for (int i = 5; i < 10; i++) {
        listener.checkEnqueue(q, (boost::format("%1%_%2%") % "Message" % (i+1)).str(), enqueueId++);
    }
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
