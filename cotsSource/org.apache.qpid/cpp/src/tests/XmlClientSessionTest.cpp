/*
 *
 * Licensed to  the Apachef Software Foundation (ASF) under one
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
#include "qpid/sys/Shlib.h"
#include "qpid/sys/Monitor.h"
#include "qpid/sys/Thread.h"
#include "qpid/sys/Runnable.h"
#include "qpid/client/Message.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/client/Connection.h"
#include "qpid/client/SubscriptionManager.h"
#include "qpid/client/LocalQueue.h"
#include "qpid/client/Session.h"
#include "qpid/client/SubscriptionManager.h"

#include <boost/optional.hpp>
#include <boost/lexical_cast.hpp>

#include <vector>

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(XmlClientSessionTest)

using namespace qpid::client;

using namespace qpid::client::arg;
using namespace qpid::framing;
using namespace qpid;
using qpid::sys::Shlib;
using qpid::sys::Monitor;
using std::string;
using std::cout;
using std::endl;


Shlib shlib(getLibPath("XML_LIB"));

class SubscribedLocalQueue : public LocalQueue {
  private:
    SubscriptionManager& subscriptions;
  public:
    SubscribedLocalQueue(SubscriptionManager& subs) : subscriptions(subs) {}
    Message get () { return pop(); }
    virtual ~SubscribedLocalQueue() {}
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
    void declareSubscribe(const string& q="odd_blue",
                          const string& dest="xml")
    {
        session.queueDeclare(queue=q);
        session.messageSubscribe(queue=q, destination=dest, acquireMode=1);
        session.messageFlow(destination=dest, unit=0, value=0xFFFFFFFF);//messages
        session.messageFlow(destination=dest, unit=1, value=0xFFFFFFFF);//bytes
    }
};

// ########### START HERE ####################################



QPID_AUTO_TEST_CASE(testXmlBinding) {
    ClientSessionFixture f;

    SubscriptionManager subscriptions(f.session);
    SubscribedLocalQueue localQueue(subscriptions);

    f.session.exchangeDeclare(qpid::client::arg::exchange="xml", qpid::client::arg::type="xml");
    f.session.queueDeclare(qpid::client::arg::queue="odd_blue");
    subscriptions.subscribe(localQueue, "odd_blue");

    FieldTable binding;
    binding.setString("xquery", "declare variable $color external;"
                      "(./message/id mod 2 = 1) and ($color = 'blue')");
    f.session.exchangeBind(qpid::client::arg::exchange="xml", qpid::client::arg::queue="odd_blue", qpid::client::arg::bindingKey="query_name", qpid::client::arg::arguments=binding);

    Message message;
    message.getDeliveryProperties().setRoutingKey("query_name");

    message.getHeaders().setString("color", "blue");
    string m = "<message><id>1</id></message>";
    message.setData(m);

    f.session.messageTransfer(qpid::client::arg::content=message,  qpid::client::arg::destination="xml");

    Message m2 = localQueue.get();
    BOOST_CHECK_EQUAL(m, m2.getData());
}

/**
 * Ensure that multiple queues can be bound using the same routing key
 */
QPID_AUTO_TEST_CASE(testXMLBindMultipleQueues) {
    ClientSessionFixture f;


    f.session.exchangeDeclare(arg::exchange="xml", arg::type="xml");
    f.session.queueDeclare(arg::queue="blue", arg::exclusive=true, arg::autoDelete=true);
    f.session.queueDeclare(arg::queue="red", arg::exclusive=true, arg::autoDelete=true);

    FieldTable blue;
    blue.setString("xquery", "./colour = 'blue'");
    f.session.exchangeBind(arg::exchange="xml", arg::queue="blue", arg::bindingKey="by-colour", arg::arguments=blue);
    FieldTable red;
    red.setString("xquery", "./colour = 'red'");
    f.session.exchangeBind(arg::exchange="xml", arg::queue="red", arg::bindingKey="by-colour", arg::arguments=red);

    Message sent1("<colour>blue</colour>", "by-colour");
    f.session.messageTransfer(arg::content=sent1,  arg::destination="xml");

    Message sent2("<colour>red</colour>", "by-colour");
    f.session.messageTransfer(arg::content=sent2,  arg::destination="xml");

    Message received;
    BOOST_CHECK(f.subs.get(received, "blue"));
    BOOST_CHECK_EQUAL(sent1.getData(), received.getData());
    BOOST_CHECK(f.subs.get(received, "red"));
    BOOST_CHECK_EQUAL(sent2.getData(), received.getData());
}

//### Test: Bad XML does not kill the server - and does not even
// raise an exception, the content is not required to be XML.

QPID_AUTO_TEST_CASE(testXMLSendBadXML) {
    ClientSessionFixture f;

    f.session.exchangeDeclare(arg::exchange="xml", arg::type="xml");
    f.session.queueDeclare(arg::queue="blue", arg::exclusive=true, arg::autoDelete=true)\
        ;
    f.session.queueDeclare(arg::queue="red", arg::exclusive=true, arg::autoDelete=true);

    FieldTable blue;
    blue.setString("xquery", "./colour = 'blue'");
    f.session.exchangeBind(arg::exchange="xml", arg::queue="blue", arg::bindingKey="by-c\
olour", arg::arguments=blue);
    FieldTable red;
    red.setString("xquery", "./colour = 'red'");
    f.session.exchangeBind(arg::exchange="xml", arg::queue="red", arg::bindingKey="by-co\
lour", arg::arguments=red);

    Message sent1("<>colour>blue</colour>", "by-colour");
    f.session.messageTransfer(arg::content=sent1,  arg::destination="xml");

    BOOST_CHECK_EQUAL(1, 1);
}


//### Test: Bad XQuery does not kill the server, but does raise an exception

QPID_AUTO_TEST_CASE(testXMLBadXQuery) {
    ClientSessionFixture f;

    f.session.exchangeDeclare(arg::exchange="xml", arg::type="xml");
    f.session.queueDeclare(arg::queue="blue", arg::exclusive=true, arg::autoDelete=true)\
        ;

    try {
        ScopedSuppressLogging sl; // Supress logging of error messages for expected error.
        FieldTable blue;
        blue.setString("xquery", "./colour $=! 'blue'");
        f.session.exchangeBind(arg::exchange="xml", arg::queue="blue", arg::bindingKey="by-c\
olour", arg::arguments=blue);
    }
    catch (const InternalErrorException& e) {
        return;
    }
    BOOST_ERROR("A bad XQuery must raise an exception when used in an XML Binding.");

}


//### Test: Each session can provide its own definition for a query name



//### Test: Bindings persist, surviving broker restart

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
