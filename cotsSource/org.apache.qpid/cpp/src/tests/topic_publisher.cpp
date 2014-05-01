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

/**
 * This file provides one half of a test and example of a pub-sub
 * style of interaction. See topic_listener.cpp for the other half, in
 * which the logic for subscribers is defined.
 *
 * This file contains the publisher logic. The publisher will send a
 * number of messages to the exchange with the appropriate routing key
 * for the logical 'topic'. Once it has done this it will then send a
 * request that each subscriber report back with the number of message
 * it has received and the time that elapsed between receiving the
 * first one and receiving the report request. Once the expected
 * number of reports are received, it sends out a request that each
 * subscriber shutdown.
 */

#include "TestOptions.h"
#include "qpid/client/Connection.h"
#include "qpid/client/MessageListener.h"
#include "qpid/client/AsyncSession.h"
#include "qpid/client/SubscriptionManager.h"
#include "qpid/sys/Monitor.h"
#include "qpid/sys/Time.h"
#include <cstdlib>
#include <iostream>

using namespace qpid;
using namespace qpid::client;
using namespace qpid::sys;
using namespace std;

namespace qpid {
namespace tests {

/**
 * The publishing logic is defined in this class. It implements
 * message listener and can therfore be used to receive messages sent
 * back by the subscribers.
 */
class Publisher {
    AsyncSession session;
    SubscriptionManager mgr;
    LocalQueue queue;
    const string controlTopic;
    const bool transactional;
    const bool durable;

    string generateData(int size);

public:
    Publisher(const AsyncSession& session, const string& controlTopic, bool tx, bool durable);
    int64_t publish(int msgs, int listeners, int size);
    void terminate();
};

/**
 * A utility class for managing the options passed in to the test
 */
struct Args : public TestOptions {
    int messages;
    int subscribers;
    bool transactional;
    bool durable;
    int batches;
    int delay;
    int size;
    string statusqueue;

    Args() : messages(1000), subscribers(1),
             transactional(false), durable(false),
             batches(1), delay(0), size(256)
    {
        addOptions()
            ("messages", optValue(messages, "N"), "how many messages to send")
            ("subscribers", optValue(subscribers, "N"), "how many subscribers to expect reports from")
            ("transactional", optValue(transactional), "client should use transactions")
            ("durable", optValue(durable), "messages should be durable")
            ("batches", optValue(batches, "N"), "how many batches to run")
            ("delay", optValue(delay, "SECONDS"), "Causes a delay between each batch")
            ("size", optValue(size, "BYTES"), "size of the published messages")
            ("status-queue", optValue(statusqueue, "QUEUE-NAME"), "Message queue to read status messages from");
    }
};

Publisher::Publisher(const AsyncSession& _session, const string& _controlTopic, bool tx, bool d) :
    session(_session), mgr(session), controlTopic(_controlTopic), transactional(tx), durable(d)
{
    mgr.subscribe(queue, "response");
}

int64_t Publisher::publish(int msgs, int listeners, int size){
    Message msg(generateData(size), controlTopic);
    if (durable) {
        msg.getDeliveryProperties().setDeliveryMode(framing::PERSISTENT);
    }
    AbsTime start = now();

    for(int i = 0; i < msgs; i++){
        session.messageTransfer(arg::content=msg, arg::destination="amq.topic", arg::acceptMode=1);
    }
    //send report request
    Message reportRequest("", controlTopic);
    reportRequest.getHeaders().setString("TYPE", "REPORT_REQUEST");
    session.messageTransfer(arg::content=reportRequest, arg::destination="amq.topic", arg::acceptMode=1);
    if(transactional){
        sync(session).txCommit();
    }
    //wait for a response from each listener (TODO, could log these)
    for (int i = 0; i < listeners; i++) {
        Message report = queue.pop();
    }

    if(transactional){
        sync(session).txCommit();
    }

    AbsTime finish = now();
    return Duration(start, finish);
}

string Publisher::generateData(int size){
    string data;
    for(int i = 0; i < size; i++){
        data += ('A' + (i / 26));
    }
    return data;
}

void Publisher::terminate(){
    //send termination request
    Message terminationRequest("", controlTopic);
    terminationRequest.getHeaders().setString("TYPE", "TERMINATION_REQUEST");
    session.messageTransfer(arg::content=terminationRequest, arg::destination="amq.topic", arg::acceptMode=1);
    if(transactional){
        session.txCommit();
    }
}

}} // namespace qpid::tests

using namespace qpid::tests;

int main(int argc, char** argv) {
    try{
        Args args;
        args.parse(argc, argv);
        if(args.help)
            cout << args << endl;
        else {
            Connection connection;
            args.open(connection);
            AsyncSession session = connection.newSession();

            // If status-queue is defined, wait for all expected listeners to join in before we start
            if( args.statusqueue.length() > 0 ) {
                cout << "Waiting for " << args.subscribers << " listeners..." << endl;
                SubscriptionManager statusSubs(session);
                LocalQueue statusQ;
                statusSubs.subscribe(statusQ, args.statusqueue);
                for (int i = 0; i < args.subscribers; i++) {
                    Message m = statusQ.get();
                    if( m.getData().find("topic_listener: ", 0) == 0 ) {
                        cout << "Listener " << (i+1) << " of " << args.subscribers
                            << " is ready (pid " << m.getData().substr(16, m.getData().length() - 16)
                            << ")" << endl;
                    } else {
                        throw Exception(QPID_MSG("Unexpected message received on status queue: " << m.getData()));
                    }
                }
            }

            if (args.transactional) {
                session.txSelect();
            }
            session.queueDeclare(arg::queue="response");
            session.exchangeBind(arg::exchange="amq.direct", arg::queue="response", arg::bindingKey="response");

            Publisher publisher(session, "topic_control", args.transactional, args.durable);

            int batchSize(args.batches);
            int64_t max(0);
            int64_t min(0);
            int64_t sum(0);
            for(int i = 0; i < batchSize; i++){
                if(i > 0 && args.delay) qpid::sys::sleep(args.delay);
                int64_t msecs =
                    publisher.publish(args.messages,
                                      args.subscribers,
                                      args.size) / TIME_MSEC;
                if(!max || msecs > max) max = msecs;
                if(!min || msecs < min) min = msecs;
                sum += msecs;
                cout << "Completed " << (i+1) << " of " << batchSize
                    << " in " << msecs << "ms" << endl;
            }
            publisher.terminate();
            int64_t avg = sum / batchSize;
            if(batchSize > 1){
                cout << batchSize << " batches completed. avg=" << avg <<
                    ", max=" << max << ", min=" << min << endl;
            }
            session.close();
            connection.close();
        }
        return 0;
    }catch(exception& error) {
        cout << error.what() << endl;
    }
    return 1;
}
