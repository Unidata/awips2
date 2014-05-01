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

#include <algorithm>
#include <iostream>
#include <memory>
#include <sstream>
#include <vector>

#include "TestOptions.h"
#include "qpid/client/Connection.h"
#include "qpid/client/Message.h"
#include "qpid/client/Session.h"
#include "qpid/client/SubscriptionManager.h"

using namespace qpid;
using namespace qpid::client;
using namespace qpid::sys;
using namespace std;

namespace qpid {
namespace tests {

typedef vector<string> StringSet;

struct Args : public qpid::TestOptions {
    uint count;
    uint ack;
    string queue;
    bool declare;
    bool summary;
    bool print;
    bool durable;

    Args() : count(1000), ack(0), queue("publish-consume"),
             declare(false), summary(false), print(false)
    {
        addOptions()
            ("count", optValue(count, "N"), "number of messages to publish")
            ("ack-frequency", optValue(ack, "N"), "ack every N messages (0 means use no-ack mode)")
            ("queue", optValue(queue, "<queue name>"), "queue to consume from")
            ("declare", optValue(declare), "declare the queue")
            ("durable", optValue(durable), "declare the queue durable, use with declare")
            ("print-data", optValue(print), "Print the recieved data at info level")
            ("s,summary", optValue(summary), "Print undecorated rate.");
    }
};

Args opts;

struct Client
{
    Connection connection;
    Session session;

    Client()
    {
        opts.open(connection);
        session = connection.newSession();
    }

    void consume()
    {
        if (opts.declare)
            session.queueDeclare(arg::queue=opts.queue, arg::durable=opts.durable);
        SubscriptionManager subs(session);
        LocalQueue lq;
        SubscriptionSettings settings;
        settings.acceptMode = opts.ack > 0 ? ACCEPT_MODE_EXPLICIT : ACCEPT_MODE_NONE;
        settings.flowControl = FlowControl(opts.count, SubscriptionManager::UNLIMITED,false);
        Subscription sub = subs.subscribe(lq, opts.queue, settings);
        Message msg;
        AbsTime begin=now();
        for (size_t i = 0; i < opts.count; ++i) {
            msg=lq.pop();
            QPID_LOG(info, "Received: " << msg.getMessageProperties().getCorrelationId());
            if (opts.print) QPID_LOG(info, "Data: " << msg.getData());
        }
        if (opts.ack != 0)
            sub.accept(sub.getUnaccepted()); // Cumulative ack for final batch.
        AbsTime end=now();
        double secs(double(Duration(begin,end))/TIME_SEC);
        if (opts.summary) cout << opts.count/secs << endl;
        else cout << "Time: " << secs << "s Rate: " << opts.count/secs << endl;
    }

    ~Client()
    {
        try{
            session.close();
            connection.close();
        } catch(const exception& e) {
            cout << e.what() << endl;
        }
    }
};

}} // namespace qpid::tests

using namespace qpid::tests;

int main(int argc, char** argv)
{
    try {
        opts.parse(argc, argv);
        Client client;
        client.consume();
        return 0;
    } catch(const exception& e) {
	cout << e.what() << endl;
    }
    return 1;
}
