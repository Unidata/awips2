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
#include "qpid/client/AsyncSession.h"
#include "qpid/client/SubscriptionManager.h"

using namespace qpid;
using namespace qpid::client;
using namespace qpid::sys;
using namespace std;

namespace qpid {
namespace tests {

typedef vector<string> StringSet;

struct Args : public qpid::TestOptions {
    uint size;
    uint count;
    bool durable;
    string destination;
    string routingKey;
    bool summary;
    bool id;

    Args() : size(256), count(1000), durable(true), routingKey("publish-consume"), summary(false), id(false) {
        addOptions()
            ("size", optValue(size, "N"), "message size")
            ("count", optValue(count, "N"), "number of messages to publish")
            ("durable", optValue(durable, "yes|no"), "use durable messages")
            ("destination", optValue(destination, "<exchange name>"), "destination to publish to")
            ("routing-key", optValue(routingKey, "<key>"), "routing key to publish with")
            ("summary,s", optValue(summary), "Output only the rate.")
            ("id", optValue(id), "Add unique correlation ID");
    }
};

Args opts;

struct Client
{
    Connection connection;
    AsyncSession session;

    Client()
    {
        opts.open(connection);
        session = connection.newSession();
    }

    // Cheap hex calculation, avoid expensive ostrstream and string
    // creation to generate correlation ids in message loop.
    char hex(char i) { return i<10 ? '0'+i : 'A'+i-10; }
    void hex(char i, string& s) {
        s[0]=hex(i>>24); s[1]=hex(i>>16); s[2]=hex(i>>8); s[3]=i;
    }

    void publish()
    {
        AbsTime begin=now();
        Message msg(string(opts.size, 'X'), opts.routingKey);
        string correlationId = "0000";
        if (opts.durable)
            msg.getDeliveryProperties().setDeliveryMode(framing::PERSISTENT);

        for (uint i = 0; i < opts.count; i++) {
            if (opts.id) {
                hex(i+1, correlationId);
                msg.getMessageProperties().setCorrelationId(correlationId);
            }
            session.messageTransfer(arg::destination=opts.destination,
                                    arg::content=msg,
                                    arg::acceptMode=1);
        }
        session.sync();
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
        client.publish();
        return 0;
    } catch(const exception& e) {
	cout << e.what() << endl;
    }
    return 1;
}
