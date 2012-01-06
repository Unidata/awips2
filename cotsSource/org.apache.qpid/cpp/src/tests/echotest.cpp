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

#include <qpid/client/Connection.h>
#include <qpid/client/SubscriptionManager.h>
#include <qpid/client/AsyncSession.h>
#include <qpid/client/Message.h>
#include <qpid/client/MessageListener.h>
#include <qpid/sys/Time.h>

#include <iostream>

using namespace qpid::client;
using namespace qpid::framing;
using namespace qpid::sys;
using namespace std;

namespace qpid {
namespace tests {

struct  Args : public qpid::Options,
               public qpid::client::ConnectionSettings
{
    bool help;
    uint count;
    uint size;
    bool summary;

    Args() : qpid::Options("Simple latency test optins"), help(false), count(20), size(0), summary()
    {
        using namespace qpid;
        addOptions()
            ("help", optValue(help), "Print this usage statement")
            ("count", optValue(count, "N"), "Number of messages to send")
            ("size", optValue(count, "N"), "Size of messages")
            ("broker,b", optValue(host, "HOST"), "Broker host to connect to")
            ("port,p", optValue(port, "PORT"), "Broker port to connect to")
            ("username", optValue(username, "USER"), "user name for broker log in.")
            ("password", optValue(password, "PASSWORD"), "password for broker log in.")
            ("mechanism", optValue(mechanism, "MECH"), "SASL mechanism to use when authenticating.")
            ("tcp-nodelay", optValue(tcpNoDelay), "Turn on tcp-nodelay")
            ("s,summary", optValue(summary), "Print only average latency.");
    }
};

uint64_t current_time()
{
    Duration t(now());
    return t;
}

class Listener : public MessageListener
{
  private:
    Session session;
    SubscriptionManager subscriptions;
    uint counter;
    const uint limit;
    std::string queue;
    Message request;
    double total, min, max;
    bool summary;

  public:
    Listener(Session& session, uint limit, bool summary);
    void start(uint size);
    void received(Message& message);
};

Listener::Listener(Session& s, uint l, bool summary_) :
    session(s), subscriptions(s), counter(0), limit(l),
    queue(session.getId().getName()), total(),
    min(std::numeric_limits<double>::max()), max(), summary(summary_)
{}

void Listener::start(uint size)
{
    session.queueDeclare(arg::queue=queue, arg::exclusive=true, arg::autoDelete=true);
    request.getDeliveryProperties().setRoutingKey(queue);
    subscriptions.subscribe(*this, queue, SubscriptionSettings(FlowControl::unlimited(), ACCEPT_MODE_NONE));

    request.getDeliveryProperties().setTimestamp(current_time());
    if (size) request.setData(std::string(size, 'X'));
    async(session).messageTransfer(arg::content=request);
    subscriptions.run();
}

void Listener::received(Message& response)
{
    //extract timestamp and compute latency:
    uint64_t sentAt = response.getDeliveryProperties().getTimestamp();
    uint64_t receivedAt = current_time();

    double latency = ((double) (receivedAt - sentAt)) / TIME_MSEC;
    if (!summary) cout << "Latency: " << latency << "ms" << endl;
    min = std::min(latency, min);
    max = std::max(latency, max);
    total += latency;

    if (++counter < limit) {
        request.getDeliveryProperties().setTimestamp(current_time());
        async(session).messageTransfer(arg::content=request);
    } else {
        subscriptions.cancel(queue);
        if (summary) cout << min << "\t" << max << "\t" << total/limit << endl;
        else cout << "min: " << min << " max: " <<  max << " average: " << total/limit << endl;
    }
}

}} // namespace qpid::tests

using namespace qpid::tests;

int main(int argc, char** argv)
{
    Args opts;
    opts.parse(argc, argv);

    if (opts.help) {
        std::cout << opts << std::endl;
        return 0;
    }

    Connection connection;
    try {
        connection.open(opts);
        Session session = connection.newSession();
        Listener listener(session, opts.count, opts.summary);
        listener.start(opts.size);

        connection.close();
        return 0;
    } catch(const std::exception& error) {
        std::cout << error.what() << std::endl;
    }
    return 1;
}


