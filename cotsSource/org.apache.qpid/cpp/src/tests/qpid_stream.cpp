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

#include <qpid/messaging/Connection.h>
#include <qpid/messaging/Message.h>
#include <qpid/messaging/Receiver.h>
#include <qpid/messaging/Sender.h>
#include <qpid/messaging/Session.h>
#include <qpid/sys/Runnable.h>
#include <qpid/sys/Thread.h>
#include <qpid/sys/Time.h>
#include <qpid/Options.h>
#include <iostream>
#include <string>

using namespace qpid::messaging;
using namespace qpid::sys;

namespace qpid {
namespace tests {

struct Args : public qpid::Options 
{
    std::string url;
    std::string address;
    uint rate;
    bool durable;

    Args() : url("amqp:tcp:127.0.0.1:5672"), address("test-queue"), rate(1000), durable(false)
    {
        addOptions()
            ("url", qpid::optValue(url, "URL"), "Url to connect to.")
            ("address", qpid::optValue(address, "ADDRESS"), "Address to stream messages through.")
            ("rate", qpid::optValue(rate, "msgs/sec"), "Rate at which to stream messages.")
            ("durable", qpid::optValue(durable, "true|false"), "Mark messages as durable.");
    }
};

Args opts;

const std::string TIMESTAMP = "ts";

uint64_t timestamp(const AbsTime& time)
{
    Duration t(time);
    return t;
}

struct Client : Runnable
{
    virtual ~Client() {}
    virtual void doWork(Session&) = 0;

    void run()
    {
        try {
            Connection connection = Connection::open(opts.url);
            Session session = connection.newSession();
            doWork(session);
            session.close();
            connection.close();
        } catch(const std::exception& error) {
            std::cout << error.what() << std::endl;
        }
    }

    Thread thread;

    void start() { thread = Thread(this); }
    void join() { thread.join(); }
};

struct Publish : Client
{
    void doWork(Session& session)
    {
        Sender sender = session.createSender(opts.address);
        Message msg;
        uint64_t interval = TIME_SEC / opts.rate;
        uint64_t sent = 0, missedRate = 0;
        AbsTime start = now();
        while (true) {
            AbsTime sentAt = now();
            msg.getHeaders()[TIMESTAMP] = timestamp(sentAt);
            sender.send(msg);
            ++sent;
            AbsTime waitTill(start, sent*interval);
            Duration delay(sentAt, waitTill);
            if (delay < 0) {
                ++missedRate;
            } else {
                qpid::sys::usleep(delay / TIME_USEC);
            }
        }
    }
};

struct Consume : Client
{
    void doWork(Session& session)
    {
        Message msg;
        uint64_t received = 0;
        double minLatency = std::numeric_limits<double>::max();
        double maxLatency = 0;
        double totalLatency = 0;
        Receiver receiver = session.createReceiver(opts.address);
        while (receiver.fetch(msg)) {
            session.acknowledge();//TODO: add batching option
            ++received;
            //calculate latency
            uint64_t receivedAt = timestamp(now());
            uint64_t sentAt = msg.getHeaders()[TIMESTAMP].asUint64();
            double latency = ((double) (receivedAt - sentAt)) / TIME_MSEC;

            //update avg, min & max
            minLatency = std::min(minLatency, latency);
            maxLatency = std::max(maxLatency, latency);
            totalLatency += latency;

            if (received % opts.rate == 0) {
                std::cout << "count=" << received 
                          << ", avg=" << (totalLatency/received) 
                          << ", min=" << minLatency 
                          << ", max=" << maxLatency << std::endl;
            }
        }
    }
};

}} // namespace qpid::tests

using namespace qpid::tests;

int main(int argc, char** argv)
{
    try {
        opts.parse(argc, argv);
        Publish publish;
        Consume consume;
        publish.start();
        consume.start();
        consume.join();
        publish.join();
        return 0;
    } catch(const std::exception& error) {
        std::cout << error.what() << std::endl;
    }
    return 1;
}


