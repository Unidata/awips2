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
#include <limits>
#include <iostream>
#include <memory>
#include <sstream>
#include <vector>

#include "TestOptions.h"
#include "qpid/sys/Thread.h"
#include "qpid/client/Connection.h"
#include "qpid/client/Message.h"
#include "qpid/client/AsyncSession.h"
#include "qpid/client/SubscriptionManager.h"
#include "qpid/sys/Time.h"

using namespace qpid;
using namespace qpid::client;
using namespace qpid::sys;
using std::string;

namespace qpid {
namespace tests {

typedef std::vector<std::string> StringSet;

struct Args : public qpid::TestOptions {
    uint size;
    uint count;
    uint rate;
    bool sync;
    uint reportFrequency;
    uint timeLimit;
    uint concurrentConnections;
    uint prefetch;
    uint ack;
    bool cumulative;
    bool csv;
    bool durable;
    string base;
    bool singleConnect;

    Args() : size(256), count(1000), rate(0), reportFrequency(1000),
	     timeLimit(0), concurrentConnections(1),
             prefetch(100), ack(0),
             durable(false), base("latency-test"), singleConnect(false)

    {
        addOptions()

            ("size", optValue(size, "N"), "message size")
            ("concurrentTests", optValue(concurrentConnections, "N"), "number of concurrent test setups, will create another publisher,\
 subcriber, queue, and connections")
            ("single-connection", optValue(singleConnect, "yes|no"), "Use one connection for multiple sessions.")
            ("count", optValue(count, "N"), "number of messages to send")
            ("rate", optValue(rate, "N"), "target message rate (causes count to be ignored)")
            ("sync", optValue(sync), "send messages synchronously")
            ("report-frequency", optValue(reportFrequency, "N"),
             "number of milliseconds to wait between reports (ignored unless rate specified)")
            ("time-limit", optValue(timeLimit, "N"),
             "test duration, in seconds")
            ("prefetch", optValue(prefetch, "N"), "prefetch count (0 implies no flow control, and no acking)")
            ("ack", optValue(ack, "N"), "Ack frequency in messages (defaults to half the prefetch value)")
            ("durable", optValue(durable, "yes|no"), "use durable messages")
            ("csv", optValue(csv), "print stats in csv format (rate,min,max,avg)")
            ("cumulative", optValue(cumulative), "cumulative stats in csv format")
            ("queue-base-name", optValue(base, "<name>"), "base name for queues");
    }
};

const std::string chars("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");

Args opts;
double c_min, c_avg, c_max;
Connection globalConnection;

uint64_t current_time()
{
    Duration t(now());
    return t;
}

struct Stats
{
    Mutex lock;
    uint count;
    double minLatency;
    double maxLatency;
    double totalLatency;

    Stats();
    void update(double l);
    void print();
    void reset();
};

class Client : public Runnable
{
protected:
    Connection* connection;
    Connection localConnection;
    AsyncSession session;
    Thread thread;
    string queue;

public:
    Client(const string& q);
    virtual ~Client();

    void start();
    void join();
    void run();
    virtual void test() = 0;
};

class Receiver : public Client, public MessageListener
{
    SubscriptionManager mgr;
    uint count;
    Stats& stats;

public:
    Receiver(const string& queue, Stats& stats);
    void test();
    void received(Message& msg);
    Stats getStats();
    uint getCount() { return count; }
    void stop() {  mgr.stop(); mgr.cancel(queue); }
};


class Sender : public Client
{
    string generateData(uint size);
    void sendByRate();
    void sendByCount();
    Receiver& receiver;
    const string data;

public:
    Sender(const string& queue, Receiver& receiver);
    void test();
};


class Test
{
    const string queue;
    Stats stats;
    Receiver receiver;
    Sender sender;
    AbsTime begin;

public:
    Test(const string& q) : queue(q), receiver(queue, stats), sender(queue, receiver), begin(now()) {}
    void start();
    void join();
    void report();
};


Client::Client(const string& q) : queue(q)
{
    if (opts.singleConnect){
        connection = &globalConnection;
        if (!globalConnection.isOpen()) opts.open(globalConnection);
    }else{
        connection = &localConnection;
        opts.open(localConnection);
    }
    session = connection->newSession();
}

void Client::start()
{
    thread = Thread(this);
}

void Client::join()
{
    thread.join();
}

void Client::run()
{
    try{
        test();
    } catch(const std::exception& e) {
        std::cout << "Error in receiver: " << e.what() << std::endl;
    }
}

Client::~Client()
{
    try{
        session.close();
        connection->close();
    } catch(const std::exception& e) {
        std::cout << "Error in receiver: " << e.what() << std::endl;
    }
}

Receiver::Receiver(const string& q, Stats& s) : Client(q), mgr(session), count(0), stats(s)
{
    session.queueDeclare(arg::queue=queue, arg::durable=opts.durable, arg::autoDelete=true);
    uint msgCount = session.queueQuery(arg::queue=queue).get().getMessageCount();
    if (msgCount) {
        std::cout << "Warning: found " << msgCount << " msgs on " << queue << ". Purging..." << std::endl;
        session.queuePurge(arg::queue=queue);
        session.sync();
    }
    SubscriptionSettings settings;
    if (opts.prefetch) {
        settings.autoAck = (opts.ack ? opts.ack : (opts.prefetch / 2));
        settings.flowControl = FlowControl::messageWindow(opts.prefetch);
    } else {
        settings.acceptMode = ACCEPT_MODE_NONE;
        settings.flowControl = FlowControl::unlimited();
    }
    mgr.subscribe(*this, queue, settings);
}

void Receiver::test()
{
    mgr.run();
    mgr.cancel(queue);
}

void Receiver::received(Message& msg)
{
    ++count;
    uint64_t receivedAt = current_time();
    uint64_t sentAt = msg.getDeliveryProperties().getTimestamp();

    stats.update(((double) (receivedAt - sentAt)) / TIME_MSEC);

    if (!opts.rate && count >= opts.count) {
        mgr.stop();
    }
}

void Stats::update(double latency)
{
    Mutex::ScopedLock l(lock);
    count++;
    minLatency = std::min(minLatency, latency);
    maxLatency = std::max(maxLatency, latency);
    totalLatency += latency;
}

Stats::Stats() : count(0), minLatency(std::numeric_limits<double>::max()), maxLatency(0), totalLatency(0) {}

void Stats::print()
{
    static bool already_have_stats = false;
    uint value;

    if (opts.rate)
        value = opts.rate;
    else
        value = opts.count;
    Mutex::ScopedLock l(lock);
    double aux_avg = (totalLatency / count);
    if (!opts.cumulative) {
        if (!opts.csv) {
            if (count) {
                std::cout << "Latency(ms): min=" << minLatency << ", max=" <<
	                 maxLatency << ", avg=" << aux_avg;
            } else {
                std::cout << "Stalled: no samples for interval";
            }
        } else {
            if (count) {
          	    std::cout << value << "," << minLatency << "," << maxLatency <<
    				     "," << aux_avg;
            } else {
          	    std::cout << value << "," << minLatency << "," << maxLatency <<
    				     ", Stalled";
            }
        }
    } else {
       if (count) {
            if (already_have_stats) {
                c_avg = (c_min + aux_avg) / 2;
                if (c_min > minLatency) c_min = minLatency;
                if (c_max < maxLatency) c_max = maxLatency;
            } else {
                c_avg = aux_avg;
                c_min = minLatency;
                c_max = maxLatency;
                already_have_stats = true;
            }
  	        std::cout << value << "," << c_min << "," << c_max <<
    				     "," << c_avg;
        } else {
            std::cout << "Stalled: no samples for interval";
        }
    }
}

void Stats::reset()
{
    Mutex::ScopedLock l(lock);
    count = 0;
    totalLatency = maxLatency = 0;
    minLatency = std::numeric_limits<double>::max();
}

Sender::Sender(const string& q, Receiver& receiver) : Client(q), receiver(receiver), data(generateData(opts.size)) {}

void Sender::test()
{
    if (opts.rate) sendByRate();
    else sendByCount();
}

void Sender::sendByCount()
{
    Message msg(data, queue);
    if (opts.durable) {
        msg.getDeliveryProperties().setDeliveryMode(framing::PERSISTENT);
    }

    for (uint i = 0; i < opts.count; i++) {
        uint64_t sentAt(current_time());
        msg.getDeliveryProperties().setTimestamp(sentAt);
        async(session).messageTransfer(arg::content=msg, arg::acceptMode=1);
        if (opts.sync) session.sync();
    }
    session.sync();
}

void Sender::sendByRate()
{
    Message msg(data, queue);
    if (opts.durable) {
        msg.getDeliveryProperties().setDeliveryMode(framing::PERSISTENT);
    }
    uint64_t interval = TIME_SEC/opts.rate;
    int64_t timeLimit = opts.timeLimit * TIME_SEC;
    uint64_t sent = 0, missedRate = 0;
    AbsTime start = now();
    while (true) {
        AbsTime sentAt=now();
        msg.getDeliveryProperties().setTimestamp(Duration(sentAt));
        async(session).messageTransfer(arg::content=msg, arg::acceptMode=1);
        if (opts.sync) session.sync();
        ++sent;
        AbsTime waitTill(start, sent*interval);
        Duration delay(sentAt, waitTill);
        if (delay < 0)
            ++missedRate;
        else
            sys::usleep(delay / TIME_USEC);
        if (timeLimit != 0 && Duration(start, now()) > timeLimit) {
            session.sync();
            receiver.stop();
            break;
        }
    }
}

string Sender::generateData(uint size)
{
    if (size < chars.length()) {
        return chars.substr(0, size);
    }
    std::string data;
    for (uint i = 0; i < (size / chars.length()); i++) {
        data += chars;
    }
    data += chars.substr(0, size % chars.length());
    return data;
}


void Test::start()
{
    receiver.start();
    begin = AbsTime(now());
    sender.start();
}

void Test::join()
{
    sender.join();
    receiver.join();
    AbsTime end = now();
    Duration time(begin, end);
    double msecs(time / TIME_MSEC);
    if (!opts.csv) {
        std::cout << "Sent " << receiver.getCount() << " msgs through " << queue
                  << " in " << msecs << "ms (" << (receiver.getCount() * 1000 / msecs) << " msgs/s) ";
    }
    stats.print();
    std::cout << std::endl;
}

void Test::report()
{
    stats.print();
    std::cout << std::endl;
    stats.reset();
}

}} // namespace qpid::tests

using namespace qpid::tests;

int main(int argc, char** argv)
{
    try {
        opts.parse(argc, argv);
        if (opts.cumulative)
            opts.csv = true;

        Connection localConnection;
        AsyncSession session;

        boost::ptr_vector<Test> tests(opts.concurrentConnections);
        for (uint i = 0; i < opts.concurrentConnections; i++) {
            std::ostringstream out;
            out << opts.base << "-" << (i+1);
            tests.push_back(new Test(out.str()));
        }
        for (boost::ptr_vector<Test>::iterator i = tests.begin(); i != tests.end(); i++) {
            i->start();
        }
        if (opts.rate && !opts.timeLimit) {
            while (true) {
                qpid::sys::usleep(opts.reportFrequency * 1000);
                //print latency report:
                for (boost::ptr_vector<Test>::iterator i = tests.begin(); i != tests.end(); i++) {
                    i->report();
                }
            }
        } else {
            for (boost::ptr_vector<Test>::iterator i = tests.begin(); i != tests.end(); i++) {
                i->join();
            }
        }

        return 0;
    } catch(const std::exception& e) {
	std::cout << e.what() << std::endl;
    }
    return 1;
}
