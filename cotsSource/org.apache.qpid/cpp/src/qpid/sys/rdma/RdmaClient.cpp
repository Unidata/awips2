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
#include "qpid/sys/rdma/RdmaIO.h"
#include "qpid/sys/Time.h"

#include <netdb.h>
#include <arpa/inet.h>

#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
#include <cmath>
#include <boost/bind.hpp>

using std::vector;
using std::string;
using std::cout;
using std::cerr;
using std::copy;
using std::rand;

using qpid::sys::Poller;
using qpid::sys::Dispatcher;
using qpid::sys::SocketAddress;
using qpid::sys::AbsTime;
using qpid::sys::Duration;
using qpid::sys::TIME_SEC;
using qpid::sys::TIME_INFINITE;

namespace qpid {
namespace tests {

// count of messages
int64_t smsgs = 0;
int64_t sbytes = 0;
int64_t rmsgs = 0;
int64_t rbytes = 0;

int target = 1000000;
int msgsize = 200;
AbsTime startTime;
Duration sendingDuration(TIME_INFINITE);
Duration fullTestDuration(TIME_INFINITE);

vector<char> testString;

void write(Rdma::AsynchIO& aio) {
    while (aio.writable()) {
        if (smsgs >= target)
            return;
        Rdma::Buffer* b = aio.getBuffer();
        std::copy(testString.begin(), testString.end(), b->bytes);
        b->dataCount = msgsize;
        aio.queueWrite(b);
        ++smsgs;
        sbytes += msgsize;
    }
}

void dataError(Rdma::AsynchIO&) {
    cout << "Data error:\n";
}

void data(Poller::shared_ptr p, Rdma::AsynchIO& aio, Rdma::Buffer* b) {
    ++rmsgs;
    rbytes += b->dataCount;

    // When all messages have been recvd stop
    if (rmsgs < target) {
        write(aio);
    } else {
        fullTestDuration = std::min(fullTestDuration, Duration(startTime, AbsTime::now()));
        if (aio.incompletedWrites() == 0)
            p->shutdown();
    }
}

void full(Rdma::AsynchIO& a, Rdma::Buffer* b) {
    // Warn as we shouldn't get here anymore
    cerr << "!";

    // Don't need to keep buffer just adjust the counts
    --smsgs;
    sbytes -= b->dataCount;

    // Give buffer back
    a.returnBuffer(b);
}

void idle(Poller::shared_ptr p, Rdma::AsynchIO& aio) {
    if (smsgs < target) {
        write(aio);
    } else {
        sendingDuration = std::min(sendingDuration, Duration(startTime, AbsTime::now()));
        if (rmsgs >= target && aio.incompletedWrites() == 0)
            p->shutdown();
    }
}

void connected(Poller::shared_ptr poller, Rdma::Connection::intrusive_ptr& ci, const Rdma::ConnectionParams& cp) {
    cout << "Connected\n";
    Rdma::QueuePair::intrusive_ptr q = ci->getQueuePair();

    Rdma::AsynchIO* aio = new Rdma::AsynchIO(ci->getQueuePair(),
        cp.maxRecvBufferSize, cp.initialXmitCredit , Rdma::DEFAULT_WR_ENTRIES,
        boost::bind(&data, poller, _1, _2),
        boost::bind(&idle, poller, _1),
        &full,
        dataError);

    startTime = AbsTime::now();
    write(*aio);

    aio->start(poller);
}

void disconnected(boost::shared_ptr<Poller> p, Rdma::Connection::intrusive_ptr&) {
    cout << "Disconnected\n";
    p->shutdown();
}

void connectionError(boost::shared_ptr<Poller> p, Rdma::Connection::intrusive_ptr&, const Rdma::ErrorType) {
    cout << "Connection error\n";
    p->shutdown();
}

void rejected(boost::shared_ptr<Poller> p, Rdma::Connection::intrusive_ptr&, const Rdma::ConnectionParams&) {
    cout << "Connection rejected\n";
    p->shutdown();
}

}} // namespace qpid::tests

using namespace qpid::tests;

int main(int argc, char* argv[]) {
    vector<string> args(&argv[0], &argv[argc]);

    string host = args[1];
    string port = (args.size() < 3) ? "20079" : args[2];

    if (args.size() > 3)
        msgsize = atoi(args[3].c_str());
    cout << "Message size: " << msgsize << "\n";

    // Make a random message of that size
    testString.resize(msgsize);
    for (int i = 0; i < msgsize; ++i) {
        testString[i] = 32 + (rand() & 0x3f);
    }

    try {
        boost::shared_ptr<Poller> p(new Poller());
        Dispatcher d(p);

        SocketAddress sa(host, port);
        cout << "Connecting to: " << sa.asString() <<"\n";
        Rdma::Connector c(
            sa,
            Rdma::ConnectionParams(msgsize, Rdma::DEFAULT_WR_ENTRIES),
            boost::bind(&connected, p, _1, _2),
            boost::bind(&connectionError, p, _1, _2),
            boost::bind(&disconnected, p, _1),
            boost::bind(&rejected, p, _1, _2));

        c.start(p);
        d.run();
    } catch (Rdma::Exception& e) {
        int err = e.getError();
        cerr << "Error: " << e.what() << "(" << err << ")\n";
    }

    cout
        << "Sent: " << smsgs
        << "msgs (" << sbytes
        << "bytes) in: " << double(sendingDuration)/TIME_SEC
        << "s: " << double(smsgs)*TIME_SEC/sendingDuration
        << "msgs/s(" << double(sbytes)*TIME_SEC/sendingDuration
        << "bytes/s)\n";
    cout
        << "Recd: " << rmsgs
        << "msgs (" << rbytes
        << "bytes) in: " << double(fullTestDuration)/TIME_SEC
        << "s: " << double(rmsgs)*TIME_SEC/fullTestDuration
        << "msgs/s(" << double(rbytes)*TIME_SEC/fullTestDuration
        << "bytes/s)\n";

}
