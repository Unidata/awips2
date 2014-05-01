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

#include <arpa/inet.h>

#include <vector>
#include <queue>
#include <string>
#include <iostream>

#include <boost/bind.hpp>

using std::vector;
using std::queue;
using std::string;
using std::cout;
using std::cerr;

using qpid::sys::SocketAddress;
using qpid::sys::Poller;
using qpid::sys::Dispatcher;

// All the accepted connections
namespace qpid {
namespace tests {

struct ConRec {
    Rdma::Connection::intrusive_ptr connection;
    Rdma::AsynchIO* data;
    queue<Rdma::Buffer*> queuedWrites;

    ConRec(Rdma::Connection::intrusive_ptr c) :
        connection(c)
    {}
};

void dataError(Rdma::AsynchIO&) {
    cout << "Data error:\n";
}

void idle(ConRec* cr, Rdma::AsynchIO& a) {
    // Need to make sure full is not called as it would reorder messages
    while (!cr->queuedWrites.empty() && a.writable()) {
        Rdma::Buffer* buf = cr->queuedWrites.front();
        cr->queuedWrites.pop();
        a.queueWrite(buf);
    }
}

void data(ConRec* cr, Rdma::AsynchIO& a, Rdma::Buffer* b) {
    // Echo data back
    Rdma::Buffer* buf = a.getBuffer();
    std::copy(b->bytes+b->dataStart, b->bytes+b->dataStart+b->dataCount, buf->bytes);
    buf->dataCount = b->dataCount;
    if (cr->queuedWrites.empty()) {
        // If can't write then full will be called and push buffer on back of queue
        a.queueWrite(buf);
    } else {
        cr->queuedWrites.push(buf);
        // Try to empty queue
        idle(cr, a);
    }
}

void full(ConRec* cr, Rdma::AsynchIO&, Rdma::Buffer* buf) {
    cr->queuedWrites.push(buf);
}

void disconnected(Rdma::Connection::intrusive_ptr& ci) {
    ConRec* cr = ci->getContext<ConRec>();
    cr->connection->disconnect();
    cr->data->queueWriteClose();
    delete cr;
    cout << "Disconnected: " << cr << "\n";
}

void connectionError(Rdma::Connection::intrusive_ptr& ci, Rdma::ErrorType) {
    ConRec* cr = ci->getContext<ConRec>();
    cr->connection->disconnect();
    if (cr) {
        cr->data->queueWriteClose();
        delete cr;
    }
    cout << "Connection error: " << cr << "\n";
}

bool connectionRequest(Rdma::Connection::intrusive_ptr& ci,  const Rdma::ConnectionParams& cp) {
    cout << "Incoming connection: ";

    // For fun reject alternate connection attempts
    static bool x = false;
    x = true;

    // Must create aio here so as to prepost buffers *before* we accept connection
    if (x) {
        ConRec* cr = new ConRec(ci);
        Rdma::AsynchIO* aio =
            new Rdma::AsynchIO(ci->getQueuePair(),
                cp.maxRecvBufferSize, cp.initialXmitCredit, Rdma::DEFAULT_WR_ENTRIES,
                boost::bind(data, cr, _1, _2),
                boost::bind(idle, cr, _1),
                boost::bind(full, cr, _1, _2),
                dataError);
        ci->addContext(cr);
        cr->data = aio;
        cout << "Accept=>" << cr << "\n";
    } else {
        cout << "Reject\n";
    }

    return x;
}

void connected(Poller::shared_ptr poller, Rdma::Connection::intrusive_ptr& ci) {
    static int cnt = 0;
    ConRec* cr = ci->getContext<ConRec>();
    cout << "Connected: " << cr << "(" << ++cnt << ")\n";

    cr->data->start(poller);
}

}} // namespace qpid::tests

using namespace qpid::tests;

int main(int argc, char* argv[]) {
    vector<string> args(&argv[0], &argv[argc]);

    std::string port = (args.size() < 2) ? "20079" : args[1];
    cout << "Listening on port: " << port << "\n";

    try {
        boost::shared_ptr<Poller> p(new Poller());
        Dispatcher d(p);

        SocketAddress sa("", port);
        Rdma::Listener a(sa,
            Rdma::ConnectionParams(16384, Rdma::DEFAULT_WR_ENTRIES),
            boost::bind(connected, p, _1),
            connectionError,
            disconnected,
            connectionRequest);


        a.start(p);
        d.run();
    } catch (Rdma::Exception& e) {
        int err = e.getError();
        cerr << "Error: " << e.what() << "(" << err << ")\n";
    }
}
