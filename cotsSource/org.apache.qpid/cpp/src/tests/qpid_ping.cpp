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

#include "TestOptions.h"
#include "qpid/client/SubscriptionManager.h"
#include "qpid/client/Connection.h"
#include "qpid/client/AsyncSession.h"
#include "qpid/sys/Time.h"
#include "qpid/sys/Thread.h"
#include "qpid/sys/Runnable.h"
#include "qpid/sys/Monitor.h"
#include "qpid/framing/Uuid.h"
#include <string>
#include <iostream>

using namespace std;
using namespace qpid::sys;
using namespace qpid::framing;
using namespace qpid::client;
using namespace qpid;

namespace qpid {
namespace tests {

struct PingOptions : public qpid::TestOptions {
    int timeout;                // Timeout in seconds.
    bool quiet;                 // No output
    PingOptions() : timeout(1), quiet(false) {
        addOptions()
            ("timeout,t", optValue(timeout, "SECONDS"), "Max time to wait.")
            ("quiet,q", optValue(quiet), "Don't print anything to stderr/stdout.");
    }
};

PingOptions opts;

class Ping : public Runnable {
    Connection connection;
    Thread thread;
    Monitor lock;
    enum { WAITING, SUCCESS, ERROR } status;

  public:
    Ping() : status(WAITING) {}

    void run() {
        try {
            opts.open(connection);
            if (!opts.quiet) cout << "Opened connection." << endl;
            AsyncSession s = connection.newSession();
            string qname(Uuid(true).str());
            s.queueDeclare(arg::queue=qname,arg::autoDelete=true,arg::exclusive=true);
            s.messageTransfer(arg::content=Message("hello", qname));
            if (!opts.quiet) cout << "Sent message." << endl;
            SubscriptionManager subs(s);
            subs.get(qname);
            if (!opts.quiet) cout << "Received message." << endl;
            s.sync();
            s.close();
            connection.close();
            Mutex::ScopedLock l(lock);
            status = SUCCESS;
            lock.notifyAll();
        } catch (const exception& e) {
            if (!opts.quiet)
                cerr << "Unexpected exception: " << e.what() << endl;
            Mutex::ScopedLock l(lock);
            status = ERROR;
            lock.notifyAll();
        }
    }

    void start() { thread=Thread(this); }

    bool wait() {
        Mutex::ScopedLock l(lock);
        AbsTime deadline(now(), opts.timeout*TIME_SEC);
        while (status == WAITING && lock.wait(deadline))
            ;
        if (status == WAITING && !opts.quiet)
            cerr << "Timed out after " << opts.timeout << " seconds." << endl;
        if (status != WAITING) thread.join();
        return status == SUCCESS;
    }
};

}} // namespace qpid::tests

using namespace qpid::tests;

int main(int argc, char** argv) {
    try {
        opts.parse(argc, argv);
        Ping ping;
        ping.start();
        if (!ping.wait()) exit(1);
        if (!opts.quiet) cout << "Success!" << endl;
        return 0;
    } catch (const exception& e) {
        if (!opts.quiet)
            cerr << "Unexpected exception: " << e.what()  << endl;
        return 1;
    }
}
