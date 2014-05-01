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

#include "qpid/sys/Poller.h"
#include "qpid/sys/IOHandle.h"
#include "qpid/sys/Dispatcher.h"
#include "qpid/sys/DispatchHandle.h"
#include "qpid/sys/posix/PrivatePosix.h"
#include "qpid/sys/Thread.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>

#include <iostream>
#include <boost/bind.hpp>

using namespace std;
using namespace qpid::sys;

namespace qpid {
namespace tests {

int writeALot(int fd, const string& s) {
    int bytesWritten = 0;
    do {
        errno = 0;
        int lastWrite = ::write(fd, s.c_str(), s.size());
        if ( lastWrite >= 0) {
            bytesWritten += lastWrite;
        }
    } while (errno != EAGAIN);
    return bytesWritten;
}

int readALot(int fd) {
    int bytesRead = 0;
    char buf[10240];

    do {
        errno = 0;
        int lastRead = ::read(fd, buf, sizeof(buf));
        if ( lastRead >= 0) {
            bytesRead += lastRead;
        }
    } while (errno != EAGAIN);
    return bytesRead;
}

int64_t writtenBytes = 0;
int64_t readBytes = 0;

void writer(DispatchHandle& h, int fd, const string& s) {
    writtenBytes += writeALot(fd, s);
    h.rewatch();
}

void reader(DispatchHandle& h, int fd) {
    readBytes += readALot(fd);
    h.rewatch();
}

void rInterrupt(DispatchHandle&) {
	cerr << "R";
}

void wInterrupt(DispatchHandle&) {
	cerr << "W";
}

DispatchHandle::Callback rcb = rInterrupt;
DispatchHandle::Callback wcb = wInterrupt;

DispatchHandleRef *volatile rh = 0;
DispatchHandleRef *volatile wh = 0;

volatile bool stopWait = false;
volatile bool phase1finished = false;

timer_t timer;

void stop_handler(int /*signo*/, siginfo_t* /*info*/, void* /*context*/) {
    stopWait = true;
}

void timer_handler(int /*signo*/, siginfo_t* /*info*/, void* /*context*/) {
    static int count = 0;
    if (count++ < 10) {
        rh->call(rcb);
	   wh->call(wcb);
    } else {
        phase1finished = true;
        assert(::timer_delete(timer) == 0);
    }
}

}} // namespace qpid::tests

using namespace qpid::tests;

int main(int /*argc*/, char** /*argv*/)
{
    // Create poller
    Poller::shared_ptr poller(new Poller);

    // Create dispatcher thread
    Dispatcher d(poller);
    Dispatcher d1(poller);
    Dispatcher d2(poller);
    Dispatcher d3(poller);
    Thread dt(d);
    Thread dt1(d1);
    Thread dt2(d2);
    Thread dt3(d3);

    // Setup sender and receiver
    int sv[2];
    int rc = ::socketpair(AF_UNIX, SOCK_STREAM, 0, sv);
    assert(rc >= 0);

    // Set non-blocking
    rc = ::fcntl(sv[0], F_SETFL, O_NONBLOCK);
    assert(rc >= 0);

    rc = ::fcntl(sv[1], F_SETFL, O_NONBLOCK);
    assert(rc >= 0);

    // Make up a large string
    string testString = "This is only a test ... 1,2,3,4,5,6,7,8,9,10;";
    for (int i = 0; i < 8; i++)
        testString += testString;

    PosixIOHandle f0(sv[0]);
    PosixIOHandle f1(sv[1]);

    rh = new DispatchHandleRef(f0, boost::bind(reader, _1, sv[0]), 0, 0);
    wh = new DispatchHandleRef(f1, 0, boost::bind(writer, _1, sv[1], testString), 0);

    rh->startWatch(poller);
    wh->startWatch(poller);

    // Set up a regular itimer interupt

    // Ignore signal in this thread
    ::sigset_t sm;
    ::sigemptyset(&sm);
    ::sigaddset(&sm, SIGRTMIN);
    ::pthread_sigmask(SIG_BLOCK, &sm, 0);

    // Signal handling
    struct ::sigaction sa;
    sa.sa_sigaction = timer_handler;
    sa.sa_flags = SA_RESTART | SA_SIGINFO;
    ::sigemptyset(&sa.sa_mask);
    rc = ::sigaction(SIGRTMIN, &sa,0);
    assert(rc == 0);

    ::sigevent se;
    ::memset(&se, 0, sizeof(se)); // Clear to make valgrind happy (this *is* the neatest way to do this portably - sigh)
    se.sigev_notify = SIGEV_SIGNAL;
    se.sigev_signo = SIGRTMIN;
    rc = ::timer_create(CLOCK_REALTIME, &se, &timer);
    assert(rc == 0);

    itimerspec ts = {
    /*.it_value = */ {2, 0},  // s, ns
    /*.it_interval = */ {2, 0}}; // s, ns

    rc = ::timer_settime(timer, 0, &ts, 0);
    assert(rc == 0);

    // wait
    while (!phase1finished) {
        ::sleep(1);
    }

    // Now test deleting/creating DispatchHandles in tight loop, so that we are likely to still be using the
    // attached PollerHandles after deleting the DispatchHandle
    DispatchHandleRef* t = wh;
    wh = 0;
    delete t;
    t = rh;
    rh = 0;
    delete t;

    sa.sa_sigaction = stop_handler;
    rc = ::sigaction(SIGRTMIN, &sa,0);
    assert(rc == 0);

    itimerspec nts = {
    /*.it_value = */ {30, 0},  // s, ns
    /*.it_interval = */ {30, 0}}; // s, ns

    rc = ::timer_create(CLOCK_REALTIME, &se, &timer);
    assert(rc == 0);
    rc = ::timer_settime(timer, 0, &nts, 0);
    assert(rc == 0);

    DispatchHandleRef* rh1;
    DispatchHandleRef* wh1;

    struct timespec w = {0, 1000000};
    while (!stopWait) {
        rh1 = new DispatchHandleRef(f0, boost::bind(reader, _1, sv[0]), 0, 0);
        wh1 = new DispatchHandleRef(f1, 0, boost::bind(writer, _1, sv[1], testString), 0);
        rh1->startWatch(poller);
        wh1->startWatch(poller);

        ::nanosleep(&w, 0);

        delete wh1;
        delete rh1;
    }

    rc = ::timer_delete(timer);
    assert(rc == 0);

    poller->shutdown();
    dt.join();
    dt1.join();
    dt2.join();
    dt3.join();

    cout << "\nWrote: " << writtenBytes << "\n";
    cout << "Read: " << readBytes << "\n";

    return 0;
}
