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

#include "test_tools.h"
#include "unit_test.h"
#include "qpid/sys/Poller.h"
#include "qpid/sys/PollableCondition.h"
#include "qpid/sys/Monitor.h"
#include "qpid/sys/Time.h"
#include "qpid/sys/Thread.h"
#include <boost/bind.hpp>

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(PollableConditionTest)

using namespace qpid::sys;

const Duration SHORT = TIME_SEC/100;
const Duration LONG = TIME_SEC/10;

class  Callback {
  public:
    enum Action { NONE, CLEAR };

    Callback() : count(), action(NONE) {}

    void call(PollableCondition& pc) {
        Mutex::ScopedLock l(lock);
        ++count;
        switch(action) {
          case NONE: break;
          case CLEAR: pc.clear(); break;
        }
        action = NONE;
        lock.notify();
    }

    bool isCalling() { Mutex::ScopedLock l(lock); return wait(LONG); }

    bool isNotCalling() { Mutex::ScopedLock l(lock); return !wait(SHORT); }

    bool nextCall(Action a=NONE) {
        Mutex::ScopedLock l(lock);
        action = a;
        return wait(LONG);
    }

  private:
    bool wait(Duration timeout) {
        int n = count;
        AbsTime deadline(now(), timeout);
        while (n == count && lock.wait(deadline))
               ;
        return n != count;
    }

    Monitor lock;
    int count;
    Action action;
};

QPID_AUTO_TEST_CASE(testPollableCondition) {
    boost::shared_ptr<Poller> poller(new Poller());
    Callback callback;
    PollableCondition pc(boost::bind(&Callback::call, &callback, _1), poller);

    Thread runner = Thread(*poller);

    BOOST_CHECK(callback.isNotCalling()); // condition is not set.

    pc.set();
    BOOST_CHECK(callback.isCalling()); // Set.
    BOOST_CHECK(callback.isCalling()); // Still set.

    callback.nextCall(Callback::CLEAR);
    BOOST_CHECK(callback.isNotCalling()); // Cleared

    pc.set();
    BOOST_CHECK(callback.isCalling()); // Set.
    callback.nextCall(Callback::CLEAR);
    BOOST_CHECK(callback.isNotCalling()); // Cleared.

    poller->shutdown();
    runner.join();
}

QPID_AUTO_TEST_SUITE_END()

}} //namespace qpid::tests
