#ifndef TESTS_FORKEDBROKER_H
#define TESTS_FORKEDBROKER_H


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

#include "qpid/Exception.h"
#include "qpid/log/Statement.h"
#include "qpid/broker/Broker.h"
#include <boost/lexical_cast.hpp>
#include <string>
#include <stdio.h>
#include <sys/wait.h>

namespace qpid {
namespace tests {

/**
 * Class to fork a broker child process.
 *
 * For most tests a BrokerFixture may be more convenient as it starts
 * a broker in the same process which allows you to easily debug into
 * the broker.
 *
 * This useful for tests that need to start multiple brokers where
 * those brokers can't coexist in the same process (e.g. for cluster
 * tests where CPG doesn't allow multiple group members in a single
 * process.)
 *
 */
class ForkedBroker {
  public:
    typedef std::vector<std::string> Args;

    // argv args are passed to broker.
    //
    // Special value "TMP_DATA_DIR" is substituted with a temporary
    // data directory for the broker.
    //
    ForkedBroker(const Args& argv);
    ~ForkedBroker();

    void kill(int sig=SIGINT);
    int wait();                 // Wait for exit, return exit status.
    uint16_t getPort() { return port; }
    pid_t getPID() { return pid; }

  private:

    void init(const Args& args);

    pid_t pid;
    int port;
    std::string dataDir;
};

}} // namespace qpid::tests

#endif  /*!TESTS_FORKEDBROKER_H*/
