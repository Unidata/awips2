#ifndef CLUSTER_FIXTURE_H
#define CLUSTER_FIXTURE_H

/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "test_tools.h"
#include "unit_test.h"
#include "ForkedBroker.h"
#include "BrokerFixture.h"

#include "qpid/client/Connection.h"
#include "qpid/client/ConnectionAccess.h"
#include "qpid/client/Session.h"
#include "qpid/client/FailoverListener.h"
#include "qpid/cluster/Cluster.h"
#include "qpid/cluster/Cpg.h"
#include "qpid/cluster/UpdateClient.h"
#include "qpid/framing/AMQBody.h"
#include "qpid/framing/Uuid.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/framing/enum.h"
#include "qpid/log/Logger.h"

#include <boost/bind.hpp>
#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>

#include <string>
#include <iostream>
#include <iterator>
#include <vector>
#include <set>
#include <algorithm>
#include <iterator>


using namespace std;
using namespace qpid;
using namespace qpid::cluster;
using namespace qpid::framing;
using namespace qpid::client;
using qpid::sys::TIME_SEC;
using qpid::broker::Broker;
using boost::shared_ptr;
using qpid::cluster::Cluster;

namespace qpid {
namespace tests {

/** Cluster fixture is a vector of ports for the replicas.
 *
 * At most one replica (by default replica 0) is in the current
 * process, all others are forked as children.
 */
class ClusterFixture : public vector<uint16_t>  {
  public:
    typedef std::vector<std::string> Args;

    /** @param localIndex can be -1 meaning don't automatically start a local broker.
     * A local broker can be started with addLocal().
     */
    ClusterFixture(size_t n, const Args& args, int localIndex=-1);

    /**@param updateArgs function is passed the index of the cluster member and can update the arguments. */
    ClusterFixture(size_t n, boost::function<void (Args&, size_t)> updateArgs, int localIndex=-1);

    void add(size_t n) { for (size_t i=0; i < n; ++i) add(); }
    void add();                 // Add a broker.
    void setup();

    bool hasLocal() const;

    /** Kill a forked broker with sig, or shutdown localBroker. */
    void kill(size_t n, int sig=SIGINT);

    /** Kill a broker and suppressing errors from closing connection c. */
    void killWithSilencer(size_t n, client::Connection& c, int sig=SIGINT);

  private:

    void addLocal();            // Add a local broker.
    Args makeArgs(const std::string& prefix, size_t index);
    string name;
    std::auto_ptr<BrokerFixture> localBroker;
    int localIndex;
    std::vector<shared_ptr<ForkedBroker> > forkedBrokers;
    Args userArgs;
    boost::function<void (Args&, size_t)> updateArgs;
};

/**
 * Get the known broker ports from a Connection.
 *@param n if specified wait for the cluster size to be n, up to a timeout.
 */
std::set<int> knownBrokerPorts(qpid::client::Connection& source, int n=-1);

}} // namespace qpid::tests

#endif  /*!CLUSTER_FIXTURE_H*/
