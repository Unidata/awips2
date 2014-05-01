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
#include <boost/shared_ptr.hpp>
#include <boost/assign.hpp>

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
using boost::assign::list_of;


#include "ClusterFixture.h"

namespace qpid {
namespace tests {

ClusterFixture::ClusterFixture(size_t n, const Args& args_, int localIndex_)
    : name(Uuid(true).str()), localIndex(localIndex_), userArgs(args_)
{
    add(n);
}

ClusterFixture::ClusterFixture(size_t n, boost::function<void (Args&, size_t)> updateArgs_, int localIndex_)
    : name(Uuid(true).str()), localIndex(localIndex_), updateArgs(updateArgs_)
{
    add(n);
}

ClusterFixture::Args ClusterFixture::makeArgs(const std::string& prefix, size_t index) {
    Args args = list_of<string>("qpidd ")
        ("--cluster-name")(name)
        ("--log-prefix")(prefix);
    args.insert(args.end(), userArgs.begin(), userArgs.end());
    if (updateArgs) updateArgs(args, index);
    return args;
}

void ClusterFixture::add() {
    if (size() != size_t(localIndex))  { // fork a broker process.
        std::ostringstream os; os << "fork" << size();
        std::string prefix = os.str();
        forkedBrokers.push_back(shared_ptr<ForkedBroker>(new ForkedBroker(makeArgs(prefix, size()))));
        push_back(forkedBrokers.back()->getPort());
    }
    else {                      // Run in this process
        addLocal();
    }
}

namespace {
/** Parse broker & cluster options */
Broker::Options parseOpts(size_t argc, const char* argv[]) {
    Broker::Options opts;
    Plugin::addOptions(opts); // Pick up cluster options.
    opts.parse(argc, argv, "", true); // Allow-unknown for --load-module
    return opts;
}
}

void ClusterFixture::addLocal() {
    assert(int(size()) == localIndex);
    ostringstream os; os << "local" << localIndex;
    string prefix = os.str();
    Args args(makeArgs(prefix, localIndex));
    vector<const char*> argv(args.size());
    transform(args.begin(), args.end(), argv.begin(), boost::bind(&string::c_str, _1));
    qpid::log::Logger::instance().setPrefix(prefix);
    localBroker.reset(new BrokerFixture(parseOpts(argv.size(), &argv[0])));
    push_back(localBroker->getPort());
    forkedBrokers.push_back(shared_ptr<ForkedBroker>());
}

bool ClusterFixture::hasLocal() const { return localIndex >= 0 && size_t(localIndex) < size(); }

/** Kill a forked broker with sig, or shutdown localBroker if n==0. */
void ClusterFixture::kill(size_t n, int sig) {
    if (n == size_t(localIndex))
        localBroker->broker->shutdown();
    else
        forkedBrokers[n]->kill(sig);
}

/** Kill a broker and suppressing errors from closing connection c. */
void ClusterFixture::killWithSilencer(size_t n, client::Connection& c, int sig) {
    ScopedSuppressLogging sl;
    kill(n,sig);
    try { c.close(); } catch(...) {}
}

/**
 * Get the known broker ports from a Connection.
 *@param n if specified wait for the cluster size to be n, up to a timeout.
 */
std::set<int> knownBrokerPorts(qpid::client::Connection& c, int n) {
    FailoverListener fl(c);
    std::vector<qpid::Url> urls = fl.getKnownBrokers();
    if (n >= 0 && unsigned(n) != urls.size()) {
        // Retry up to 10 secs in .1 second intervals.
        for (size_t retry=100; urls.size() != unsigned(n) && retry != 0; --retry) {
            qpid::sys::usleep(1000*100); // 0.1 secs
            urls = fl.getKnownBrokers();
        }
    }
    std::set<int> s;
    for (std::vector<qpid::Url>::const_iterator i = urls.begin(); i != urls.end(); ++i)
        s.insert((*i)[0].get<qpid::TcpAddress>()->port);
    return s;
}

}} // namespace qpid::tests
