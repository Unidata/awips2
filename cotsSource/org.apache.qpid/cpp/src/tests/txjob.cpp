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

#include <iostream>
#include <boost/bind.hpp>
#include <boost/ptr_container/ptr_vector.hpp>

#include "TestOptions.h"
#include "qpid/client/AsyncSession.h"
#include "qpid/client/FailoverManager.h"
#include "qpid/client/Message.h"
#include "qpid/client/SubscriptionManager.h"
#include "qpid/sys/Thread.h"

using namespace qpid::client;
using namespace qpid::sys;

namespace qpid {
namespace tests {

struct Args : public qpid::TestOptions
{
    string workQueue;
    string source;
    string dest;
    uint messages;
    uint jobs;
    bool quit;
    bool declareQueues;

    Args() : workQueue("txshift-control"), source("txshift-1"), dest("txshift-2"), messages(0), jobs(0),
             quit(false), declareQueues(false)
    {
        addOptions()
            ("messages", qpid::optValue(messages, "N"), "Number of messages to shift")
            ("jobs", qpid::optValue(jobs, "N"), "Number of shift jobs to request")
            ("source", qpid::optValue(source, "QUEUE NAME"), "source queue from which messages will be shifted")
            ("dest", qpid::optValue(dest, "QUEUE NAME"), "dest queue to which messages will be shifted")
            ("work-queue", qpid::optValue(workQueue, "QUEUE NAME"), "work queue from which to take instructions")
            ("add-quit", qpid::optValue(quit), "add a 'quit' instruction to the queue (after any other jobs)")
            ("declare-queues", qpid::optValue(declareQueues), "issue a declare for all queues");
    }
};

}} // namespace qpid::tests

using namespace qpid::tests;

//TODO: might be nice to make this capable of failover as well at some
//point; for now its just for the setup phase.
int main(int argc, char** argv)
{
    Args opts;
    try {
        opts.parse(argc, argv);
        Connection connection;
        connection.open(opts.con);
        Session session = connection.newSession();
        if (opts.declareQueues) {
            session.queueDeclare(arg::queue=opts.workQueue);
            session.queueDeclare(arg::queue=opts.source);
            session.queueDeclare(arg::queue=opts.dest);
        }
        for (uint i = 0; i < opts.jobs; ++i) {
            Message job("transfer", opts.workQueue);
            job.getHeaders().setString("src", opts.source);
            job.getHeaders().setString("dest", opts.dest);
            job.getHeaders().setInt("count", opts.messages);
            async(session).messageTransfer(arg::content=job);
        }

        if (opts.quit) {
            async(session).messageTransfer(arg::content=Message("quit", opts.workQueue));
        }

        session.sync();
        session.close();

        return 0;
    } catch(const std::exception& e) {
	std::cout << e.what() << std::endl;
        return 1;
    }
}
