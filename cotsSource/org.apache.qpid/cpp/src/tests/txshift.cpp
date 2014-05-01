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
#include "qpid/log/Statement.h"
#include "qpid/sys/Thread.h"

using namespace qpid::client;
using namespace qpid::sys;

namespace qpid {
namespace tests {

struct Args : public qpid::TestOptions
{
    string workQueue;
    size_t workers;

    Args() : workQueue("txshift-control"), workers(1)
    {
        addOptions()
            ("workers", qpid::optValue(workers, "N"), "Number of separate worker sessions to start")
            ("work-queue", qpid::optValue(workQueue, "NAME"), "work queue from which to take instructions");
    }
};

struct Transfer : MessageListener
{
    std::string control;
    std::string source;
    std::string destination;
    uint expected;
    uint transfered;
    SubscriptionSettings controlSettings;
    Subscription controlSubscription;
    SubscriptionSettings sourceSettings;
    Subscription sourceSubscription;

    Transfer(const std::string control_) : control(control_), expected(0), transfered(0) {}

    void subscribeToSource(SubscriptionManager manager)
    {
        sourceSettings.autoAck = 0;//will accept once at the end of the batch
        sourceSettings.flowControl = FlowControl::messageCredit(expected);
        sourceSubscription = manager.subscribe(*this, source, sourceSettings);
        QPID_LOG(info, "Subscribed to source: " << source << " expecting: " << expected);
    }

    void subscribeToControl(SubscriptionManager manager)
    {
        controlSettings.flowControl = FlowControl::messageCredit(1);
        controlSubscription = manager.subscribe(*this, control, controlSettings);
        QPID_LOG(info, "Subscribed to job queue");
    }

    void received(Message& message)
    {
        QPID_LOG(debug, "received: " << message.getData() << " for " << message.getDestination());
        if (message.getDestination() == source) {
            receivedFromSource(message);
        } else if (message.getDestination() == control) {
            receivedFromControl(message);
        } else {
            QPID_LOG(error, "Unexpected message: " << message.getData() << " to " << message.getDestination());
        }
    }

    void receivedFromSource(Message& message)
    {
        QPID_LOG(debug, "transfering  " << (transfered+1) << " of " << expected);
        message.getDeliveryProperties().setRoutingKey(destination);
        async(sourceSubscription.getSession()).messageTransfer(arg::content=message);
        if (++transfered == expected) {
            QPID_LOG(info, "completed job: " << transfered << " messages shifted from " <<
                     source << " to " << destination);
            sourceSubscription.accept(sourceSubscription.getUnaccepted());
            sourceSubscription.getSession().txCommit();
            sourceSubscription.cancel();
            //grant credit to allow broker to send us another control message
            controlSubscription.grantMessageCredit(1);
        }
    }

    void receivedFromControl(Message& message)
    {
        if (message.getData() == "transfer") {
            source = message.getHeaders().getAsString("src");
            destination = message.getHeaders().getAsString("dest");
            expected = message.getHeaders().getAsInt("count");
            transfered = 0;
            QPID_LOG(info, "received transfer request: " << expected << " messages to be shifted from " <<
                     source << " to " << destination);
            subscribeToSource(controlSubscription.getSubscriptionManager());
        } else if (message.getData() == "quit") {
            QPID_LOG(info, "received quit request");
            controlSubscription.cancel();
        } else {
            std::cerr << "Rejecting invalid message: " << message.getData() << std::endl;
            controlSubscription.getSession().messageReject(SequenceSet(message.getId()));
        }
    }

};

struct Worker : FailoverManager::Command, Runnable
{
    FailoverManager& connection;
    Transfer transfer;
    Thread runner;

    Worker(FailoverManager& c, const std::string& controlQueue) : connection(c), transfer(controlQueue) {}

    void run()
    {
        connection.execute(*this);
    }

    void start()
    {
        runner = Thread(this);
    }

    void join()
    {
        runner.join();
    }

    void execute(AsyncSession& session, bool isRetry)
    {
        if (isRetry) QPID_LOG(info, "Retrying...");
        session.txSelect();
        SubscriptionManager subs(session);
        transfer.subscribeToControl(subs);
        subs.run();
        session.txCommit();//commit accept of control messages
    }
};

}} // namespace qpid::tests

using namespace qpid::tests;

int main(int argc, char** argv)
{
    Args opts;
    try {
        opts.parse(argc, argv);
        FailoverManager connection(opts.con);
        connection.connect();
        if (opts.workers == 1) {
            Worker worker(connection, opts.workQueue);
            worker.run();
        } else {
            boost::ptr_vector<Worker> workers;
            for (size_t i = 0; i < opts.workers; i++) {
                workers.push_back(new Worker(connection, opts.workQueue));
            }
            std::for_each(workers.begin(), workers.end(), boost::bind(&Worker::start, _1));
            std::for_each(workers.begin(), workers.end(), boost::bind(&Worker::join, _1));
        }

        return 0;
    } catch(const std::exception& e) {
	std::cout << e.what() << std::endl;
        return 1;
    }
}
