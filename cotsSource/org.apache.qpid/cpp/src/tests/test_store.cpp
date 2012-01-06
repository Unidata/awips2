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


/**@file
 * Plug-in message store for tests.
 *
 * Add functionality as required, build up a comprehensive set of
 * features to support persistent behavior tests.
 *
 * Current features special "action" messages can:
 *  - raise exception from enqueue.
 *  - force host process to exit.
 *  - do async completion after a delay.
 */

#include "qpid/broker/NullMessageStore.h"
#include "qpid/broker/Broker.h"
#include "qpid/framing/AMQFrame.h"
#include "qpid/log/Statement.h"
#include "qpid/Plugin.h"
#include "qpid/Options.h"
#include <boost/cast.hpp>
#include <boost/lexical_cast.hpp>
#include <memory>
#include <fstream>

using namespace qpid;
using namespace broker;
using namespace std;
using namespace qpid::sys;

namespace qpid {
namespace tests {

struct TestStoreOptions : public Options {

    string name;
    string dump;

    TestStoreOptions() : Options("Test Store Options") {
        addOptions()
            ("test-store-name", optValue(name, "NAME"), "Name of test store instance.")
            ("test-store-dump", optValue(dump, "FILE"), "File to dump enqueued messages.")
            ;
    }
};

struct Completer : public Runnable {
    boost::intrusive_ptr<PersistableMessage> message;
    int usecs;
    Completer(boost::intrusive_ptr<PersistableMessage> m, int u) : message(m), usecs(u) {}
    void run() {
        qpid::sys::usleep(usecs);
        message->enqueueComplete();
        delete this;
    }
};

class TestStore : public NullMessageStore {
  public:
    TestStore(const TestStoreOptions& opts, Broker& broker_)
        : options(opts), name(opts.name), broker(broker_)
    {
        QPID_LOG(info, "TestStore name=" << name << " dump=" << options.dump);
        if (!options.dump.empty()) 
            dump.reset(new ofstream(options.dump.c_str()));
    }

    ~TestStore() {
        for_each(threads.begin(), threads.end(), boost::bind(&Thread::join, _1));
    }

    virtual bool isNull() const { return false; }
    
    void enqueue(TransactionContext* ,
                 const boost::intrusive_ptr<PersistableMessage>& pmsg,
                 const PersistableQueue& )
    {
        Message* msg = dynamic_cast<Message*>(pmsg.get());
        assert(msg);

        // Dump the message if there is a dump file.
        if (dump.get()) {
            msg->getFrames().getMethod()->print(*dump);
            *dump  << endl << "  ";
            msg->getFrames().getHeaders()->print(*dump);
            *dump << endl << "  ";
            *dump << msg->getFrames().getContentSize() << endl;
        }

        // Check the message for special instructions.
        string data = msg->getFrames().getContent();
        size_t i = string::npos;
        size_t j = string::npos;
        if (strncmp(data.c_str(), TEST_STORE_DO.c_str(), strlen(TEST_STORE_DO.c_str())) == 0
            && (i = data.find(name+"[")) != string::npos
            && (j = data.find("]", i)) != string::npos)
        {
            size_t start = i+name.size()+1;
            string action = data.substr(start, j-start);

            if (action == EXCEPTION) {
                throw Exception(QPID_MSG("TestStore " << name << " throwing exception for: " << data));
            }
            else if (action == EXIT_PROCESS) {
                // FIXME aconway 2009-04-10: this is a dubious way to
                // close the process at best, it can cause assertions or seg faults
                // rather than clean exit.
                QPID_LOG(critical, "TestStore " << name << " forcing process exit for: " << data);
                exit(0);
            }
            else if (strncmp(action.c_str(), ASYNC.c_str(), strlen(ASYNC.c_str())) == 0) {
                std::string delayStr(action.substr(ASYNC.size()));
                int delay = boost::lexical_cast<int>(delayStr);
                threads.push_back(Thread(*new Completer(msg, delay)));
            }
            else {
                QPID_LOG(error, "TestStore " << name << " unknown action " << action);
                msg->enqueueComplete();
            }
        }
        else
            msg->enqueueComplete();
    }

  private:
    static const string TEST_STORE_DO, EXCEPTION, EXIT_PROCESS, ASYNC;
    TestStoreOptions options;
    string name;
    Broker& broker;
    vector<Thread> threads;
    std::auto_ptr<ofstream> dump;
};

const string TestStore::TEST_STORE_DO = "TEST_STORE_DO: ";
const string TestStore::EXCEPTION = "exception";
const string TestStore::EXIT_PROCESS = "exit_process";
const string TestStore::ASYNC="async ";

struct TestStorePlugin : public Plugin {

    TestStoreOptions options;

    Options* getOptions() { return &options; }

    void earlyInitialize (Plugin::Target& target)
    {
        Broker* broker = dynamic_cast<Broker*>(&target);
        if (!broker) return;
        boost::shared_ptr<MessageStore> p(new TestStore(options, *broker));
        broker->setStore (p);
    }

    void initialize(qpid::Plugin::Target&) {}
};

static TestStorePlugin pluginInstance;

}} // namespace qpid::tests
