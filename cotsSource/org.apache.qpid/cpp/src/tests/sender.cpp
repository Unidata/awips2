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

#include <qpid/client/FailoverManager.h>
#include <qpid/client/Session.h>
#include <qpid/client/AsyncSession.h>
#include <qpid/client/Message.h>
#include <qpid/client/MessageReplayTracker.h>
#include <qpid/client/QueueOptions.h>
#include <qpid/Exception.h>
#include "TestOptions.h"

#include <fstream>
#include <iostream>

using namespace qpid;
using namespace qpid::client;
using namespace qpid::framing;

using namespace std;

namespace qpid {
namespace tests {

struct Args : public qpid::TestOptions
{
    string destination;
    string key;
    uint sendEos;
    bool durable;
    uint ttl;
    string lvqMatchValue;
    string lvqMatchFile;

    Args() : key("test-queue"), sendEos(0), durable(false), ttl(0)
    {
        addOptions()
            ("exchange", qpid::optValue(destination, "EXCHANGE"), "Exchange to send messages to")
            ("routing-key", qpid::optValue(key, "KEY"), "Routing key to add to messages")
            ("send-eos", qpid::optValue(sendEos, "N"), "Send N EOS messages to mark end of input")
            ("durable", qpid::optValue(durable, "true|false"), "Mark messages as durable.")
	    ("ttl", qpid::optValue(ttl, "msecs"), "Time-to-live for messages, in milliseconds")
            ("lvq-match-value", qpid::optValue(lvqMatchValue, "KEY"), "The value to set for the LVQ match key property")
            ("lvq-match-file", qpid::optValue(lvqMatchFile, "FILE"), "A file containing values to set for the LVQ match key property");
    }
};

const string EOS("eos");

class Sender : public FailoverManager::Command
{
  public:
    Sender(const std::string& destination, const std::string& key, uint sendEos, bool durable, uint ttl,
           const std::string& lvqMatchValue, const std::string& lvqMatchFile);
    void execute(AsyncSession& session, bool isRetry);
  private:
    const std::string destination;
    MessageReplayTracker sender;
    Message message;
    const uint sendEos;
    uint sent;
    std::ifstream lvqMatchValues;
};

Sender::Sender(const std::string& dest, const std::string& key, uint eos, bool durable, uint ttl, const std::string& lvqMatchValue, const std::string& lvqMatchFile) :
    destination(dest), sender(10), message("", key), sendEos(eos), sent(0) , lvqMatchValues(lvqMatchFile.c_str())
{
    if (durable){
        message.getDeliveryProperties().setDeliveryMode(framing::PERSISTENT);
    }

    if (ttl) {
        message.getDeliveryProperties().setTtl(ttl);
    }

    if (!lvqMatchValue.empty()) {
        message.getHeaders().setString(QueueOptions::strLVQMatchProperty, lvqMatchValue);
    }
}

void Sender::execute(AsyncSession& session, bool isRetry)
{
    if (isRetry) sender.replay(session);
    else sender.init(session);
    string data;
    while (getline(std::cin, data)) {
        message.setData(data);
        message.getHeaders().setInt("sn", ++sent);
        string matchKey;
        if (lvqMatchValues && getline(lvqMatchValues, matchKey)) {
            message.getHeaders().setString(QueueOptions::strLVQMatchProperty, matchKey);
        }
        sender.send(message, destination);
    }
    for (uint i = sendEos; i > 0; --i) {
        message.setData(EOS);
        sender.send(message, destination);
    }
}

}} // namespace qpid::tests

using namespace qpid::tests;

int main(int argc, char ** argv)
{
    Args opts;
    try {
        opts.parse(argc, argv);
        FailoverManager connection(opts.con);
        Sender sender(opts.destination, opts.key, opts.sendEos, opts.durable, opts.ttl, opts.lvqMatchValue, opts.lvqMatchFile);
        connection.execute(sender);
        connection.close();
        return 0;
    } catch(const std::exception& error) {
        std::cout << "Failed: " << error.what() << std::endl;
    }
    return 1;
}
