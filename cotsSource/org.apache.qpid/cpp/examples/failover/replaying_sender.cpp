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
#include <qpid/Exception.h>

#include <iostream>
#include <sstream>

using namespace qpid;
using namespace qpid::client;
using namespace qpid::framing;

using namespace std;

class Sender : public FailoverManager::Command
{
  public:
    Sender(const std::string& queue, uint count);
    void execute(AsyncSession& session, bool isRetry);
    uint getSent();
  private:
    MessageReplayTracker sender;
    const uint count;
    uint sent;
    Message message;
    
};

Sender::Sender(const std::string& queue, uint count_) : sender(10), count(count_), sent(0) 
{
    message.getDeliveryProperties().setRoutingKey(queue);
}

void Sender::execute(AsyncSession& session, bool isRetry)
{
    if (isRetry) sender.replay(session);
    else sender.init(session);
    while (sent < count) {
        stringstream message_data;
        message_data << ++sent;
        message.setData(message_data.str());
        message.getHeaders().setInt("sn", sent);
        sender.send(message);
        if (count > 1000 && !(sent % 1000)) {
            std::cout << "sent " << sent << " of " << count << std::endl;
        }
    }
    message.setData("That's all, folks!");
    sender.send(message);
}

uint Sender::getSent()
{
    return sent;
}

int main(int argc, char ** argv) 
{
    ConnectionSettings settings;
    if (argc > 1) settings.host = argv[1];
    if (argc > 2) settings.port = atoi(argv[2]);
    
    FailoverManager connection(settings);
    Sender sender("message_queue", argc > 3 ? atoi(argv[3]) : 1000);
    try {
        connection.execute(sender);
        std::cout << "Sent " << sender.getSent() << " messages." << std::endl;
        connection.close();
        return 0;  
    } catch(const std::exception& error) {
        std::cout << "Failed: " << error.what() << std::endl;
    }
    return 1;
}
