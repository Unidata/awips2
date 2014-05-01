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

#include <qpid/messaging/Address.h>
#include <qpid/messaging/Connection.h>
#include <qpid/messaging/Message.h>
#include <qpid/messaging/Receiver.h>
#include <qpid/messaging/Sender.h>
#include <qpid/messaging/Session.h>
#include <qpid/messaging/Variant.h>

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <sstream>

using namespace qpid::messaging;

using std::stringstream;
using std::string;

int main(int argc, char** argv) {
    const char* url = argc>1 ? argv[1] : "amqp:tcp:127.0.0.1:5672";

    try {
        Connection connection = Connection::open(url);
        Session session = connection.newSession();
        Receiver receiver = session.createReceiver("service_queue; {create: always}");

        while (true) {
            Message request = receiver.fetch();
            const Address& address = request.getReplyTo();
            if (address) {
                Sender sender = session.createSender(address);
                std::string s = request.getContent();
                std::transform(s.begin(), s.end(), s.begin(), toupper);
                Message response(s);
                sender.send(response);
                std::cout << "Processed request: " 
                          << request.getContent() 
                          << " -> " 
                          << response.getContent() << std::endl;
                session.acknowledge();
            } else {
                std::cerr << "Error: no reply address specified for request: " << request.getContent() << std::endl;
                session.reject(request);
            }
        }
        connection.close();
        return 0;
    } catch(const std::exception& error) {
        std::cout << error.what() << std::endl;
    }
    return 1;
}


