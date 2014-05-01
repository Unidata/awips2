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

#include <qpid/messaging/Connection.h>
#include <qpid/messaging/MapView.h>
#include <qpid/messaging/Message.h>
#include <qpid/messaging/Receiver.h>
#include <qpid/messaging/Session.h>
#include <qpid/Exception.h>
#include <qpid/Options.h>
#include <qpid/log/Logger.h>
#include <qpid/log/Options.h>
#include <qpid/sys/Time.h>

#include <iostream>

using namespace qpid::messaging;
using qpid::sys::Duration;
using qpid::sys::TIME_INFINITE;
using qpid::sys::TIME_SEC;

struct Options : public qpid::Options
{
    bool help;
    std::string url;
    std::string address;
    int64_t timeout;
    bool forever;
    qpid::log::Options log;

    Options(const std::string& argv0=std::string())
        : qpid::Options("Options"),
          help(false),
          url("amqp:tcp:127.0.0.1"),
          timeout(0),
          forever(false),
          log(argv0)
    {
        addOptions()
            ("broker,b", qpid::optValue(url, "URL"), "url of broker to connect to")
            ("address,a", qpid::optValue(address, "ADDRESS"), "address to drain from")
            ("timeout,t", qpid::optValue(timeout, "TIMEOUT"), "timeout in seconds to wait before exiting")
            ("forever,f", qpid::optValue(forever), "ignore timeout and wait forever")
            ("help", qpid::optValue(help), "print this usage statement");
        add(log);
    }

    Duration getTimeout()
    {
        if (forever) return TIME_INFINITE;
        else return timeout*TIME_SEC;

    }
    bool parse(int argc, char** argv)
    {
        try {
            qpid::Options::parse(argc, argv);
            if (address.empty()) throw qpid::Exception("Address must be specified!");
            qpid::log::Logger::instance().configure(log);
            if (help) {
                std::ostringstream msg;
                std::cout << msg << *this << std::endl << std::endl 
                          << "Drains messages from the specified address" << std::endl;
                return false;
            } else {
                return true;
            }
        } catch (const std::exception& e) {
            std::cerr << *this << std::endl << std::endl << e.what() << std::endl;
            return false;
        }
    }
};


int main(int argc, char** argv)
{
    Options options(argv[0]);
    if (options.parse(argc, argv)) {
        try {
            Connection connection = Connection::open(options.url);
            Session session = connection.newSession();
            Receiver receiver = session.createReceiver(options.address);
            Duration timeout = options.getTimeout();
            Message message;
            while (receiver.fetch(message, timeout)) {
                std::cout << "Message(properties=" << message.getHeaders() << ", content='" ;
                if (message.getContentType() == "amqp/map") {
                    std::cout << MapView(message);
                } else {
                    std::cout << message.getContent();
                }
                std::cout  << "')" << std::endl;
                session.acknowledge();
            }
            receiver.cancel();
            session.close();            
            connection.close();
            return 0;
        } catch(const std::exception& error) {
            std::cout << error.what() << std::endl;
        }
    }
    return 1;   
}


