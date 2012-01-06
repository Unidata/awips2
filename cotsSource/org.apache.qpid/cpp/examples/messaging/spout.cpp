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
#include <qpid/messaging/MapContent.h>
#include <qpid/messaging/Message.h>
#include <qpid/messaging/Sender.h>
#include <qpid/messaging/Session.h>
#include <qpid/messaging/Variant.h>
#include <qpid/framing/Uuid.h>
#include <qpid/Exception.h>
#include <qpid/Options.h>
#include <qpid/log/Logger.h>
#include <qpid/log/Options.h>
#include <qpid/sys/Time.h>

#include <iostream>
#include <vector>

#include <boost/format.hpp>

using namespace qpid::messaging;
using qpid::framing::Uuid;
using qpid::sys::AbsTime;
using qpid::sys::now;
using qpid::sys::TIME_INFINITE;

typedef std::vector<std::string> string_vector;

struct Options : public qpid::Options
{
    bool help;
    std::string url;
    std::string address;
    int64_t timeout;
    uint count;
    std::string id;
    std::string replyto;
    string_vector properties;
    string_vector entries;
    std::string content;
    qpid::log::Options log;

    Options(const std::string& argv0=std::string())
        : qpid::Options("Options"),
          help(false),
          url("amqp:tcp:127.0.0.1"),
          timeout(TIME_INFINITE),
          count(1),
          log(argv0)
    {
        addOptions()
            ("broker,b", qpid::optValue(url, "URL"), "url of broker to connect to")
            ("address,a", qpid::optValue(address, "ADDRESS"), "address to drain from")
            ("timeout,t", qpid::optValue(timeout, "TIMEOUT"), "exit after the specified time")
            ("count,c", qpid::optValue(count, "COUNT"), "stop after count messages have been sent, zero disables")
            ("id,i", qpid::optValue(id, "ID"), "use the supplied id instead of generating one")
            ("reply-to", qpid::optValue(replyto, "REPLY-TO"), "specify reply-to address")
            ("property,P", qpid::optValue(properties, "NAME=VALUE"), "specify message property")
            ("map,M", qpid::optValue(entries, "NAME=VALUE"), "specify entry for map content")
            ("content", qpid::optValue(content, "CONTENT"), "specify textual content")
            ("help", qpid::optValue(help), "print this usage statement");
        add(log);
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

    static bool nameval(const std::string& in, std::string& name, std::string& value)
    {
        std::string::size_type i = in.find("=");
        if (i == std::string::npos) {
            name = in;
            return false;
        } else {
            name = in.substr(0, i);
            if (i+1 < in.size()) {
                value = in.substr(i+1);
                return true;
            } else {
                return false;
            }
        }
    }

    static void setProperty(Message& message, const std::string& property)
    {
        std::string name;
        std::string value;
        if (nameval(property, name, value)) {
            message.getHeaders()[name] = value;
        } else {
            message.getHeaders()[name] = Variant();
        }    
    }

    void setProperties(Message& message) const
    {
        for (string_vector::const_iterator i = properties.begin(); i != properties.end(); ++i) {
            setProperty(message, *i);
        }
    }

    void setEntries(MapContent& content) const
    {
        for (string_vector::const_iterator i = entries.begin(); i != entries.end(); ++i) {
            std::string name;
            std::string value;
            if (nameval(*i, name, value)) {
                content[name] = value;
            } else {
                content[name] = Variant();
            }
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
            Sender sender = session.createSender(options.address);

            Message message;
            options.setProperties(message);
            if (options.entries.size()) {
                MapContent content(message);
                options.setEntries(content);
                content.encode();
            } else if (options.content.size()) {
                message.setContent(options.content);
                message.setContentType("text/plain; charset=utf8");
            }
            AbsTime end(now(), options.timeout);
            for (uint count = 0; (count < options.count || options.count == 0) && end > now(); count++) {
                if (!options.replyto.empty()) message.setReplyTo(Address(options.replyto));
                std::string id = options.id.empty() ? Uuid(true).str() : options.id;
                message.getHeaders()["spout-id"] = (boost::format("%1%:%2%") % id % count).str();
                sender.send(message);
            }
            connection.close();
            return 0;
        } catch(const std::exception& error) {
            std::cout << error.what() << std::endl;
        }
    }
    return 1;
}


