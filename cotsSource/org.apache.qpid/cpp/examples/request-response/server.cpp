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


/**
 *  server.cpp
 *
 *  This program is one of two programs that illustrate the
 *  request/response pattern.
 *  
 *
 *  client.cpp 
 *   
 *     A client application that sends messages to the "amq.direct"
 *     exchange, using the routing key "request" to route messages to
 *     the server.
 *
 *     Each instance of the client creates its own private response
 *     queue, binding it to the "amq.direct" exchange using it's
 *     session identifier as the routing key, and places its session
 *     identifier in the "reply-to" property of each message it sends.
 *
 *
 *  server.cpp (this program)
 *
 *     A service that accepts messages from a request queue, converts
 *     their content to upper case, and sends the result to the
 *     original sender.
 *
 *     This program creates a request queue, binds it to "amq.direct"
 *     using the routing key "request", then receives messages from
 *     the request queue. Each incoming message is converted to upper
 *     case, then sent to the "amq.direct" exchange using the
 *     request's reply-to property as the routing key for the
 *     response.
 *
 *
 */


#include <qpid/client/Connection.h>
#include <qpid/client/Session.h>

#include <qpid/client/AsyncSession.h>
#include <qpid/client/Message.h>
#include <qpid/client/MessageListener.h>
#include <qpid/client/SubscriptionManager.h>

#include <cstdlib>
#include <iostream>

#include <sstream>
#include <string>

using namespace qpid::client;
using namespace qpid::framing;
using std::stringstream;
using std::string;

class Listener : public MessageListener{
  private:
    SubscriptionManager& subscriptions;
    AsyncSession asyncSession;
  public:
    Listener(SubscriptionManager& subscriptions, Session& session);
    virtual void received(Message& message);
};

Listener::Listener(SubscriptionManager& subs, Session& session)
    : subscriptions(subs), asyncSession(session)
{}

void Listener::received(Message& request) {
    Message response;

    // Get routing key for response from the request's replyTo property
    string routingKey;

    if (request.getMessageProperties().hasReplyTo()) {
        routingKey = request.getMessageProperties().getReplyTo().getRoutingKey();
    } else {
        std::cout << "Error: " << "No routing key for request (" << request.getData() << ")" << std::endl;
        return;
    } 

    std::cout << "Request: " << request.getData() << "  (" <<routingKey << ")" << std::endl;

    // Transform message content to upper case
    std::string s = request.getData();
    std::transform (s.begin(), s.end(), s.begin(), toupper);
    response.setData(s);

    // Send it back to the user
    response.getDeliveryProperties().setRoutingKey(routingKey);

    // Asynchronous transfer sends messages as quickly as
    // possible without waiting for confirmation.
    asyncSession.messageTransfer(arg::content=response, arg::destination="amq.direct");
}


int main(int argc, char** argv) {
    const char* host = argc>1 ? argv[1] : "127.0.0.1";
    int port = argc>2 ? atoi(argv[2]) : 5672;
    Connection connection;

    try {
        connection.open(host, port);
        Session session =  connection.newSession();

        //--------- Main body of program --------------------------------------------


	// Create a request queue for clients to use when making
	// requests.
	string request_queue = "request";

        // Use the name of the request queue as the routing key
	session.queueDeclare(arg::queue=request_queue);
        session.exchangeBind(arg::exchange="amq.direct", arg::queue=request_queue, arg::bindingKey=request_queue);

        // Create a listener and subscribe it to the request_queue
        std::cout << "Activating request queue listener for: " << request_queue << std::endl;
        SubscriptionManager subscriptions(session);
        Listener listener(subscriptions, session);
        subscriptions.subscribe(listener, request_queue);
        // Deliver messages until the subscription is cancelled
        // by Listener::received()

        std::cout << "Waiting for requests" << std::endl;
        subscriptions.run();

        //-----------------------------------------------------------------------------

        connection.close();
        return 0;
    } catch(const std::exception& error) {
        std::cout << error.what() << std::endl;
    }
    return 1;
}


