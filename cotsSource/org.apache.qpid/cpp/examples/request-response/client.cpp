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
 *  client.cpp
 *
 *  This program is one of two programs that illustrate the
 *  request/response pattern.
 *  
 *
 *  client.cpp  (this program) 
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
 *  server.cpp
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
#include <qpid/client/Message.h>
#include <qpid/client/MessageListener.h>
#include <qpid/client/SubscriptionManager.h>

#include <cstdlib>
#include <iostream>

#include <sstream>

using namespace qpid::client;
using namespace qpid::framing;

using std::stringstream;
using std::string;

class Listener : public MessageListener{
  private:
    SubscriptionManager& subscriptions;
    int counter;
  public:
    Listener(SubscriptionManager& subscriptions);
    virtual void received(Message& message);
};

Listener::Listener(SubscriptionManager& subs) : subscriptions(subs), counter(0)
{}

void Listener::received(Message& message) {
    std::cout << "Response: " << message.getData() << std::endl;

    ++ counter;
    if (counter > 3) {
        std::cout << "Shutting down listener for " << message.getDestination() << std::endl;
        subscriptions.cancel(message.getDestination());
    }
}



int main(int argc, char** argv) {
    const char* host = argc>1 ? argv[1] : "127.0.0.1";
    int port = argc>2 ? atoi(argv[2]) : 5672;
    Connection connection;
    try {
        connection.open(host, port);
        Session session =  connection.newSession();

        //--------- Main body of program --------------------------------------------

	// Create a response queue so the server can send us responses
	// to our requests. Use the client's session ID as the name
	// of the response queue.

	stringstream response_queue;
	response_queue << "client" << session.getId().getName();

        // Use the name of the response queue as the routing key

	session.queueDeclare(arg::queue=response_queue.str());
        session.exchangeBind(arg::exchange="amq.direct", arg::queue=response_queue.str(), arg::bindingKey=response_queue.str());

	// Each client sends the name of their own response queue so
	// the service knows where to route messages.

    	Message request;
	request.getDeliveryProperties().setRoutingKey("request");
	request.getMessageProperties().setReplyTo(ReplyTo("amq.direct", response_queue.str()));

	// Create a listener for the response queue and listen for response messages.
        std::cout << "Activating response queue listener for: " << response_queue.str() << std::endl;
        SubscriptionManager subscriptions(session);
        Listener listener(subscriptions);
        subscriptions.subscribe(listener, response_queue.str());

	// Now send some requests ...

	string s[] = {
            "Twas brillig, and the slithy toves",
            "Did gire and gymble in the wabe.",
            "All mimsy were the borogroves,",
            "And the mome raths outgrabe."
        };


	for (int i=0; i<4; i++) {
            request.setData(s[i]);
            session.messageTransfer(arg::content=request, arg::destination="amq.direct");
            std::cout << "Request: " << s[i] << std::endl;
	}

        std::cout << "Waiting for all responses to arrive ..." << std::endl;
	subscriptions.run();

        //-----------------------------------------------------------------------------

        connection.close();
        return 0;
    } catch(const std::exception& error) {
        std::cout << error.what() << std::endl;
    }
    return 1;
}


