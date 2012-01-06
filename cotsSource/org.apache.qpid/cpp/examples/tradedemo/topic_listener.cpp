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
 *  topic_publisher.cpp:
 *
 *  This program is one of three programs designed to be used
 *  together. These programs implement a publish-subscribe example
 *  using the "amq.topic" exchange. In the example multiple listeners
 *  can subscribe to the same queues for TTL messages.  
 *  The TTL messages are all ticker price data. Messages are 
 *  browsed and therefore shared among the multiple listeners. 
 *  Messages timeout using TTL so that they don't stay in the queue 
 *  for too long and fill it up.  
 *  Local exclusive LVQ are also declared for market data.
 *
 *   declare_queues.cpp 
 *
 *     Declares several non-exclusive queues bound to the amq:topic exchange
 *
 *   topic_publisher.cpp 
 *
 *      Sends messages to the "amq.topic" exchange, using the
 *      multipart routing keys for ticker price and market data
 *      Ticker messages are sent using a TTL value.
 *
 *   topic_listener.cpp (this program)
 *
 *      Subscribes to non-exclusive queues in NOT_ACQUIRE mode for
 *      ticker price data and declares two LVQs for market data.
 *
 *      Multiple listeners can be run at the same time.
 *
 */

#include <qpid/client/Connection.h>
#include <qpid/client/Session.h>
#include <qpid/client/Message.h>
#include <qpid/client/MessageListener.h>
#include <qpid/client/SubscriptionManager.h>
#include "qpid/client/QueueOptions.h"

#include <cstdlib>
#include <iostream>

using namespace qpid::client;
using namespace qpid::framing;


class Listener : public MessageListener {
  private:
    Session& session;
    SubscriptionManager subscriptions;
  public:
    Listener(Session& session);
    virtual void subscribeTTLQueue(std::string queue);
    virtual void subscribeLVQQueue(std::string queue);
    virtual void received(Message& message);
    virtual void listen();
    ~Listener() { };
};


/*
 *  Listener::Listener
 *
 *  Subscribe to the queue, route it to a client destination for the
 *  listener. (The destination name merely identifies the destination
 *  in the listener, you can use any name as long as you use the same
 *  name for the listener).
 */

Listener::Listener(Session& session) : 
        session(session),
        subscriptions(session)
{
}


void Listener::subscribeTTLQueue(std::string queue) {

    /*
     * Subscribe to the queue using the subscription manager.
     * The queues were declared elsewhere alog with their bindings. 
     */

    std::cout << "Subscribing to queue " << queue << std::endl;
    subscriptions.subscribe(*this, queue);
    // Will not acquire messages but instead browse them.
    subscriptions.setAcquireMode(message::ACQUIRE_MODE_NOT_ACQUIRED);
}

void Listener::subscribeLVQQueue(std::string queue) {

    /*
     * Declare and subscribe to the queue using the subscription manager.
     */

  QueueOptions qo;
  qo.setOrdering(LVQ);
  std::string binding = queue + ".#";
  queue += session.getId().getName();
  session.queueDeclare(arg::queue=queue, arg::exclusive=true, arg::arguments=qo);
  session.exchangeBind(arg::exchange="amq.topic", arg::queue=queue, arg::bindingKey=binding);
  std::cout << "Declared queue " << queue <<  " non-exclusive with amq:topic binding " << binding << std::endl; 
  std::cout << "Subscribing to queue " << queue << std::endl;
  subscriptions.subscribe(*this, queue, SubscriptionSettings(FlowControl::unlimited(), ACCEPT_MODE_NONE));    

}

void Listener::received(Message& message) {
  // If you want to see the destination you can swap the following lines.
  //  std::cout << message.getDestination() << "\t" << message.getData() << std::endl; 
  std::cout << message.getData() << std::endl;

}

void Listener::listen() {
    // Receive messages
    subscriptions.run();
}

int main(int argc, char** argv) {
    const char* host = argc>1 ? argv[1] : "127.0.0.1";
    int port = argc>2 ? atoi(argv[2]) : 5672;

    Connection connection;
    try {
        connection.open(host, port);
        Session session =  connection.newSession();

        //--------- Main body of program --------------------------------------------



	// Create a listener for the session

        Listener listener(session);

        // Subscribe to messages on the queues we are interested in

	  listener.subscribeTTLQueue("TICKER.NASDAQ");
	  listener.subscribeTTLQueue("TICKER.NYSE");
	  
	  listener.subscribeLVQQueue("MRKT.NASDAQ");
	  listener.subscribeLVQQueue("MRKT.NYSE");

        std::cout << "Starting Listener <Ctrl>-C to exit." << std::endl;
        std::cout << "Listening for messages ..." << std::endl;

        // Give up control and receive messages
        listener.listen();


        //-----------------------------------------------------------------------------

        connection.close();
        return 0;
    } catch(const std::exception& error) {
        std::cout << error.what() << std::endl;
    }
    return 1;   
}


