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
 *  topic_listener.cpp:
 *
 *  This program is one of two programs designed to be used
 *  together. These programs implement a publish-subscribe example
 *  using the "amq.topic" exchange.
 *
 *   topic_publisher.cpp 
 *
 *      Sends messages to the "amq.topic" exchange, using the
 *      multipart routing keys "usa.news", "usa.weather",
 *      "europe.news", and "europe.weather".
 *
 *   topic_listener.cpp (this program)
 *
 *      Creates private queues for "news", "weather", "usa", and
 *      "europe", binding them to the amq.topic exchange using
 *      bindings that match the corresponding parts of the multipart
 *      routing keys.
 *
 *      Multiple listeners can be run at the same time.
 *
 */

#include <qpid/client/Connection.h>
#include <qpid/client/Session.h>
#include <qpid/client/Message.h>
#include <qpid/client/MessageListener.h>
#include <qpid/client/SubscriptionManager.h>

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
    virtual void prepareQueue(std::string queue, std::string exchange, std::string routing_key);
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


void Listener::prepareQueue(std::string queue, std::string exchange, std::string routing_key) {

    /* Create a unique queue name for this consumer by concatenating
     * the queue name parameter with the Session ID.
     */

    queue += session.getId().getName();
    std::cout << "Declaring queue: " << queue <<  std::endl;
   
    /* Declare an exclusive queue on the broker
     */

    session.queueDeclare(arg::queue=queue, arg::exclusive=true, arg::autoDelete=true);

    /* Route messages to the new queue if they match the routing key.
     *
     * Also route any messages to with the "control" routing key to
     * this queue so we know when it's time to stop. A publisher sends
     * a message with the content "That's all, Folks!", using the
     * "control" routing key, when it is finished.
     */

    session.exchangeBind(arg::exchange=exchange, arg::queue=queue, arg::bindingKey=routing_key);
    session.exchangeBind(arg::exchange=exchange, arg::queue=queue, arg::bindingKey="control");

    /*
     * subscribe to the queue using the subscription manager.
     */

    std::cout << "Subscribing to queue " << queue << std::endl;
    subscriptions.subscribe(*this, queue);
}

void Listener::received(Message& message) {
    std::cout << "Message: " << message.getData() << " from " << message.getDestination() << std::endl;

    if (message.getData() == "That's all, folks!") {
        std::cout << "Shutting down listener for " << message.getDestination() << std::endl;
        subscriptions.cancel(message.getDestination());
    }
}

void Listener::listen() {
    // Receive messages
    subscriptions.run();
}

int main(int argc, char** argv) {
    const char* host = argc>1 ? argv[1] : "127.0.0.1";
    int port = argc>2 ? atoi(argv[2]) : 5672;
    std::string exchange = argc>3 ? argv[3] : "amq.topic";
    Connection connection;
    try {
        connection.open(host, port);
        Session session =  connection.newSession();

        //--------- Main body of program --------------------------------------------

	// Create a listener for the session

        Listener listener(session);

        // Subscribe to messages on the queues we are interested in

        listener.prepareQueue("usa", exchange, "usa.#");
        listener.prepareQueue("europe", exchange, "europe.#");
        listener.prepareQueue("news", exchange, "#.news");
        listener.prepareQueue("weather", exchange, "#.weather");

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


