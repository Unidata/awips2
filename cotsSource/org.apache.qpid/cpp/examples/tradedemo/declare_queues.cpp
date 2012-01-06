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


using namespace qpid::client;
using namespace qpid::framing;


int main(int argc, char** argv) {
    const char* host = argc>1 ? argv[1] : "127.0.0.1";
    int port = argc>2 ? atoi(argv[2]) : 5672;
    Connection connection;

    try {
      connection.open(host, port);
      Session session =  connection.newSession();


  //--------- Main body of program --------------------------------------------

      // Create a queue named "message_queue", and route all messages whose
      // routing key is "routing_key" to this newly created queue.

      session.queueDeclare(arg::queue="TICKER.NYSE", arg::exclusive=false);
      session.exchangeBind(arg::exchange="amq.topic", arg::queue="TICKER.NYSE", arg::bindingKey="TICKER.NYSE.#");
      std::cout << "Declared queue Ticker NYSE  non-exclusive with amq:topic binding TICKER.NYSE.#" << std::endl; 
      session.queueDeclare(arg::queue="TICKER.NASDAQ", arg::exclusive=false);
      session.exchangeBind(arg::exchange="amq.topic", arg::queue="TICKER.NASDAQ", arg::bindingKey="TICKER.NASDAQ.#");
      std::cout << "Declared queue Ticker NASDAQ  non-exclusive with amq:topic binding TICKER.NASDAQ.#" << std::endl; 


  //-----------------------------------------------------------------------------

      connection.close();
      return 0;
    } catch(const std::exception& error) {
        std::cout << error.what() << std::endl;
    }
    return 1;
   
}



