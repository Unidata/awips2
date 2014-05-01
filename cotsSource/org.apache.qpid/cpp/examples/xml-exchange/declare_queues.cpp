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
 *
 * declare_queues.cpp
 *
 * This is one of three programs used to implement XML-based content
 * routing in C++.
 *
 * declare_queues.cpp (this program)
 *
 *       Creates a queue named "message_qaueue" on the broker,
 *       declares an XML Exchange, subscribes the queue to the XML
 *       Exchange using an XQuery in the binding, then exits.
 *
 * xml_producer.cpp 
 *
 *       Publishes messages to the XML Exchange.
 *
 * listener.cpp
 *
 *       Reads messages from the "message_queue" queue.
 */

#include <qpid/client/Connection.h>
#include <qpid/client/Session.h>

#include <cstdlib>
#include <iostream>

using namespace qpid::client;
using namespace qpid::framing;

using std::string;


int main(int argc, char** argv) {
    const char* host = argc>1 ? argv[1] : "127.0.0.1";
    int port = argc>2 ? atoi(argv[2]) : 5672;
    Connection connection;

    try {
      connection.open(host, port);
      Session session =  connection.newSession();


  //--------- Main body of program --------------------------------------------

      // Set up queues, bind them with queries. Note that the XML exchange
      // is not in the AMQP specification, so it is called "xml", not "amq.xml".
      // Note that the XML exchange is not predeclared in Qpid, it must
      // be declared by the application.

      session.queueDeclare(arg::queue="message_queue");
      session.exchangeDeclare(arg::exchange="xml", arg::type="xml");

      // Application message properties are mapped to external variables
      // in the XQuery. An XML Exchange can query message properties much
      // like JMS, query the XML content of the message, or both.

      FieldTable binding;
      binding.setString("xquery", "declare variable $control external;"
				  "./message/id mod 2 = 1 or $control = 'end'");
      session.exchangeBind(arg::exchange="xml", arg::queue="message_queue", arg::bindingKey="content_feed", arg::arguments=binding); 

  //-----------------------------------------------------------------------------

      connection.close();
      return 0;
    } catch(const std::exception& error) {
        std::cout << error.what() << std::endl;
    }
    return 1;
   
}



