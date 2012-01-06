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
 * xml_producer.cpp
 *
 * This is one of three programs used to implement XML-based content
 * routing in C++.
 *
 * declare_queues.cpp 
 *
 *       Creates a queue named "message_qaueue" on the broker,
 *       declares an XML Exchange, subscribes the queue to the XML
 *       Exchange using an XQuery in the binding, then exits.
 *
 * xml_producer.cpp (this program)
 *
 *       Publishes messages to the XML Exchange.
 *
 * listener.cpp
 *
 *       Reads messages from the "message_queue" queue.
 */


#include <qpid/client/Connection.h>
#include <qpid/client/Session.h>
#include <qpid/client/AsyncSession.h>
#include <qpid/client/Message.h>


#include <cstdlib>
#include <iostream>

#include <sstream>

using namespace qpid::client;
using namespace qpid::framing;

using std::stringstream;
using std::string;

int main(int argc, char** argv) {
    const char* host = argc>1 ? argv[1] : "127.0.0.1";
    int port = argc>2 ? atoi(argv[2]) : 5672;
    Connection connection;
    Message message;
    try {
        connection.open(host, port);
        Session session =  connection.newSession();

  //--------- Main body of program --------------------------------------------

	// Publish some XML messages. Use the control property to
	// indicate when we are finished.
	//
	// In the XML exchange, the routing key and the name of
	// the query match.

	message.getDeliveryProperties().setRoutingKey("content_feed"); 
	message.getHeaders().setString("control","continue");

	// Now send some messages ...

	for (int i=0; i<10; i++) {
	  stringstream message_data;
	  message_data << "<message><id>" << i << "</id></message>";

	  std::cout << "Message data: " << message_data.str() << std::endl;

	  message.setData(message_data.str());
          // Asynchronous transfer sends messages as quickly as
          // possible without waiting for confirmation.
          async(session).messageTransfer(arg::content=message,  arg::destination="xml");
	}
	
	// And send a final message to indicate termination.

	message.getHeaders().setString("control","end");
	message.setData("<end>That's all, folks!</end>");
        session.messageTransfer(arg::content=message,  arg::destination="xml"); 

  //-----------------------------------------------------------------------------

        connection.close();
        return 0;
    } catch(const std::exception& error) {
        std::cout << error.what() << std::endl;
    }
    return 1;
}


