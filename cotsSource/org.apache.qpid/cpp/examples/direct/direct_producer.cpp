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
 *  direct_producer.cpp:
 *
 *  This program is one of three programs designed to be used
 *  together. 
 *  
 *    create_queues.cpp:
 *
 *      Creates a queue named "message_queue" on a broker, binding the
 *      queue to the "amq.direct" exchange, using the routing key
 *      "routing_key".
 *
 *    direct_producer.cpp (this program):
 *
 *      Publishes to the "amq.direct" exchange, specifying the routing
 *      key "routing_key"
 *
 *    listener.cpp
 *
 *      Reads  from the "message_queue"  queue on  the broker  using a
 *      message listener.
 *
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
    int count = argc>3 ? atoi(argv[3]) : 10;
  
    Connection connection;
    try {
        connection.open(host, port);
        Session session =  connection.newSession();

  //--------- Main body of program --------------------------------------------

	// The routing key is a message property. We will use the same
	// routing key for each message, so we'll set this property
	// just once. (In most simple cases, there is no need to set
	// other message properties.)

    	Message message;
	message.getDeliveryProperties().setRoutingKey("routing_key"); 

	// Now send some messages ...

	for (int i=0; i<count; i++) {
	  stringstream message_data;
	  message_data << "Message " << i;

	  message.setData(message_data.str());
          session.messageTransfer(arg::content=message,  arg::destination="amq.direct");
	}
	
	// And send a final message to indicate termination.

	message.setData("That's all, folks!");
        session.messageTransfer(arg::content=message,  arg::destination="amq.direct"); 

  //-----------------------------------------------------------------------------

        connection.close();
        return 0;
    } catch(const std::exception& error) {
        std::cout << error.what() << std::endl;
    }
    return 1;
}


