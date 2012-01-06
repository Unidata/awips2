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
 *  fanout_producer.cpp:
 *
 *  This program is one of two programs designed to be used
 *  together.
 *
 *    fanout_producer.cpp (this program):
 *
 *      Publishes messages to the "amq.fanout" exchange.
 *
 *    listener.cpp
 *
 *      Creates a private queue, binds it to the "amq.fanout"
 *      exchange, and reads messages from its queue as they
 *      arrive. Messages sent before the listener binds the queue are
 *      not received.
 *
 *      Multiple listeners can run at the same time.
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

    Connection connection;
    try {
        connection.open(host, port);
        Session session =  connection.newSession();

  //--------- Main body of program --------------------------------------------

	// Unlike topic exchanges and direct exchanges, a fanout
	// exchange need not set a routing key. 

	Message message;

	// Now send some messages ...

	for (int i=0; i<10; i++) {
	  stringstream message_data;
	  message_data << "Message " << i;

	  message.setData(message_data.str());
          // Asynchronous transfer sends messages as quickly as
          // possible without waiting for confirmation.
          async(session).messageTransfer(arg::content=message, arg::destination="amq.fanout");
	}
	
	// And send a final message to indicate termination.

	message.setData("That's all, folks!");
        session.messageTransfer(arg::content=message, arg::destination="amq.fanout");

  //-----------------------------------------------------------------------------

        connection.close();
        return 0;
    } catch(const std::exception& error) {
        std::cout << error.what() << std::endl;
    }
    return 1;
}


