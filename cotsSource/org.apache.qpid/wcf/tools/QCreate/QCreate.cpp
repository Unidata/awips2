/*
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
*/

#include "stdafx.h"

#include <qpid/client/Connection.h>
#include <qpid/client/Session.h>

#include <cstdlib>
#include <iostream>

using namespace qpid::client;
using namespace qpid::framing;


int main(int argc, char** argv) {

	std::string exchange = argc>1 ? argv[1] : "amq.direct";
    std::string bindingKey = argc>2 ? argv[2] : "routing_key";
	std::string queue = argc>3 ? argv[3] : "message_queue";

	const char* host = "127.0.0.1";
    int port = 5672;
    Connection connection;

    try {
      connection.open(host, port);
      Session session =  connection.newSession();


  //--------- Main body of program --------------------------------------------

      // Create a queue and route all messages whose
      // routing key is "routing_key" to this newly created queue.

      session.queueDeclare(arg::queue=queue);
      session.exchangeBind(arg::exchange=exchange, arg::queue=queue, arg::bindingKey=bindingKey);

  //-----------------------------------------------------------------------------

      connection.close();
      return 0;
    } catch(const std::exception& error) {
        std::cout << error.what() << std::endl;
    }
    return 1;
   
}

