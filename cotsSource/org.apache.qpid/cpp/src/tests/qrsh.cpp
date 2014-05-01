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

#include <qpid/client/Connection.h>
#include <qpid/client/Session.h>
#include <qpid/client/AsyncSession.h>
#include <qpid/client/Message.h>
#include <qpid/client/MessageListener.h>
#include <qpid/client/SubscriptionManager.h>

#include <stdio.h>
#include <cstdlib>
#include <iostream>

#include <sstream>

using namespace qpid::client;
using namespace qpid::framing;

using namespace std;

namespace qpid {
namespace tests {

class ResponseListener : public MessageListener
{
    public :

    int exitCode;

    ResponseListener ( SubscriptionManager & subscriptions )
        : exitCode(-1),
          subscriptions ( subscriptions )
    {
    }

    virtual void
    received ( Message & message )
    {
        char first_word[1000];
        sscanf ( message.getData().c_str(), "%s", first_word );

        if ( ! strcmp ( first_word, "wait_response" ) )
        {
            // If we receive a message here, parse out the exit code.
            sscanf ( message.getData().c_str(), "%*s%d", & exitCode );
            subscriptions.cancel(message.getDestination());
        }
        else
        if ( ! strcmp ( first_word, "get_response" ) )
        {
            // The remainder of the message is the file we requested.
            fprintf ( stdout,
                      "%s",
                      message.getData().c_str() + strlen("get_response" )
                    );
            subscriptions.cancel(message.getDestination());
        }
    }


    private :

    SubscriptionManager & subscriptions;
};

}} // namespace qpid::tests

using namespace qpid::tests;

/*
 *  argv[1] host
 *  argv[2] port
 *  argv[3] server name
 *  argv[4] command name
 *  argv[5..N] args to the command
 */
int
main ( int argc, char ** argv )
{
    const char* host = argv[1];
    int port = atoi(argv[2]);


    Connection connection;

    try
    {
        connection.open ( host, port );
        Session session = connection.newSession ( );

        // Make a queue and bind it to fanout.
        string myQueue = session.getId().getName();

        session.queueDeclare ( arg::queue=myQueue,
                               arg::exclusive=true,
                               arg::autoDelete=true
                             );

        session.exchangeBind ( arg::exchange="amq.fanout",
                               arg::queue=myQueue,
                               arg::bindingKey="my-key"
                             );

        // Get ready to listen for the wait-response.
        // or maybe a get-response.
        // ( Although this may not be one of those types
        // of command, get ready anyway.
        SubscriptionManager subscriptions ( session );
        ResponseListener responseListener ( subscriptions );
        subscriptions.subscribe ( responseListener, myQueue );

        bool response_command = false;
        if(! strcmp("exec_wait", argv[4] ))
            response_command = true;
        else
        if(! strcmp("exited", argv[4] ))
            response_command = true;
        else
        if(! strcmp("get", argv[4] ))
            response_command = true;

        // Send the payload message.
        // Skip "qrsh host_name port"
        Message message;
        stringstream ss;
        for ( int i = 3; i < argc; ++ i )
            ss << argv[i] << ' ';

        message.setData ( ss.str() );

        session.messageTransfer(arg::content=message,
                                arg::destination="amq.fanout");

        if ( response_command )
            subscriptions.run();

        session.close();
        connection.close();
        return responseListener.exitCode;
    }
    catch ( exception const & e)
    {
        cerr << e.what() << endl;
    }

    return 1;
}



