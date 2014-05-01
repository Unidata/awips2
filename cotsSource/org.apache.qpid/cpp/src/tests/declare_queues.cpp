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

#include <qpid/client/FailoverManager.h>
#include <qpid/client/Session.h>
#include <qpid/sys/Time.h>
#include <qpid/Exception.h>

#include <cstdlib>
#include <iostream>
#include <sstream>

using namespace qpid::client;

using namespace std;

int 
main(int argc, char ** argv) 
{
    ConnectionSettings settings;
    if ( argc != 6 )
    {
      cerr << "Usage: declare_queues host port durability queue_name_prefix n_queues\n";
      return 1;
    }

    settings.host = argv[1];
    settings.port = atoi(argv[2]);
    int durability = atoi(argv[3]);
    char const * queue_name_prefix = argv[4];
    int n_queues = atoi(argv[5]);
    
    FailoverManager connection(settings);

    int max_fail = 13;
    for ( int i = 0; i < n_queues; ++ i ) {
        stringstream queue_name;
        queue_name << queue_name_prefix << '_' << i;

        bool queue_created = false;
        int failure_count;

        // Any non-transport failure is Bad.
        try
        {
            while ( ! queue_created ) {
                Session session = connection.connect().newSession();
                // TransportFailures aren't too bad -- they might happen because
                // we are doing a cluster failover test.  But if we get too many,
                // we will still bug out.
                failure_count = 0;
                try {
                    if ( durability )
                        session.queueDeclare(arg::queue=queue_name.str(), arg::durable=true);
                    else
                        session.queueDeclare(arg::queue=queue_name.str());
                    queue_created = true;
                    cout << "declare_queues: Created queue " << queue_name.str() << endl;
                }
                catch ( const qpid::TransportFailure& ) {
                  if ( ++ failure_count >= max_fail ) {
                      cerr << "declare_queues failed: too many transport errors.\n";
                      cerr << "  host: " << settings.host
                           << "  port: " << settings.port << endl;
                      return 1;
                  }
                  qpid::sys::sleep ( 1 );
                }
            }
       }
       catch ( const exception & error) {
           cerr << "declare_queues failed:" << error.what() << endl;
           cerr << "  host: " << settings.host
                << "  port: " << settings.port << endl;
           return 1;
       }
    }
}





