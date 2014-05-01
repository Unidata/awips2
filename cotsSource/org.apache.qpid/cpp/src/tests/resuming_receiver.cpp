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
#include <qpid/client/Message.h>
#include <qpid/client/SubscriptionManager.h>

#include <iostream>
#include <fstream>

using namespace qpid;
using namespace qpid::client;
using namespace qpid::framing;

using namespace std;



namespace qpid {
namespace tests {

class Listener : public MessageListener,
                 public FailoverManager::Command,
                 public FailoverManager::ReconnectionStrategy
{
  public:
    Listener ( int report_frequency = 1000, 
               int verbosity = 0, 
               char const * queue_name = "message_queue" );
    void received(Message& message);
    void execute(AsyncSession& session, bool isRetry);
    void check();
    void editUrlList(vector<Url>& urls);
  private:
    Subscription subscription;
    uint count;
    vector<int> received_twice;
    uint lastSn;
    bool gaps;
    uint  reportFrequency;
    int  verbosity;
    bool done;
    string queueName;
};


Listener::Listener ( int freq, int verbosity, char const * name )
  : count(0),
    lastSn(0),
    gaps(false),
    reportFrequency(freq),
    verbosity(verbosity),
    done(false),
    queueName ( name )
{}


void Listener::received(Message & message)
{
    if (message.getData() == "That's all, folks!")
    {
        done = true;
        if(verbosity > 0 )
        {
            cout << "Shutting down listener for "
                      << message.getDestination() << endl;

            cout << "Listener received "
                      << count
                      << " messages ("
                      << received_twice.size()
                      << " received_twice)"
                      << endl;
            
        }
        subscription.cancel();
        if ( verbosity > 0 )
          cout << "LISTENER COMPLETED\n";
        
        if ( ! gaps ) {
          cout << "no gaps were detected\n";
          cout << received_twice.size() << " messages were received twice.\n";
        }
        else {
            cout << "gaps detected\n";
            for ( unsigned int i = 0; i < received_twice.size(); ++ i )
              cout << "received_twice "
                        << received_twice[i]
                        << endl;
        }
    } else {
        uint sn = message.getHeaders().getAsInt("sn");
        if (lastSn < sn) {
            if (sn - lastSn > 1) {
                cerr << "Error: gap in sequence between " << lastSn << " and " << sn << endl;
                gaps = true;
            }
            lastSn = sn;
            ++count;
            if ( ! ( count % reportFrequency ) ) {
                if ( verbosity > 0 )
                    cout << "Listener has received "
                              << count
                              << " messages on queue "
                              << queueName
                              << endl;
            }
        } else {
            received_twice.push_back ( sn );
        }
    }
}

void Listener::check()
{
    if (gaps) throw Exception("Detected gaps in sequence; messages appear to have been lost.");
}

void Listener::execute(AsyncSession& session, bool isRetry) {
    if (verbosity > 0)
        cout << "resuming_receiver " << (isRetry ? "first " : "re-") << "connect." << endl;
    if (!done) {
        SubscriptionManager subs(session);
        subscription = subs.subscribe(*this, queueName);
        subs.run();
    }
}

void Listener::editUrlList(vector<Url>& urls)
{
    /**
     * A more realistic algorithm would be to search through the list
     * for prefered hosts and ensure they come first in the list.
     */
    if (urls.size() > 1) rotate(urls.begin(), urls.begin() + 1, urls.end());
}

}} // namespace qpid::tests

using namespace qpid::tests;

int main(int argc, char ** argv)
{
    ConnectionSettings settings;

    if ( argc != 6 )
    {
      cerr << "Usage: resuming_receiver host port report_frequency verbosity queue_name\n";
      return 1;
    }

    settings.host       = argv[1];
    settings.port       = atoi(argv[2]);
    int reportFrequency = atoi(argv[3]);
    int verbosity       = atoi(argv[4]);
    char * queue_name   = argv[5];

    Listener listener ( reportFrequency, verbosity, queue_name );
    FailoverManager connection(settings, &listener);

    try {
        connection.execute(listener);
        connection.close();
        listener.check();
        return 0;
    } catch(const exception& error) {
        cerr << "Receiver failed: " << error.what() << endl;
    }
    return 1;
}



