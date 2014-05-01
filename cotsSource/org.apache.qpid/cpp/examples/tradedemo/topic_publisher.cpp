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
#include <qpid/client/AsyncSession.h>
#include <qpid/client/Message.h>
#include "qpid/client/QueueOptions.h"


#include <stdlib.h>
#include <cstdlib>
#include <iostream>
#include <set>
#include <sstream>

using namespace qpid::client;
using namespace qpid::framing;

using std::stringstream;
using std::string;

class Publisher {
  private:
    Session& session;
    int ttl_time;
    unsigned long seq;

    unsigned short high_[6];
    unsigned short low_[6];
    unsigned long  shares_[6];
    unsigned long  volume_[6];
    QueueOptions args;

  public:
    Publisher( Session& session, 
	       const int ttl_time,
	       const unsigned long shares[6]);

    virtual void publish_ticker(const std::string queue, unsigned short& curr_price);
    virtual void publish_market(const std::string queue, unsigned short& curr_price, int i);
    virtual ~Publisher() { };
};

Publisher::Publisher(Session& session, int ttl_time, const unsigned long shares[6]) : 
        session(session),
        ttl_time(ttl_time),
	seq(0)
{
  for (unsigned short i=0; i < 6; i++) {
    high_[i] = 0;
    low_[i] = 9999;
    volume_[i] = 0;
    shares_[i] = shares[i];
  }
}


void Publisher::publish_ticker(const std::string symbol, unsigned short& curr_price)
{
  Message message;

  // Set the routing key once, we'll use the same routing key for all
  // messages.

  std::string routing_key = "TICKER." + symbol;
  std::cout << "Setting routing key:" << routing_key << std::endl;
  message.getDeliveryProperties().setRoutingKey(routing_key); 

  // Randomally generate some price flucuations
  bool mvmnt;
  unsigned short change = rand() % 3;
  if (rand() % 2 == 0)
  {
    mvmnt = true;
    curr_price += change;
  }
  else
  {
    mvmnt = false;
    curr_price = (curr_price - change)>0 ? (curr_price - change) : 0;
  }

  // Was there change in price or no change ?
  std::string movement;
  if (!change) 
  {
    movement = "] [--]";
  } else 
  {
    movement = (mvmnt ? "] [UP]" : "] [DOWN]");
  }

  stringstream ticker_data;
  // Build up the ticker info  
  ticker_data << "[TICKER] " << "Symbol:" << symbol << "   \tPrice[" << curr_price << "] \t[" 
	      << change << movement;

  message.setData(ticker_data.str());
  // Set TTL value so that message will timeout after a period and be purged from queues
  message.getDeliveryProperties().setTtl(ttl_time);
  // Asynchronous transfer sends messages as quickly as
  // possible without waiting for confirmation.
  async(session).messageTransfer(arg::content=message, arg::destination="amq.topic");

}

void Publisher::publish_market(const std::string symbol, unsigned short& curr_price, int i)
{
  Message message;

  // Set the routing key 
  std::string routing_key = "MRKT." + symbol;
  std::cout << "Setting routing key:" << routing_key << std::endl;
  message.getDeliveryProperties().setRoutingKey(routing_key);

  // Calculate the market data low/hi change, vol, market cap etc.
  if (curr_price < low_[i] || low_[i] == 0)
  {
    low_[i] = curr_price;
  } 
  else if (curr_price > high_[i] || high_[i] == 9999)
  {
    high_[i] = curr_price;
  }

  volume_[i] += rand() % 1000;  // increase the daily volume tracker
  int mkt_cap = shares_[i] * curr_price; // calculate new market cap based on current price
 
  stringstream market_data;
  // Build up the ticker info  
  market_data << "[MARKET] " << "Symbol:" << symbol << "\tVolume: " << volume_[i] 
	      << "\tHi:" << high_[i] << "\tLo:" << low_[i] << "\tMktCap:" 
	      << mkt_cap <<"M\tSEQ[" << seq << "]";

  message.setData(market_data.str());

  std::string key;
  args.getLVQKey(key);
  message.getHeaders().setString(key, symbol);
  
  // Asynchronous transfer sends messages as quickly as
  // possible without waiting for confirmation.
  async(session).messageTransfer(arg::content=message, arg::destination="amq.topic");
  seq++;  // This sequence number is really just to demonstrate the LVQ nature of the queue.
          // You will notice some messages don't show because they are overwritten by last value.

}


int main(int argc, char** argv) {
    unsigned int pub_cycles = argc>1 ? atoi(argv[1]) : 100;
    unsigned int ttl_time = argc>2 ? atoi(argv[2]) : 4000;
    const char* host = argc>3 ? argv[3] : "127.0.0.1";
    int port = argc>4 ? atoi(argv[4]) : 5672;
    std::cout <<"Usage: topic_publisher <pub cycles> <TTL-timeout> <host name/IP> <port>" << std::endl;
    std::cout <<"\tparameters are optional but must be in this order when used." << std::endl;

    // Set up the stocks symbols and their prices
    std::string symbol[6];
    unsigned short price[6];
    symbol[0] = "NYSE.RHT";    // Red Hat
    symbol[1] = "NYSE.IBM";    // IBM Corp.
    symbol[2] = "NASDAQ.MSFT"; // Microsoft
    symbol[3] = "NASDAQ.CSCO"; // Cisco Systems
    symbol[4] = "NASDAQ.YHOO"; // Yahoo
    symbol[5] = "NASDAQ.GOOG"; // Google

    // Rough starting values.
    price[0] = rand() % 30 +1;
    price[1] = rand() % 120 +1;
    price[2] = rand() % 20 +1;
    price[3] = rand() % 75 +1;
    price[4] = rand() % 10 +1;
    price[5] = rand() % 323 +1;

    // Shares oustanding in millions.
    unsigned long  shares[6] = {190,1340,8890, 5860, 1390, 314}; 

    
    Connection connection;
    try {
        connection.open(host, port);
        Session session =  connection.newSession();

	Publisher theFeed(session,ttl_time, shares);

  //--------- Main body of program --------------------------------------------

	// Print the opening values for each symbol
	std::cout << std::endl << "Opening values:" << std::endl;
	for (int i=0; i < 6; i++)
	{
	  std::cout << symbol[i] << ":" << price[i] << std::endl;
	}

	// For the duration of the publishing cycles publish 
	// ticker and market data for each symbol
	for (unsigned int j=0; j<pub_cycles; j++)
	{
	  for (unsigned int i=0; i < 6; i++)
	  {
	    // for each symbol publish the ticker and the market data
	    theFeed.publish_ticker(symbol[i], price[i]);
	    theFeed.publish_market(symbol[i], price[i], i);
	  }
	}


  //-----------------------------------------------------------------------------

        connection.close();
        return 0;
    } catch(const std::exception& error) {
        std::cout << error.what() << std::endl;
    }
    return 1;
}


