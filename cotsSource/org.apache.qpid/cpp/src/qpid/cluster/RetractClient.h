#ifndef QPID_CLUSTER_RETRACTCLIENT_H
#define QPID_CLUSTER_RETRACTCLIENT_H

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

#include "qpid/client/ConnectionSettings.h"
#include "qpid/sys/Runnable.h"


namespace qpid {
namespace cluster {

/**
 * A client that retracts an offer to a remote broker using AMQP. @see UpdateClient
 */
class RetractClient : public sys::Runnable {
  public:

    RetractClient(const Url&, const client::ConnectionSettings&);
    ~RetractClient();
    void run();                 // Will delete this when finished.

  private:
    Url url;
    client::ConnectionSettings connectionSettings;
};

}} // namespace qpid::cluster

#endif  /*!QPID_CLUSTER_RETRACTCLIENT_H*/
