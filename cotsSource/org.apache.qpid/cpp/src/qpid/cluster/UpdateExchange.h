#ifndef QPID_CLUSTER_UPDATEEXCHANGE_H
#define QPID_CLUSTER_UPDATEEXCHANGE_H

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

#include "qpid/cluster/UpdateClient.h"
#include "qpid/broker/FanOutExchange.h"


namespace qpid {
namespace cluster {

/**
 * A keyless exchange (like fanout exchange) that does not modify
 * delivery-properties.exchange but copies it to the MessageTransfer.
 */
class UpdateExchange : public broker::FanOutExchange
{
  public:
    UpdateExchange(management::Manageable* parent);
    void setProperties(const boost::intrusive_ptr<broker::Message>&);
};

}} // namespace qpid::cluster

#endif  /*!QPID_CLUSTER_UPDATEEXCHANGE_H*/
