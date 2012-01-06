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
#ifndef _DeliveryAdapter_
#define _DeliveryAdapter_

#include "qpid/broker/DeliveryId.h"
#include "qpid/broker/Message.h"
#include "qpid/framing/amqp_types.h"

namespace qpid {
namespace broker {

class DeliveryRecord;

/**
 * The intention behind this interface is to separate the generic
 * handling of some form of message delivery to clients that is
 * contained in the version independent Channel class from the
 * details required for a particular situation or
 * version. i.e. where the existing adapters allow (through
 * supporting the generated interface for a version of the
 * protocol) inputs of a channel to be adapted to the version
 * independent part, this does the same for the outputs.
 */
class DeliveryAdapter
{
  public:
    virtual void deliver(DeliveryRecord&, bool sync) = 0;
    virtual ~DeliveryAdapter(){}
};

}}


#endif
