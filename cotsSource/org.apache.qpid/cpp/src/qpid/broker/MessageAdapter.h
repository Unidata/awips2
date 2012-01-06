#ifndef _broker_MessageAdapter_h
#define _broker_MessageAdapter_h

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

#include <string>
#include "qpid/framing/FieldTable.h"
#include "qpid/framing/FrameSet.h"

namespace qpid {	
namespace broker {

// TODO aconway 2007-11-09: No longer needed, we only have one type of message.
struct MessageAdapter
{
    virtual ~MessageAdapter() {}

    virtual std::string getRoutingKey(const framing::FrameSet& f) = 0;
    virtual std::string getExchange(const framing::FrameSet& f) = 0;
    virtual bool isImmediate(const framing::FrameSet& f) = 0;
    virtual const framing::FieldTable* getApplicationHeaders(const framing::FrameSet& f) = 0;
    virtual bool isPersistent(const framing::FrameSet& f) = 0;
    virtual bool requiresAccept(const framing::FrameSet& f) = 0;    
};

struct TransferAdapter : MessageAdapter
{
    virtual std::string getRoutingKey(const framing::FrameSet& f);
    virtual std::string getExchange(const framing::FrameSet& f);
    virtual const framing::FieldTable* getApplicationHeaders(const framing::FrameSet& f);
    virtual bool isPersistent(const framing::FrameSet& f);
    bool isImmediate(const framing::FrameSet&);
    bool requiresAccept(const framing::FrameSet& f);    
};

}}


#endif
