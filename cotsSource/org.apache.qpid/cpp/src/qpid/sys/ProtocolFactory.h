#ifndef _sys_ProtocolFactory_h
#define _sys_ProtocolFactory_h

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

#include "qpid/sys/IntegerTypes.h"
#include "qpid/SharedObject.h"
#include "qpid/sys/ConnectionCodec.h"
#include <boost/function.hpp>

namespace qpid {
namespace sys {

class Poller;

class ProtocolFactory : public qpid::SharedObject<ProtocolFactory>
{
  public:
    typedef boost::function2<void, int, std::string> ConnectFailedCallback;

    virtual ~ProtocolFactory() = 0;
    virtual uint16_t getPort() const = 0;
    virtual std::string getHost() const = 0;
    virtual void accept(boost::shared_ptr<Poller>, ConnectionCodec::Factory*) = 0;
    virtual void connect(
        boost::shared_ptr<Poller>,
        const std::string& host, int16_t port,
        ConnectionCodec::Factory* codec,
        ConnectFailedCallback failed) = 0;
    virtual bool supports(const std::string& /*capability*/) { return false; }
};

inline ProtocolFactory::~ProtocolFactory() {}

}}


    
#endif  //!_sys_ProtocolFactory_h
