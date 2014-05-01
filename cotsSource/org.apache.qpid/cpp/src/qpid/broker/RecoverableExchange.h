#ifndef _broker_RecoverableExchange_h
#define _broker_RecoverableExchange_h

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

#include <boost/shared_ptr.hpp>
#include "qpid/framing/FieldTable.h"

namespace qpid {
namespace broker {

/**
 * The interface through which bindings are recovered.
 */
class RecoverableExchange
{
public:
    typedef boost::shared_ptr<RecoverableExchange> shared_ptr;

    virtual void setPersistenceId(uint64_t id) = 0;
    /**
     * Recover binding. Nb: queue must have been recovered earlier.
     */
    virtual void bind(const std::string& queue,
                      const std::string& routingKey,
                      qpid::framing::FieldTable& args) = 0;
    virtual ~RecoverableExchange() {};
};

}}


#endif
