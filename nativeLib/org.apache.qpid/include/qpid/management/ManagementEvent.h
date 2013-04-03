#ifndef _ManagementEvent_
#define _ManagementEvent_

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

#include "qpid/management/ManagementObject.h"
#include "qpid/types/Variant.h"
#include <string>

namespace qpid {
namespace management {

class ManagementAgent;

class ManagementEvent : public ManagementItem {
 public:
    static const uint8_t MD5_LEN = 16;
    //typedef void (*writeSchemaCall_t)(qpid::framing::Buffer&);
    typedef void (*writeSchemaCall_t)(std::string&);
    virtual ~ManagementEvent() {}

    virtual writeSchemaCall_t getWriteSchemaCall(void) = 0;
    //virtual mapEncodeSchemaCall_t getMapEncodeSchemaCall(void) = 0;
    virtual std::string& getEventName() const = 0;
    virtual std::string& getPackageName() const = 0;
    virtual uint8_t* getMd5Sum() const = 0;
    virtual uint8_t getSeverity() const = 0;
    virtual void encode(std::string&) const = 0;
    virtual void mapEncode(qpid::types::Variant::Map&) const = 0;
};

}}

#endif  /*!_ManagementEvent_*/
