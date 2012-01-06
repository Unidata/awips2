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
#ifndef _TransferContent_
#define _TransferContent_

#include "qpid/framing/FrameSet.h"
#include "qpid/framing/MethodContent.h"
#include "qpid/Exception.h"
#include "qpid/framing/MessageProperties.h"
#include "qpid/framing/DeliveryProperties.h"
#include "qpid/CommonImportExport.h"

namespace qpid {
namespace framing {

/** Message content */
class TransferContent : public MethodContent
{
    AMQHeaderBody header;
    std::string data;
public:
    QPID_COMMON_EXTERN	TransferContent(const std::string& data = std::string(), const std::string& key=std::string());

    ///@internal
    QPID_COMMON_EXTERN AMQHeaderBody getHeader() const;

    QPID_COMMON_EXTERN void setData(const std::string&);
    QPID_COMMON_EXTERN const std::string& getData() const;
    QPID_COMMON_EXTERN std::string& getData();

    QPID_COMMON_EXTERN void appendData(const std::string&);

    QPID_COMMON_EXTERN bool hasMessageProperties() const;
    QPID_COMMON_EXTERN MessageProperties& getMessageProperties();
    QPID_COMMON_EXTERN const MessageProperties& getMessageProperties() const;

    QPID_COMMON_EXTERN bool hasDeliveryProperties() const;
    QPID_COMMON_EXTERN DeliveryProperties& getDeliveryProperties();
    QPID_COMMON_EXTERN const DeliveryProperties& getDeliveryProperties() const;

    ///@internal
    QPID_COMMON_EXTERN void populate(const FrameSet& frameset);
};

}}
#endif  
