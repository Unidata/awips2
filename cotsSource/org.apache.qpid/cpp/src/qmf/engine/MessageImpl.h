#ifndef _QmfEngineMessageImpl_
#define _QmfEngineMessageImpl_

/*
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
 */

#include "qmf/engine/Message.h"
#include <string>
#include <boost/shared_ptr.hpp>

namespace qmf {
namespace engine {

    struct MessageImpl {
        typedef boost::shared_ptr<MessageImpl> Ptr;
        std::string body;
        std::string destination;
        std::string routingKey;
        std::string replyExchange;
        std::string replyKey;
        std::string userId;

        Message copy();
    };
}
}

#endif
