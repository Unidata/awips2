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

#include "qmf/engine/MessageImpl.h"
#include <string.h>

using namespace std;
using namespace qmf::engine;

#define STRING_REF(s) {if (!s.empty()) item.s = const_cast<char*>(s.c_str());}

Message MessageImpl::copy()
{
    Message item;

    ::memset(&item, 0, sizeof(Message));
    item.body   = const_cast<char*>(body.c_str());
    item.length = body.length();
    STRING_REF(destination);
    STRING_REF(routingKey);
    STRING_REF(replyExchange);
    STRING_REF(replyKey);
    STRING_REF(userId);

    return item;
}

