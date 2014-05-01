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
#ifndef _ConnectionInputHandler_
#define _ConnectionInputHandler_

#include "qpid/framing/InputHandler.h"
#include "qpid/sys/OutputTask.h"
#include "qpid/sys/TimeoutHandler.h"

namespace qpid {
namespace sys {

    class ConnectionInputHandler :
        public qpid::framing::InputHandler, 
        public TimeoutHandler, public OutputTask
    {
    public:

        virtual void closed() = 0;
    };

}
}


#endif
