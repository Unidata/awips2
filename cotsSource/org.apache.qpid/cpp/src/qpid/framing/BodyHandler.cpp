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
#include "qpid/framing/BodyHandler.h"
#include "qpid/framing/AMQMethodBody.h"
#include "qpid/framing/AMQHeaderBody.h"
#include "qpid/framing/AMQContentBody.h"
#include "qpid/framing/AMQHeartbeatBody.h"
#include <boost/cast.hpp>
#include "qpid/framing/reply_exceptions.h"

using namespace qpid::framing;
using namespace boost;

BodyHandler::~BodyHandler() {}

// TODO aconway 2007-08-13: Replace with visitor.
void BodyHandler::handleBody(AMQBody* body) {
    switch(body->type())
    {
      case METHOD_BODY:
	handleMethod(polymorphic_downcast<AMQMethodBody*>(body));
	break;
      case HEADER_BODY:
	handleHeader(polymorphic_downcast<AMQHeaderBody*>(body));
	break;
      case CONTENT_BODY:
	handleContent(polymorphic_downcast<AMQContentBody*>(body));
	break;
      case HEARTBEAT_BODY:
	handleHeartbeat(polymorphic_downcast<AMQHeartbeatBody*>(body));
	break;
      default:
          throw FramingErrorException(
            QPID_MSG("Invalid frame type " << body->type()));
    }
}

