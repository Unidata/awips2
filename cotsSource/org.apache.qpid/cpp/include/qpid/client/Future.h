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

#ifndef _Future_
#define _Future_

#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include "qpid/Exception.h"
#include "qpid/framing/SequenceNumber.h"
#include "qpid/client/FutureCompletion.h"
#include "qpid/client/FutureResult.h"
#include "qpid/client/ClientImportExport.h"

namespace qpid {
namespace client {

/**@internal */
class Future
{
    framing::SequenceNumber command;
    boost::shared_ptr<FutureResult> result;
    bool complete;

public:
    Future() : complete(false) {}
    Future(const framing::SequenceNumber& id) : command(id), complete(false) {}

    std::string getResult(SessionImpl& session) {
        if (result) return result->getResult(session);
        else throw Exception("Result not expected");
    }

    QPID_CLIENT_EXTERN void wait(SessionImpl& session);
    QPID_CLIENT_EXTERN bool isComplete(SessionImpl& session);
    QPID_CLIENT_EXTERN void setFutureResult(boost::shared_ptr<FutureResult> r);
};

}}

#endif
