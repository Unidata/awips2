#ifndef QPID_CLIENT_COMPLETIONIMPL_H
#define QPID_CLIENT_COMPLETIONIMPL_H

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

#include "qpid/RefCounted.h"
#include "qpid/client/Future.h"
#include <boost/shared_ptr.hpp>

namespace qpid {
namespace client {

///@internal
class CompletionImpl : public RefCounted
{
public:
    CompletionImpl() {}
    CompletionImpl(Future f, boost::shared_ptr<SessionImpl> s) : future(f), session(s) {}

    bool isComplete() { return future.isComplete(*session); }
    void wait() { future.wait(*session); }
    std::string getResult() { return future.getResult(*session); }

protected:
    Future future;
    boost::shared_ptr<SessionImpl> session;
};

}} // namespace qpid::client


#endif  /*!QPID_CLIENT_COMPLETIONIMPL_H*/
