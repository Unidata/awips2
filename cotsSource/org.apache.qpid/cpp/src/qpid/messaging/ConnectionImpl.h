#ifndef QPID_MESSAGING_CONNECTIONIMPL_H
#define QPID_MESSAGING_CONNECTIONIMPL_H

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
#include <string>
#include "qpid/RefCounted.h"

namespace qpid {
namespace client {
}

namespace messaging {

class Session;

class ConnectionImpl : public virtual qpid::RefCounted
{
  public:
    virtual ~ConnectionImpl() {}
    virtual void close() = 0;
    virtual Session newSession(bool transactional, const std::string& name) = 0;
    virtual Session getSession(const std::string& name) const = 0;
  private:
};
}} // namespace qpid::messaging

#endif  /*!QPID_MESSAGING_CONNECTIONIMPL_H*/
