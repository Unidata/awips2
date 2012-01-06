#ifndef QPID_CLIENT_SESSIONBASEACCESS_H
#define QPID_CLIENT_SESSIONBASEACCESS_H

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

#include "qpid/client/SessionBase_0_10.h"

/**@file @internal  Internal use only */

namespace qpid {
namespace client {

class SessionBase_0_10Access {
  public:
    SessionBase_0_10Access(SessionBase_0_10& sb_) : sb(sb_) {}
    void set(const boost::shared_ptr<SessionImpl>& si) { sb.impl = si; }
    boost::shared_ptr<SessionImpl> get() const { return sb.impl; }
  private:
    SessionBase_0_10& sb;
};
}} // namespace qpid::client

#endif  /*!QPID_CLIENT_SESSIONBASEACCESS_H*/
