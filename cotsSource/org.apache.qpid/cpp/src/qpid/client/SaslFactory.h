#ifndef QPID_CLIENT_SASLFACTORY_H
#define QPID_CLIENT_SASLFACTORY_H

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
#include "qpid/client/Sasl.h"
#include "qpid/sys/Mutex.h"
#include <memory>

namespace qpid {
namespace client {

/**
 * Factory for instances of the Sasl interface through which Sasl
 * support is provided to a ConnectionHandler.
 */
class SaslFactory
{
  public:
    std::auto_ptr<Sasl> create(const ConnectionSettings&);
    static SaslFactory& getInstance();
    ~SaslFactory();
  private:
    SaslFactory();
    static qpid::sys::Mutex lock;
    static std::auto_ptr<SaslFactory> instance;
};
}} // namespace qpid::client

#endif  /*!QPID_CLIENT_SASLFACTORY_H*/
