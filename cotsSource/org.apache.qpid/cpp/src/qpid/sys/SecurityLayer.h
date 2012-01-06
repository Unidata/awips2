#ifndef QPID_SYS_SECURITYLAYER_H
#define QPID_SYS_SECURITYLAYER_H

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
#include "qpid/sys/Codec.h"

namespace qpid {
namespace sys {

/**
 * Defines interface to a SASL negotiated Security Layer (for
 * encryption/integrity)
 */
class SecurityLayer : public Codec
{
  public:
    virtual void init(Codec*) = 0;
    virtual ~SecurityLayer() {}
};

}} // namespace qpid::sys

#endif  /*!QPID_SYS_SECURITYLAYER_H*/
