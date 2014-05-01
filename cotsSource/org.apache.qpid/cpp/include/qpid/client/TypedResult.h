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

#ifndef _TypedResult_
#define _TypedResult_

#include "qpid/client/Completion.h"
#include "qpid/framing/StructHelper.h"

namespace qpid {
namespace client {

/**
 * Returned by asynchronous commands that return a result.
 * You can use get() to wait for completion and get the result value.
 * \ingroup clientapi
 */
template <class T> class TypedResult : public Completion
{
    T result;
    bool decoded;

public:
    ///@internal
    TypedResult(const Completion& c) : Completion(c), decoded(false) {}

    /**
     * Wait for the asynchronous command that returned this TypedResult to complete
     * and return its result.
     *
     *@return The result returned by the command.
     *@exception If the command returns an error, get() throws an exception.
     *
     */
    T& get() {
        if (!decoded) {
            framing::StructHelper helper;
            helper.decode(result, getResult());
            decoded = true;
        }
        return result;
    }
};

}}

#endif
