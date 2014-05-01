#ifndef QPID_FRAMING_INVOKER_H
#define QPID_FRAMING_INVOKER_H

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
#include "qpid/framing/AMQMethodBody.h"
#include "qpid/framing/MethodBodyDefaultVisitor.h"
#include "qpid/framing/StructHelper.h"

#include <boost/optional.hpp>

namespace qpid {
namespace framing {

class AMQMethodBody;

/**
 * Base class for invoker visitors.
 */ 
class Invoker: public MethodBodyDefaultVisitor, protected StructHelper
{
  public:
    struct Result {
      public:
        Result() : handled(false) {}
        const std::string& getResult() const { return result; }
        bool hasResult() const { return !result.empty(); }
        bool wasHandled() const { return handled; }
        operator bool() const { return handled; }

        std::string result;
        bool handled;
    };

    void defaultVisit(const AMQMethodBody&) {}
    Result getResult() const { return result; }

  protected:
    Result result;
};

/**
 * Invoke an invocable object.
 * Invocable classes must provide a nested type Invoker.
 */
template <class Invocable>
Invoker::Result invoke(Invocable& target, const AMQMethodBody& body) {
    typename Invocable::Invoker invoker(target);
    body.accept(invoker);
    return invoker.getResult();
}

/**
 * Invoke an invocable object.
 * Invocable classes must provide a nested type Invoker.
 */
template <class Invocable>
Invoker::Result invoke(Invocable& target, const AMQBody& body) {
    typename Invocable::Invoker invoker(target);
    const AMQMethodBody* method = body.getMethod();
    if (method)
        method->accept(invoker);
    return invoker.getResult();
}

}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_INVOKER_H*/
