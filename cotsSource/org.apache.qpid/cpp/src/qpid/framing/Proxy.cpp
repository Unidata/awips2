/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "qpid/framing/Proxy.h"
#include "qpid/framing/AMQFrame.h"
#include "qpid/framing/AMQMethodBody.h"
#include "qpid/log/Statement.h"

namespace qpid {
namespace framing {

Proxy::Proxy(FrameHandler& h) : out(&h), sync(false) {}

Proxy::~Proxy() {}

void Proxy::send(const AMQBody& b) {
    if (sync) {
        const AMQMethodBody* m = dynamic_cast<const AMQMethodBody*>(&b);
        if (m)  m->setSync(sync);
    }
    AMQFrame f(b);
    out->handle(f);
}

ProtocolVersion Proxy::getVersion() const {
    return ProtocolVersion();
}

FrameHandler& Proxy::getHandler() { return *out; }

void Proxy::setHandler(FrameHandler& f) { out=&f; }

Proxy::ScopedSync::ScopedSync(Proxy& p) : proxy(p) { proxy.sync = true; }
Proxy::ScopedSync::~ScopedSync() { proxy.sync = false; }

}} // namespace qpid::framing
