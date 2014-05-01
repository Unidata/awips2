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
#include "qpid/messaging/ListView.h"
#include "qpid/messaging/Message.h"
#include "qpid/client/amqp0_10/Codecs.h"

namespace qpid {
namespace messaging {

class ListViewImpl : public Variant
{
  public:
    ListViewImpl(const Message& msg) : Variant(Variant::List())
    {
        if (msg.getContent().size()) {
            qpid::client::amqp0_10::ListCodec codec;
            codec.decode(msg.getContent(), *this);
        }
    }
};

ListView::ListView(const Message& m) :impl(new ListViewImpl(m)) {}
ListView::~ListView() { delete impl; }
ListView& ListView::operator=(const ListView& l) { *impl = *l.impl; return *this; }

ListView::const_iterator ListView::begin() const { return impl->asList().begin(); }
ListView::const_iterator ListView::end() const { return impl->asList().end(); }
ListView::const_reverse_iterator ListView::rbegin() const { return impl->asList().rbegin(); }
ListView::const_reverse_iterator ListView::rend() const { return impl->asList().rend(); }

bool ListView::empty() const { return impl->asList().empty(); }
size_t ListView::size() const { return impl->asList().size(); }

const Variant& ListView::front() const { return impl->asList().front(); }
const Variant& ListView::back() const { return impl->asList().back(); }
    
const Variant::List& ListView::asList() const { return impl->asList(); }

std::ostream& operator<<(std::ostream& out, const ListView& m)
{
    out << m.asList();
    return out;
}

}} // namespace qpid::messaging
