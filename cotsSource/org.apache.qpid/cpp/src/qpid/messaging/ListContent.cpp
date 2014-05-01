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
#include "qpid/messaging/ListContent.h"
#include "qpid/messaging/Message.h"
#include "qpid/client/amqp0_10/Codecs.h"

namespace qpid {
namespace messaging {

class ListContentImpl : public Variant
{
    Message* msg;
  public:
    ListContentImpl(Message& m) : Variant(Variant::List()), msg(&m)
    {
        if (msg->getContent().size()) {
            qpid::client::amqp0_10::ListCodec codec;
            codec.decode(msg->getContent(), *this);
        }
    }

    void encode()
    {
        qpid::client::amqp0_10::ListCodec codec;
        codec.encode(*this, msg->getContent());
    }
};

ListContent::ListContent(Message& m) : impl(new ListContentImpl(m)) {}
ListContent::~ListContent() { delete impl; }
ListContent& ListContent::operator=(const ListContent& l) { *impl = *l.impl; return *this; }

ListContent::const_iterator ListContent::begin() const { return impl->asList().begin(); }
ListContent::const_iterator ListContent::end() const { return impl->asList().end(); }
ListContent::const_reverse_iterator ListContent::rbegin() const { return impl->asList().rbegin(); }
ListContent::const_reverse_iterator ListContent::rend() const { return impl->asList().rend(); }

ListContent::iterator ListContent::begin() { return impl->asList().begin(); }
ListContent::iterator ListContent::end() { return impl->asList().end(); }
ListContent::reverse_iterator ListContent::rbegin() { return impl->asList().rbegin(); }
ListContent::reverse_iterator ListContent::rend() { return impl->asList().rend(); }

bool ListContent::empty() const { return impl->asList().empty(); }
size_t ListContent::size() const { return impl->asList().size(); }

const Variant& ListContent::front() const { return impl->asList().front(); }
Variant& ListContent::front() { return impl->asList().front(); }
const Variant& ListContent::back() const { return impl->asList().back(); }
Variant& ListContent::back() { return impl->asList().back(); }

void ListContent::push_front(const Variant& v) { impl->asList().push_front(v); }
void ListContent::push_back(const Variant& v) { impl->asList().push_back(v); }

void ListContent::pop_front() { impl->asList().pop_front(); }
void ListContent::pop_back() { impl->asList().pop_back(); }

ListContent::iterator ListContent::insert(iterator position, const Variant& v)
{
    return impl->asList().insert(position, v);
}
void ListContent::insert(iterator position, size_t n, const Variant& v)
{
    impl->asList().insert(position, n, v);
}
ListContent::iterator ListContent::erase(iterator position) { return impl->asList().erase(position); }
ListContent::iterator ListContent::erase(iterator first, iterator last) { return impl->asList().erase(first, last); }
void ListContent::clear() { impl->asList().clear(); }

void ListContent::encode() { impl->encode(); }
    
const Variant::List& ListContent::asList() const { return impl->asList(); }
Variant::List& ListContent::asList() { return impl->asList(); }

std::ostream& operator<<(std::ostream& out, const ListContent& m)
{
    out << m.asList();
    return out;
}

}} // namespace qpid::messaging
