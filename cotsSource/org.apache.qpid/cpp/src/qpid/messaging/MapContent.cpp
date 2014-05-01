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
#include "qpid/messaging/MapContent.h"
#include "qpid/messaging/Message.h"
#include "qpid/client/amqp0_10/Codecs.h"

namespace qpid {
namespace messaging {

class MapContentImpl : public Variant
{
    Message* msg;
  public:
    MapContentImpl(Message& m) : Variant(Variant::Map()), msg(&m)
    {
        if (msg->getContent().size()) {
            qpid::client::amqp0_10::MapCodec codec;
            codec.decode(msg->getContent(), *this);
        }
    }

    void encode()
    {
        qpid::client::amqp0_10::MapCodec codec;
        codec.encode(*this, msg->getContent());
        msg->setContentType(qpid::client::amqp0_10::MapCodec::contentType);
    }
};

MapContent::MapContent(Message& m) : impl(new MapContentImpl(m)) {}
MapContent::~MapContent() { delete impl; }
MapContent& MapContent::operator=(const MapContent& m) { *impl = *m.impl; return *this; }

MapContent::const_iterator MapContent::begin() const { return impl->asMap().begin(); }
MapContent::const_iterator MapContent::end() const { return impl->asMap().end(); }
MapContent::const_reverse_iterator MapContent::rbegin() const { return impl->asMap().rbegin(); }
MapContent::const_reverse_iterator MapContent::rend() const { return impl->asMap().rend(); }
MapContent::iterator MapContent::begin() { return impl->asMap().begin(); }
MapContent::iterator MapContent::end() { return impl->asMap().end(); }
MapContent::reverse_iterator MapContent::rbegin() { return impl->asMap().rbegin(); }
MapContent::reverse_iterator MapContent::rend() { return impl->asMap().rend(); }

bool MapContent::empty() const { return impl->asMap().empty(); }
size_t MapContent::size() const { return impl->asMap().size(); }

MapContent::const_iterator MapContent::find(const key_type& key) const { return impl->asMap().find(key); }
MapContent::iterator MapContent::find(const key_type& key) { return impl->asMap().find(key); }
const Variant& MapContent::operator[](const key_type& key) const { return impl->asMap()[key]; }
Variant& MapContent::operator[](const key_type& key) { return impl->asMap()[key]; }

std::pair<MapContent::iterator,bool> MapContent::insert(const value_type& item) { return impl->asMap().insert(item); }
MapContent::iterator MapContent::insert(iterator position, const value_type& item) { return impl->asMap().insert(position, item); }
void MapContent::erase(iterator position) { impl->asMap().erase(position); }
void MapContent::erase(iterator first, iterator last) { impl->asMap().erase(first, last); }
size_t MapContent::erase(const key_type& key) { return impl->asMap().erase(key); }
void MapContent::clear() { impl->asMap().clear(); }

void MapContent::encode() { impl->encode(); }

const std::map<MapContent::key_type, Variant>& MapContent::asMap() const { return impl->asMap(); }
std::map<MapContent::key_type, Variant>& MapContent::asMap() { return impl->asMap(); }


std::ostream& operator<<(std::ostream& out, const MapContent& m)
{
    out << m.asMap();
    return out;
}

}} // namespace qpid::messaging
