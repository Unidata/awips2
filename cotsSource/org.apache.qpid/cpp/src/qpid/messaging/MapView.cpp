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
#include "qpid/messaging/MapView.h"
#include "qpid/messaging/Message.h"
#include "qpid/client/amqp0_10/Codecs.h"

namespace qpid {
namespace messaging {

class MapViewImpl : public Variant
{
  public:
    MapViewImpl(const Message& msg) : Variant(Variant::Map())
    {
        if (msg.getContent().size()) {
            qpid::client::amqp0_10::MapCodec codec;
            codec.decode(msg.getContent(), *this);
        }
    }
};

MapView::MapView(const Message& m) : impl(new MapViewImpl(m)) {}
MapView::~MapView() { delete impl; }
MapView& MapView::operator=(const MapView& m) { *impl = *m.impl; return *this; }

MapView::const_iterator MapView::begin() const { return impl->asMap().begin(); }
MapView::const_iterator MapView::end() const { return impl->asMap().end(); }
MapView::const_reverse_iterator MapView::rbegin() const { return impl->asMap().rbegin(); }
MapView::const_reverse_iterator MapView::rend() const { return impl->asMap().rend(); }

bool MapView::empty() const { return impl->asMap().empty(); }
size_t MapView::size() const { return impl->asMap().size(); }

MapView::const_iterator MapView::find(const key_type& key) const { return impl->asMap().find(key); }
const Variant& MapView::operator[](const key_type& key) const { return impl->asMap()[key]; }

const std::map<MapView::key_type, Variant>& MapView::asMap() const { return impl->asMap(); }

std::ostream& operator<<(std::ostream& out, const MapView& m)
{
    out << m.asMap();
    return out;
}

}} // namespace qpid::messaging
