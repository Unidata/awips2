#ifndef QPID_MESSAGING_MAPVIEW_H
#define QPID_MESSAGING_MAPVIEW_H

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
#include "qpid/client/ClientImportExport.h"
#include "Variant.h"
#include <map>
#include <string>

namespace qpid {
namespace messaging {

class MapViewImpl;
class Message;

/**
 * Provides a view of message content as a list
 */
class MapView
{
  public:
    typedef std::string key_type;
    typedef std::pair<key_type, Variant> value_type;
    typedef std::map<key_type, Variant>::const_iterator const_iterator;
    typedef std::map<key_type, Variant>::const_reverse_iterator const_reverse_iterator;

    QPID_CLIENT_EXTERN MapView(const Message&);
    QPID_CLIENT_EXTERN ~MapView();
    QPID_CLIENT_EXTERN MapView& operator=(const MapView&);

    QPID_CLIENT_EXTERN const_iterator begin() const;
    QPID_CLIENT_EXTERN const_iterator end() const;
    QPID_CLIENT_EXTERN const_reverse_iterator rbegin() const;
    QPID_CLIENT_EXTERN const_reverse_iterator rend() const;

    QPID_CLIENT_EXTERN bool empty() const;
    QPID_CLIENT_EXTERN size_t size() const;

    QPID_CLIENT_EXTERN const_iterator find(const key_type&) const;
    QPID_CLIENT_EXTERN const Variant& operator[](const key_type&) const;

    QPID_CLIENT_EXTERN const std::map<key_type, Variant>& asMap() const;
  private:
    MapViewImpl* impl;
};

QPID_CLIENT_EXTERN std::ostream& operator<<(std::ostream& out, const MapView& m);

}} // namespace qpid::messaging

#endif  /*!QPID_MESSAGING_MAPVIEW_H*/
