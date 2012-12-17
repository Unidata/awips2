#ifndef QPID_MESSAGING_LISTCONTENT_H
#define QPID_MESSAGING_LISTCONTENT_H

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

namespace qpid {
namespace messaging {

class ListContentImpl;
class Message;

/**
 * Allows message content to be manipulated as a list.
 */
class ListContent
{
  public:
    typedef Variant::List::iterator iterator;
    typedef Variant::List::reverse_iterator reverse_iterator;
    typedef Variant::List::const_iterator const_iterator;
    typedef Variant::List::const_reverse_iterator const_reverse_iterator;

    QPID_CLIENT_EXTERN ListContent(Message&);
    QPID_CLIENT_EXTERN ~ListContent();

    QPID_CLIENT_EXTERN const_iterator begin() const;
    QPID_CLIENT_EXTERN iterator begin();
    QPID_CLIENT_EXTERN const_iterator end() const;
    QPID_CLIENT_EXTERN iterator end();
    QPID_CLIENT_EXTERN const_reverse_iterator rbegin() const;
    QPID_CLIENT_EXTERN reverse_iterator rbegin();
    QPID_CLIENT_EXTERN const_reverse_iterator rend() const;
    QPID_CLIENT_EXTERN reverse_iterator rend();

    QPID_CLIENT_EXTERN bool empty() const;
    QPID_CLIENT_EXTERN size_t size() const;

    QPID_CLIENT_EXTERN const Variant& front() const;
    QPID_CLIENT_EXTERN Variant& front();
    QPID_CLIENT_EXTERN const Variant& back() const;
    QPID_CLIENT_EXTERN Variant& back();

    QPID_CLIENT_EXTERN void push_front(const Variant&);
    QPID_CLIENT_EXTERN void push_back(const Variant&);

    QPID_CLIENT_EXTERN void pop_front();
    QPID_CLIENT_EXTERN void pop_back();

    QPID_CLIENT_EXTERN iterator insert(iterator position, const Variant&);
    QPID_CLIENT_EXTERN void insert(iterator position, size_t n, const Variant&);
    QPID_CLIENT_EXTERN iterator erase(iterator position);
    QPID_CLIENT_EXTERN iterator erase(iterator first, iterator last);
    QPID_CLIENT_EXTERN void clear();

    QPID_CLIENT_EXTERN void encode();
    
    QPID_CLIENT_EXTERN const Variant::List& asList() const;
    QPID_CLIENT_EXTERN Variant::List& asList();
  private:
    ListContentImpl* impl;

    QPID_CLIENT_EXTERN ListContent& operator=(const ListContent&);
};

QPID_CLIENT_EXTERN std::ostream& operator<<(std::ostream& out, const ListContent& m);

}} // namespace qpid::messaging

#endif  /*!QPID_MESSAGING_LISTCONTENT_H*/
