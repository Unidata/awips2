#ifndef QPID_STORE_STOREEXCEPTION_H
#define QPID_STORE_STOREEXCEPTION_H

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

#include <exception>
#include <boost/format.hpp>
#include "StorageProvider.h"

namespace qpid {
namespace store {

class StoreException : public std::exception
{
    std::string text;
public:
    StoreException(const std::string& _text) : text(_text) {}
    StoreException(const std::string& _text,
                   const StorageProvider::Exception& cause)
        : text(_text + ": " + cause.what()) {}
    virtual ~StoreException() throw() {}
    virtual const char* what() const throw() { return text.c_str(); }
};

#define THROW_STORE_EXCEPTION(MESSAGE) throw qpid::store::StoreException(boost::str(boost::format("%s (%s:%d)") % (MESSAGE) % __FILE__ % __LINE__))
#define THROW_STORE_EXCEPTION_2(MESSAGE, EXCEPTION) throw qpid::store::StoreException(boost::str(boost::format("%s (%s:%d)") % (MESSAGE) % __FILE__ % __LINE__), EXCEPTION)

}}  // namespace qpid::store

#endif /* QPID_STORE_STOREEXCEPTION_H */
