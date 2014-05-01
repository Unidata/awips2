#ifndef QPID_STORE_MSSQL_EXCEPTION_H
#define QPID_STORE_MSSQL_EXCEPTION_H

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

#include <string>
#include <comdef.h>
#include <qpid/store/StorageProvider.h>

namespace qpid {
namespace store {
namespace ms_sql {

class Exception : public qpid::store::StorageProvider::Exception
{
protected:
    std::string text;
public:
    Exception(const std::string& _text) : text(_text) {}
    virtual ~Exception() {}
    virtual const char* what() const throw() { return text.c_str(); }
};

class ADOException : public Exception
{
public:
    ADOException(const std::string& _text, _com_error &e)
      : Exception(_text) {
        text += ": ";
        text += e.ErrorMessage();
        IErrorInfo *i = e.ErrorInfo();
        if (i != 0) {
            text += ": ";
            _bstr_t wmsg = e.Description();
            text += (const char *)wmsg;
            i->Release();
        }
    }
};

}}}  // namespace qpid::store::ms_sql

#endif /* QPID_STORE_MSSQL_EXCEPTION_H */
