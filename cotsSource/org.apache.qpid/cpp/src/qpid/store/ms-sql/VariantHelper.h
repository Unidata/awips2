#ifndef QPID_STORE_MSSQL_VARIANTHELPER_H
#define QPID_STORE_MSSQL_VARIANTHELPER_H

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

#include <comutil.h>

namespace qpid {
namespace store {
namespace ms_sql {

/**
 * @class VariantHelper
 *
 * Class template to wrap the details of working with _variant_t objects.
 */
template <class Wrapped> class VariantHelper {
private:
    _variant_t var;

public:
    VariantHelper();
    VariantHelper(const Wrapped &init);

    VariantHelper& operator =(const Wrapped& rhs);
    operator const _variant_t& () const;
};

// Specialization for using _variant_t to wrap a std::string
template<> class VariantHelper<std::string> {
private:
    _variant_t var;

public:
    VariantHelper(const std::string &init);
    VariantHelper& operator =(const std::string& rhs);
    operator const _variant_t& () const;
};

}}}  // namespace qpid::store::ms_sql

#endif /* QPID_STORE_MSSQL_VARIANTHELPER_H */
