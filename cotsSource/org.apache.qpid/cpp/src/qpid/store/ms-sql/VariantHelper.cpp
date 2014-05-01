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
#include "VariantHelper.h"

namespace qpid {
namespace store {
namespace ms_sql {

template <class Wrapped>
VariantHelper<Wrapped>::VariantHelper()
{
    var.vt = VT_EMPTY;
}

template <class Wrapped>
VariantHelper<Wrapped>::operator const _variant_t& () const
{
    return var;
}

// Specialization for using _variant_t to wrap a std::string
VariantHelper<std::string>::VariantHelper(const std::string &init)
{
    var.SetString(init.c_str());
}

VariantHelper<std::string>&
VariantHelper<std::string>::operator=(const std::string &rhs)
{
    var.SetString(rhs.c_str());
    return *this;
}

VariantHelper<std::string>::operator const _variant_t& () const
{
    return var;
}

}}}  // namespace qpid::store::ms_sql
