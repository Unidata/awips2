#ifndef QPID_PTR_MAP
#define QPID_PTR_MAP

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

#include <boost/ptr_container/ptr_map.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/type_traits/remove_const.hpp>

namespace qpid {

/** @file
 * Workaround for API change between boost 1.33 and 1.34.
 *
 * To be portable across these versions, code using boost::ptr_map
 * iterators should use ptr_map_ptr(i) to get the pointer from
 * boost::ptr_map::iterator i.
 *
 * @see http://www.boost.org/libs/ptr_container/doc/ptr_container.html#upgrading-from-boost-v-1-33
 */


typedef boost::is_same<boost::ptr_map<int, int>::iterator::value_type, int> IsOldPtrMap;

template <class Iter>
typename boost::enable_if<IsOldPtrMap, typename Iter::value_type*>::type
ptr_map_ptr(const Iter& i) { return &*i; }

template <class Iter>
typename boost::disable_if<IsOldPtrMap,
                           typename boost::remove_const<typename Iter::value_type::second_type>::type
                           >::type
ptr_map_ptr(const Iter& i) { return i->second; }

} // namespace qpid

#endif  /*!QPID_PTR_MAP*/
