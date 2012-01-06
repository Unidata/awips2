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
#ifndef _QueueBindings_
#define _QueueBindings_

#include "qpid/framing/FieldTable.h"
#include <boost/ptr_container/ptr_list.hpp>
#include <boost/shared_ptr.hpp>
#include <algorithm>

namespace qpid {
namespace broker {

class ExchangeRegistry;
class Queue;

struct QueueBinding{
    std::string exchange;
    std::string key;
    qpid::framing::FieldTable args;
    QueueBinding(const std::string& exchange, const std::string& key, const qpid::framing::FieldTable& args);
};

class QueueBindings
{
  public:
    
    /** Apply f to each QueueBinding. */
    template <class F> void eachBinding(F f) const { std::for_each(bindings.begin(), bindings.end(), f); }

    void add(const std::string& exchange, const std::string& key, const qpid::framing::FieldTable& args);
    void unbind(ExchangeRegistry& exchanges, boost::shared_ptr<Queue> queue);

  private:
    typedef std::vector<QueueBinding> Bindings;
    Bindings bindings;
};


}} // namespace qpid::broker


#endif
