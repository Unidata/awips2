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

#include "qpid/broker/Broker.h"
#include <stdlib.h>

namespace qpid {
namespace broker {

const std::string Broker::Options::DEFAULT_DATA_DIR_LOCATION("/tmp");
const std::string Broker::Options::DEFAULT_DATA_DIR_NAME("/.qpidd");

std::string
Broker::Options::getHome() {
    std::string home;
    char *home_c = ::getenv("HOME");
    if (home_c != 0)
        home += home_c;
    return home;
}

}}   // namespace qpid::broker
