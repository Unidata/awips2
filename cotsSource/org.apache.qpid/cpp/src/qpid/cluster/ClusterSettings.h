#ifndef QPID_CLUSTER_CLUSTERSETTINGS_H
#define QPID_CLUSTER_CLUSTERSETTINGS_H

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

#include <qpid/Url.h>
#include <string>

namespace qpid {
namespace cluster {

struct ClusterSettings {
    std::string name;
    std::string url;
    bool quorum;
    size_t readMax;
    std::string username, password, mechanism;
    size_t size;

    ClusterSettings() : quorum(false), readMax(10), mechanism("ANONYMOUS"), size(1)
    {}
  
    Url getUrl(uint16_t port) const {
        if (url.empty()) return Url::getIpAddressesUrl(port);
        return Url(url);
    }
};

}} // namespace qpid::cluster

#endif  /*!QPID_CLUSTER_CLUSTERSETTINGS_H*/
