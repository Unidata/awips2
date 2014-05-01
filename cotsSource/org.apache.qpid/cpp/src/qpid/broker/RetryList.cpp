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
#include "qpid/broker/RetryList.h"

namespace qpid {
namespace broker {

RetryList::RetryList() : urlIndex(0), addressIndex(0) {}

void RetryList::reset(const std::vector<Url>& u)
{
    urls = u;
    urlIndex = addressIndex = 0;//reset indices
}
                
bool RetryList::next(TcpAddress& address)
{
    while (urlIndex < urls.size()) {
        while (addressIndex < urls[urlIndex].size()) {
            const TcpAddress* tcp = urls[urlIndex][addressIndex++].get<TcpAddress>();
            if (tcp) {
                address = *tcp;
                return true;
            }
        }
        urlIndex++;
        addressIndex = 0;
    }
                    
    urlIndex = addressIndex = 0;//reset indices
    return false;
}

std::ostream& operator<<(std::ostream& os, const RetryList& l)
{
    for (size_t i = 0; i < l.urls.size(); i++) {
        os << l.urls[i] << " ";
    }
    return os;
}

}} // namespace qpid::broker
