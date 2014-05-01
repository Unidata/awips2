#ifndef QPID_BROKER_RETRYLIST_H
#define QPID_BROKER_RETRYLIST_H

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

#include "qpid/broker/BrokerImportExport.h"
#include "qpid/Address.h"
#include "qpid/Url.h"

namespace qpid {
namespace broker {

/**
 * Simple utility for managing a list of urls to try on reconnecting a
 * link. Currently only supports TCP urls.
 */
class RetryList
{
  public:
    QPID_BROKER_EXTERN RetryList();                
    QPID_BROKER_EXTERN void reset(const std::vector<Url>& urls);
    QPID_BROKER_EXTERN bool next(TcpAddress& address);
  private:
    std::vector<Url> urls;
    size_t urlIndex;
    size_t addressIndex;

  friend std::ostream& operator<<(std::ostream& os, const RetryList& l);
};

std::ostream& operator<<(std::ostream& os, const RetryList& l);

}} // namespace qpid::broker

#endif  /*!QPID_BROKER_RETRYLIST_H*/
