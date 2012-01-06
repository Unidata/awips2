#ifndef QPID_CLIENT_AMQP0_10_CODECS_H
#define QPID_CLIENT_AMQP0_10_CODECS_H

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
#include "qpid/messaging/Codec.h"

namespace qpid {
namespace client {
namespace amqp0_10 {


/**
 * Codec for encoding/decoding a map of Variants using the AMQP 0-10
 * map encoding.
 */
class MapCodec : public qpid::messaging::Codec
{
  public:
    void encode(const qpid::messaging::Variant&, std::string&);
    void decode(const std::string&, qpid::messaging::Variant&);

    static const std::string contentType;
  private:
};

/**
 * Codec for encoding/decoding a list of Variants using the AMQP 0-10
 * list encoding.
 */
class ListCodec : public qpid::messaging::Codec
{
  public:
    void encode(const qpid::messaging::Variant&, std::string&);
    void decode(const std::string&, qpid::messaging::Variant&);

    static const std::string contentType;
  private:
};

}}} // namespace qpid::client::amqp0_10

#endif  /*!QPID_CLIENT_AMQP0_10_CODECS_H*/
