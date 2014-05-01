#ifndef QPID_FRAMING_ENDIAN_H
#define QPID_FRAMING_ENDIAN_H

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

#include "qpid/sys/IntegerTypes.h"

namespace qpid {
namespace framing {

/**
 * Conversion utility for little-endian platforms that need to convert
 * to and from network ordered octet sequences
 */
class Endian
{
  public:
    static uint8_t* convertIfRequired(uint8_t* const octets, int width);
  private:
    const bool littleEndian;
    Endian();
    static const Endian instance;
    static bool testBigEndian();
};
}} // namespace qpid::framing

#endif  /*!QPID_FRAMING_ENDIAN_H*/
