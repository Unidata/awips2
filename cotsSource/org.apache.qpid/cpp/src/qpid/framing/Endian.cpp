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
#include "qpid/framing/Endian.h"

namespace qpid {
namespace framing {

Endian::Endian() : littleEndian(!testBigEndian()) {}

bool Endian::testBigEndian()
{
    uint16_t a = 1;
    uint16_t b;
    uint8_t* p = (uint8_t*) &b;
    p[0] = 0xFF & (a >> 8);
    p[1] = 0xFF & (a);
    return a == b;
}

uint8_t* Endian::convertIfRequired(uint8_t* const octets, int width)
{
    if (instance.littleEndian) {
        for (int i = 0; i < (width/2); i++) {
            uint8_t temp = octets[i];
            octets[i] = octets[width - (1 + i)];
            octets[width - (1 + i)] = temp;
        }
    }
    return octets;
}

const Endian Endian::instance;

}} // namespace qpid::framing
