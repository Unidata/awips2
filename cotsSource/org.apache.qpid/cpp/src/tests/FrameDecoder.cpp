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

#include "unit_test.h"
#include "qpid/framing/AMQFrame.h"
#include "qpid/framing/FrameDecoder.h"
#include "qpid/framing/AMQContentBody.h"
#include "qpid/framing/Buffer.h"
#include <string>


namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(FrameDecoderTest)

using namespace std;
using namespace qpid::framing;


string makeData(int size) {
    string data;
    data.resize(size);
    for (int i =0; i < size; ++i)
        data[i] = 'a' + (i%26);
    return data;
}
string encodeFrame(string data) {
    AMQFrame f((AMQContentBody(data)));
    string encoded;
    encoded.resize(f.encodedSize());
    Buffer b(&encoded[0], encoded.size());
    f.encode(b);
    return encoded;
}

string getData(const AMQFrame& frame) {
    const AMQContentBody* content = dynamic_cast<const AMQContentBody*>(frame.getBody());
    BOOST_CHECK(content);
    return content->getData();
}

QPID_AUTO_TEST_CASE(testByteFragments) {
    string data = makeData(42);
    string encoded = encodeFrame(data);
    FrameDecoder decoder;
    for (size_t i = 0; i < encoded.size()-1; ++i) {
        Buffer buf(&encoded[i], 1);
        BOOST_CHECK(!decoder.decode(buf));
    }
    Buffer buf(&encoded[encoded.size()-1], 1);
    BOOST_CHECK(decoder.decode(buf));
    BOOST_CHECK_EQUAL(data, getData(decoder.getFrame()));
}



QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
