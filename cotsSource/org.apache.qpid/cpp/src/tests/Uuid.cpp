/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "qpid/framing/Uuid.h"
#include "qpid/framing/Buffer.h"
#include "qpid/sys/alloca.h"

#include "unit_test.h"

#include <set>

namespace qpid {
namespace tests {

QPID_AUTO_TEST_SUITE(UuidTestSuite)

using namespace std;
using namespace qpid::framing;

struct UniqueSet : public std::set<Uuid> {
    void operator()(const Uuid& uuid) {
        BOOST_REQUIRE(find(uuid) == end());
        insert(uuid);
    }
};

QPID_AUTO_TEST_CASE(testUuidCtor) {
    // Uniqueness
    boost::array<Uuid,1000> uuids;
    for_each(uuids.begin(), uuids.end(), mem_fun_ref(&Uuid::generate));
    UniqueSet unique;
    for_each(uuids.begin(), uuids.end(), unique);
}

boost::array<uint8_t, 16>  sample =  {{'\x1b', '\x4e', '\x28', '\xba', '\x2f', '\xa1', '\x11', '\xd2', '\x88', '\x3f', '\xb9', '\xa7', '\x61', '\xbd', '\xe3', '\xfb'}};
const string sampleStr("1b4e28ba-2fa1-11d2-883f-b9a761bde3fb");

QPID_AUTO_TEST_CASE(testUuidIstream) {
    Uuid uuid;
    istringstream in(sampleStr);
    in >> uuid;
    BOOST_CHECK(!in.fail());
    BOOST_CHECK(uuid == sample);
}

QPID_AUTO_TEST_CASE(testUuidOstream) {
    Uuid uuid(sample.c_array());
    ostringstream out;
    out << uuid;
    BOOST_CHECK(out.good());
    BOOST_CHECK_EQUAL(out.str(), sampleStr);
}

QPID_AUTO_TEST_CASE(testUuidEncodeDecode) {
    char* buff = static_cast<char*>(::alloca(Uuid::size()));
    Buffer wbuf(buff, Uuid::size());
    Uuid uuid(sample.c_array());
    uuid.encode(wbuf);

    Buffer rbuf(buff, Uuid::size());
    Uuid decoded;
    decoded.decode(rbuf);
    BOOST_CHECK_EQUAL(string(sample.begin(), sample.end()),
                      string(decoded.begin(), decoded.end()));
}

QPID_AUTO_TEST_SUITE_END()

}} // namespace qpid::tests
