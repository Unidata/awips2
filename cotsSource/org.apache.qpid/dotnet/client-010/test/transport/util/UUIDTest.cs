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
using System;
using NUnit.Framework;
using org.apache.qpid.transport.util;

namespace test.transport.util
{
    [TestFixture]

    public class UUIDTest
    {


        [Test]
        public void createUUID()
        {
            UUID uuid = UUID.RandomUuid();
            String uuidStr = uuid.ToString();
            Assert.IsNotNull(uuid);
            UUID uuid2 = UUID.RandomUuid();
            Assert.AreNotSame(uuid, uuid2);
        }

        [Test]
        public void ToString_should_override_and_not_hide_base()
        {
            UUID uuid = UUID.RandomUuid();

            string uuidStr = uuid.ToString();
            string uuidConcat = "Test." + uuid;

            Assert.AreEqual("Test." + uuidStr, uuidConcat);
        }

        [Test]
        public void two_uuid_with_same_value_should_have_same_hash_code()
        {
            UUID uuid = UUID.RandomUuid();
            UUID uuid2 = new UUID(uuid.MostSignificantBits, uuid.LeastSignificantBits);
            
            Assert.AreEqual(uuid, uuid2);
            Assert.AreEqual(uuid.GetHashCode(), uuid2.GetHashCode());
        }
    }
}
