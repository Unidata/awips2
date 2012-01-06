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
using NUnit.Framework;
using org.apache.qpid.transport.util;

namespace test.transport.util
{
    [TestFixture]
    public class SerialTest
    {
        private static readonly Logger _log = Logger.Get(typeof (SerialTest));

        [Test]
        ///
        /// Test the key boundaries where wraparound occurs.
        ///
        public void testBoundaries()
        {
            Assert.IsTrue(Serial.Gt(1, 0));
            Assert.IsTrue(Serial.Lt(0, 1));

            Assert.IsTrue(Serial.Gt(int.MaxValue, int.MaxValue - 1));
            Assert.IsTrue(Serial.Lt(int.MaxValue - 1, int.MaxValue));
        }

        ///
        /// Test the first Corollary of RFC 1982
        /// For any sequence number s and any integer n such that addition of n
        /// to s is well defined, (s + n) >= s.  Further (s + n) == s only when
        /// n == 0, in all other defined cases, (s + n) > s.
        ///
        public void testCorollary1()
        {
            int wrapcount = 0;

            int s = 0;

            for (int i = 0; i < 67108664; i++)
            {
                for (int n = 1; n < 4096; n += 512)
                {
                    Assert.IsTrue(Serial.Gt(s + n, s));
                    Assert.IsTrue(Serial.Lt(s, s + n));
                }

                s += 1024;

                if (s == 0)
                {
                    wrapcount += 1;
                }
            }

            Assert.IsTrue(wrapcount > 0);
        }
    }
}
