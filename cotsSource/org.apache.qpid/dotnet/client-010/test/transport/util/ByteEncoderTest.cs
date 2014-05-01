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

    public class ByteEncoderTest
    {        
        private static readonly Logger _log = Logger.Get(typeof(ByteEncoderTest));

        [Test]
        public void GetBigEndianInt32()
        {
            _log.Debug("Running: GetBigEndianInt32");
            const int anInt = -12345;
            Int32 aNewInt = ByteEncoder.GetBigEndian(anInt);
            Assert.IsTrue( anInt == ByteEncoder.GetBigEndian(aNewInt) );
        }

        [Test]
        public void GetBigEndianUInt16()
        {
            _log.Debug("Running: GetBigEndianUInt16");
            const UInt16 anInt = 123;
            UInt16 aNewInt = ByteEncoder.GetBigEndian(anInt);
            Assert.IsTrue(anInt == ByteEncoder.GetBigEndian(aNewInt));
        }

        [Test]
        public void GetBigEndianUInt32()
        {
            _log.Debug("Running: GetBigEndianUInt32");
            const UInt32 anInt = 12345;
            UInt32 aNewInt = ByteEncoder.GetBigEndian(anInt);
            Assert.IsTrue(anInt == ByteEncoder.GetBigEndian(aNewInt));
        }

        [Test]
        public void GetBigEndianlong()
        {
            _log.Debug("Running: GetBigEndianlong");
            const long anInt = 123456660700770;
            long aNewInt = ByteEncoder.GetBigEndian(anInt);
            Assert.IsTrue(anInt == ByteEncoder.GetBigEndian(aNewInt));
        }

        [Test]
        public void GetLittleEndianInt32()
        {
            _log.Debug("Running: GetBigEndianInt32");
            const int anInt = -12345;
            Int32 aNewInt = ByteEncoder.GetLittleEndian(anInt);
            Assert.IsTrue(anInt == ByteEncoder.GetLittleEndian(aNewInt));
        }

        [Test]
        public void GetLittleEndianUInt16()
        {
            _log.Debug("Running: GetLittleEndianUInt16");
            const UInt16 anInt = 123;
            UInt16 aNewInt = ByteEncoder.GetLittleEndian(anInt);
            Assert.IsTrue(anInt == ByteEncoder.GetLittleEndian(aNewInt));
        }

        [Test]
        public void GetLittleEndianUInt32()
        {
            _log.Debug("Running: GetLittleEndianUInt32");
            const UInt32 anInt = 12345;
            UInt32 aNewInt = ByteEncoder.GetLittleEndian(anInt);
            Assert.IsTrue(anInt == ByteEncoder.GetLittleEndian(aNewInt));
        }

        [Test]
        public void GetLittleEndianlong()
        {
            _log.Debug("Running: GetLittleEndianlong");
            const long anInt = 123456660700770;
            long aNewInt = ByteEncoder.GetLittleEndian(anInt);
            Assert.IsTrue(anInt == ByteEncoder.GetLittleEndian(aNewInt));
        }
    }
}
