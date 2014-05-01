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

namespace org.apache.qpid.transport.util
{
    public class UUID
    {
        private long _mostSigBits;
        private long _leastSigBits;
        private static readonly Random _random = new Random();
        private static readonly object _randomLock = new object();


        public UUID(long mostSigBits, long leastSigBits)
        {
            _mostSigBits = mostSigBits;
            _leastSigBits = leastSigBits;
        }

        public long MostSignificantBits
        {
            get { return _mostSigBits; }
            set { _mostSigBits = value; }
        }

        public long LeastSignificantBits
        {
            get { return _leastSigBits; }
            set { _leastSigBits = value; }
        }

        internal UUID(byte[] r)
        {
            MostSignificantBits = 0;
            LeastSignificantBits = 0;
            for (int i = 0; i < 8; i++)
                MostSignificantBits = (MostSignificantBits << 8) | (r[i] & 0xff);
            for (int i = 8; i < 16; i++)
                LeastSignificantBits = (LeastSignificantBits << 8) | (r[i] & 0xff); 
        }

        public static UUID RandomUuid()
        {
            byte[] randomBytes = new byte[16];
            lock (_randomLock)
            {
                _random.NextBytes(randomBytes);
            }

            randomBytes[6] &= 0x0f;
            randomBytes[6] |= 0x40;
            randomBytes[8] &= 0x3f;
            randomBytes[8] |= 0x80;

            return new UUID(randomBytes);
        }
       

        public override String ToString()
        {
            return (Digits(_mostSigBits >> 32, 8) + "-" +
                    Digits(_mostSigBits >> 16, 4) + "-" +
                    Digits(_mostSigBits, 4) + "-" +
                    Digits(_leastSigBits >> 48, 4) + "-" +
                    Digits(_leastSigBits, 12));
        }

        private static String Digits(long val, int digits)
        {
            long hi = 1L << (digits * 4);
            return Convert.ToString((hi | (val & (hi - 1))), 16);
        }

        #region equality
        public bool Equals(UUID other)
        {
            if (ReferenceEquals(null, other)) return false;
            if (ReferenceEquals(this, other)) return true;
            return other._mostSigBits == _mostSigBits && other._leastSigBits == _leastSigBits;
        }

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != typeof (UUID)) return false;
            return Equals((UUID) obj);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                return (_mostSigBits.GetHashCode()*397) ^ _leastSigBits.GetHashCode();
            }
        }

        public static bool operator ==(UUID left, UUID right)
        {
            return Equals(left, right);
        }

        public static bool operator !=(UUID left, UUID right)
        {
            return !Equals(left, right);
        }
        #endregion
    }
}
