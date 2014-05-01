/*
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
*/

namespace Apache.Qpid.AmqpTypes
{
    using System;
    using System.IO;
    using System.Collections.Generic;
    using System.Text;

    // for big strings: str16 in 0-10 and str32 in 1.0

    public class AmqpString : AmqpType
    {
        string value;
        Encoding encoding;

        public AmqpString(string s)
        {
            this.value = s;
            this.encoding = Encoding.UTF8;
        }

        public AmqpString(string s, Encoding enc)
        {
            ValidateEncoding(enc);
            this.value = s;
            this.encoding = enc;
        }

        public Encoding Encoding
        {
            get { return encoding; }
            set
            {
                ValidateEncoding(value);
                encoding = value;
            }
        }

        private void ValidateEncoding(Encoding enc)
        {
            if (value == null)
            {
                throw new ArgumentNullException("Encoding");
            }

            if ((enc != Encoding.UTF8) && (enc != Encoding.Unicode))
            {
                throw new ArgumentException("Encoding not one of UTF8 or Unicode");
            }
        }

        public override void Encode(byte[] bufer, int offset, int count)
        {
            throw new NotImplementedException();
        }

        public override int EncodedSize
        {
            get { throw new NotImplementedException(); }
        }

        public override AmqpType Clone()
        {
            return new AmqpString(this.value);
        }

        public string Value
        {
            get { return this.value; }
            set { this.value = value; }
        }
    }
}
