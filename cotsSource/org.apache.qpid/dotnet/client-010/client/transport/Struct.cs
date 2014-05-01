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
using System.Collections.Generic;
using System.Text;
using org.apache.qpid.transport.codec;

namespace org.apache.qpid.transport
{
	/// <summary> 
	/// Struct
	/// </summary>

    public abstract class Struct : IEncodable
    {
        public  static Struct Create(int type)
        {
            return StructFactory.create(type);
        }

        bool dirty = true;

        public bool Dirty
        {
            get { return dirty; }
            set { dirty = value; }
        }

        public abstract int GetStructType();

        public abstract int GetSizeWidth();

        public abstract int GetPackWidth();

        public int GetEncodedType()
        {
            int type = GetStructType();
            if (type < 0)
            {
                throw new Exception();
            }
            return type;
        }

        private bool IsBit<C, T>(Field<C, T> f)
        {
            return Equals(f.Type, typeof(Boolean));
        }

        private bool Packed()
        {
            return GetPackWidth() > 0;
        }

        private bool Encoded<C, T>(Field<C, T> f)
        {
            return !Packed() || !IsBit(f) && f.Has(this);
        }

        private int GetFlagWidth()
        {
            return (Fields.Count + 7) / 8;
        }

        private int GetFlagCount()
        {
            return 8 * GetPackWidth();
        }

        public abstract void Read(IDecoder dec);

        public abstract void Write(IEncoder enc);

        public abstract Dictionary<String, Object> Fields
        {
            get;
        }

        public override String ToString()
        {
            StringBuilder str = new StringBuilder();
            str.Append(GetType());
            str.Append("(");
            bool first = true;
            foreach (KeyValuePair<String, Object> me in Fields)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    str.Append(", ");
                }
                str.Append(me.Key);
                str.Append("=");
                str.Append(me.Value);
            }
            str.Append(")");
            return str.ToString();
        }
    }
}
