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

namespace org.apache.qpid.transport
{
    /// <summary> 
    /// Header
    /// </summary>
    public class Header
    {
        private readonly Struct[] _mystructs;

        public Header(List<Struct> structs)
            : this(structs.ToArray())
        {
        }

        public Header(params Struct[] structs)
        {
            _mystructs = structs;
        }

        public Struct[] Structs
        {
            get { return _mystructs; }
        }


        public Struct Get(Struct klass)
        {
            foreach (Struct st in _mystructs)
            {
                if (Equals(st.GetType(), klass.GetType()))
                {
                    return st;
                }
            }
            return null;
        }

        public override String ToString()
        {
            StringBuilder str = new StringBuilder();
            str.Append(" Header(");
            bool first = true;
            foreach (Struct s in _mystructs)
            {
                if (first)
                {
                    first = false;
                }
                else
                {
                    str.Append(", ");
                }
                str.Append(s);
            }
            str.Append(")");
            return str.ToString();
        }
    }
}
