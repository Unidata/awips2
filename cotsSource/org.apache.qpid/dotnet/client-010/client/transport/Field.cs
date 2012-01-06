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
using org.apache.qpid.transport.codec;

namespace org.apache.qpid.transport
{
    /// <summary> 
    /// Field
    /// </summary>
    public abstract class Field<C, T>
    {
        private C container;
        private T type;
        private String name;
        private int index;

        protected Field(C container, T type, String name, int index)
        {
            this.container = container;
            this.type = type;
            this.name = name;
            this.index = index;
        }

        public C Container
        {
            get { return container; }
        }

        public T Type
        {
            get { return type; }
        }

        public String Name
        {
            get { return name; }
        }

        public int Index
        {
            get { return index; }
        }

        public abstract bool Has(Object mystruct);

        public abstract void Has(Object mystruct, bool value);

        public abstract T Get(Object mystruct);

        public abstract void Read(IDecoder dec, Object mystruct);

        public abstract void Write(IEncoder enc, Object mystruct);
    }
}
