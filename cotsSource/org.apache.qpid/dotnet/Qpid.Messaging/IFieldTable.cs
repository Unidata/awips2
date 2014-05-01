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
using System.Collections;

namespace Apache.Qpid.Messaging
{
    public interface IFieldTable : IEnumerable
    {
        int Count { get; }

        object this[string key] { get; set; }

        /// <summary>
        /// Adds all the items from another field table in this one. 
        /// Will overwrite any items in the current table with the same key.
        /// </summary>
        /// <param name="source">the source field table</param>
        void AddAll(IFieldTable source);

        bool Contains(string s);
        void Clear();
        void Remove(string key);
    }
}
