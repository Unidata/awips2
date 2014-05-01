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
using System.IO;

namespace Apache.Qpid.Client.Transport
{
   /// <summary>
   /// Defines a way to introduce an arbitrary filtering
   /// stream into the stream chain managed by <see cref="IoHandler"/>
   /// </summary>
   public interface IStreamFilter
   {
      /// <summary>
      /// Creates a new filtering stream on top of another
      /// </summary>
      /// <param name="lowerStream">Next stream on the stack</param>
      /// <returns>A new filtering stream</returns>
      Stream CreateFilterStream(Stream lowerStream);
   }
}
