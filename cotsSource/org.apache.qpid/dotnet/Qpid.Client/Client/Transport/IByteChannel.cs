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
using Apache.Qpid.Buffer;

namespace Apache.Qpid.Client.Transport
{
   /// <summary>
   /// Represents input/output channels that read
   /// and write <see cref="ByteBuffer"/> instances
   /// </summary>
   public interface IByteChannel
   {
      /// <summary>
      /// Read a <see cref="ByteBuffer"/> from the underlying 
      /// network stream and any configured filters 
      /// </summary>
      /// <returns>A ByteBuffer, if available</returns>
      ByteBuffer Read();
      /// <summary>
      /// Begin an asynchronous read operation
      /// </summary>
      /// <param name="callback">Callback method to call when read operation completes</param>
      /// <param name="state">State object</param>
      /// <returns>An <see cref="System.IAsyncResult"/> object</returns>
      IAsyncResult BeginRead(AsyncCallback callback, object state);
      /// <summary>
      /// End an asynchronous read operation
      /// </summary>
      /// <param name="result">The <see cref="System.IAsyncResult"/> object returned from <see cref="BeginRead"/></param>
      /// <returns>The <see cref="ByteBuffer"/> read</returns>
      ByteBuffer EndRead(IAsyncResult result);
      /// <summary>
      /// Write a <see cref="ByteBuffer"/> to the underlying network 
      /// stream, going through any configured filters
      /// </summary>
      /// <param name="buffer"></param>
      void Write(ByteBuffer buffer);
      /// <summary>
      /// Begin an asynchronous write operation
      /// </summary>
      /// <param name="buffer">Buffer to write</param>
      /// <param name="callback">A callback to call when the operation completes</param>
      /// <param name="state">State object</param>
      /// <returns>An <see cref="System.IAsyncResult"/> object</returns>
      IAsyncResult BeginWrite(ByteBuffer buffer, AsyncCallback callback, object state);
      /// <summary>
      /// End an asynchronous write operation
      /// </summary>
      /// <param name="result">The <see cref="System.IAsyncResult"/> object returned by <see cref="BeginWrite"/></param>
      void EndWrite(IAsyncResult result);
   }
}
