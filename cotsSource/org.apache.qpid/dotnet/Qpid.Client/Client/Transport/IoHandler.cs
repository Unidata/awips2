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
using System.IO;
using System.Threading;
using log4net;
using Apache.Qpid.Buffer;
using Apache.Qpid.Client.Protocol;

namespace Apache.Qpid.Client.Transport
{
   /// <summary>
   /// Responsible for reading and writing
   /// ByteBuffers from/to network streams, and handling
   /// the stream filters
   /// </summary>
   public class IoHandler : IByteChannel, IDisposable
   {
      private static readonly ILog _log = LogManager.GetLogger(typeof(IoHandler));
      private const int DEFAULT_BUFFER_SIZE = 32 * 1024;

      private Stream _topStream;
      private IProtocolListener _protocolListener;
      private int _readBufferSize;

      public int ReadBufferSize
      {
         get { return _readBufferSize; }
         set { _readBufferSize = value; }
      }

      /// <summary>
      /// Initialize a new instance
      /// </summary>
      /// <param name="stream">Underlying network stream</param>
      /// <param name="protocolListener">Protocol listener to report exceptions to</param>
      public IoHandler(Stream stream, IProtocolListener protocolListener)
      {
         if ( stream == null )
            throw new ArgumentNullException("stream");
         if ( protocolListener == null )
            throw new ArgumentNullException("protocolListener");

         // initially, the stream at the top of the filter 
         // chain is the underlying network stream
         _topStream = stream;
         _protocolListener = protocolListener;
         _readBufferSize = DEFAULT_BUFFER_SIZE;
      }

      /// <summary>
      /// Adds a new filter on the top of the chain
      /// </summary>
      /// <param name="filter">Stream filter to put on top of the chain</param>
      /// <remarks>
      /// This should *only* be called during initialization. We don't
      /// support changing the filter change after the first read/write
      /// has been done and it's not thread-safe to boot!
      /// </remarks>
      public void AddFilter(IStreamFilter filter)
      {
         _topStream = filter.CreateFilterStream(_topStream);
      }

      #region IByteChannel Implementation
      //
      // IByteChannel Implementation
      //

      /// <summary>
      /// Read a <see cref="ByteBuffer"/> from the underlying 
      /// network stream and any configured filters 
      /// </summary>
      /// <returns>A ByteBuffer, if available</returns>
      public ByteBuffer Read()
      {
         byte[] bytes = AllocateBuffer();

         int numOctets = _topStream.Read(bytes, 0, bytes.Length);

         return WrapByteArray(bytes, numOctets);
      }

      /// <summary>
      /// Begin an asynchronous read operation
      /// </summary>
      /// <param name="callback">Callback method to call when read operation completes</param>
      /// <param name="state">State object</param>
      /// <returns>An <see cref="System.IAsyncResult"/> object</returns>
      public IAsyncResult BeginRead(AsyncCallback callback, object state)
      {
         byte[] bytes = AllocateBuffer();
         ReadData rd = new ReadData(callback, state, bytes);
         
         // only put a callback if the caller wants one.
         AsyncCallback myCallback = null;
         if ( callback != null )
            myCallback = new AsyncCallback(OnAsyncReadDone);

         IAsyncResult result = _topStream.BeginRead(
            bytes, 0, bytes.Length, myCallback,rd
            );
         return new WrappedAsyncResult(result, bytes);
      }

      /// <summary>
      /// End an asynchronous read operation
      /// </summary>
      /// <param name="result">The <see cref="System.IAsyncResult"/> object returned from <see cref="BeginRead"/></param>
      /// <returns>The <see cref="ByteBuffer"/> read</returns>
      public ByteBuffer EndRead(IAsyncResult result)
      {
         WrappedAsyncResult theResult = (WrappedAsyncResult)result;
         int bytesRead = _topStream.EndRead(theResult.InnerResult);
         return WrapByteArray(theResult.Buffer, bytesRead);
      }

      /// <summary>
      /// Write a <see cref="ByteBuffer"/> to the underlying network 
      /// stream, going through any configured filters
      /// </summary>
      /// <param name="buffer"></param>
      public void Write(ByteBuffer buffer)
      {
         try
         {
            _topStream.Write(buffer.Array, buffer.Position, buffer.Limit); // FIXME
         } 
         catch (Exception e)
         {
            _log.Warn("Write caused exception", e);
            _protocolListener.OnException(e);
         }
      }

      /// <summary>
      /// Begin an asynchronous write operation
      /// </summary>
      /// <param name="buffer">Buffer to write</param>
      /// <param name="callback">A callback to call when the operation completes</param>
      /// <param name="state">State object</param>
      /// <returns>An <see cref="System.IAsyncResult"/> object</returns>
      public IAsyncResult BeginWrite(ByteBuffer buffer, AsyncCallback callback, object state)
      {
         try 
         {
            return _topStream.BeginWrite(
               buffer.Array, buffer.Position, buffer.Limit,
               callback, state
               );
         } catch ( Exception e )
         {
            _log.Error("BeginWrite caused exception", e);
            // not clear if an exception here should be propagated? we still
            // need to propagate it upwards anyway!
            _protocolListener.OnException(e);
            throw;
         }
      }

      /// <summary>
      /// End an asynchronous write operation
      /// </summary>
      /// <param name="result">The <see cref="System.IAsyncResult"/> object returned by <see cref="BeginWrite"/></param>
      public void EndWrite(IAsyncResult result)
      {
         try
         {
            _topStream.EndWrite(result);
         } catch ( Exception e )
         {
            _log.Error("EndWrite caused exception", e);
            // not clear if an exception here should be propagated? 
            _protocolListener.OnException(e);
            //throw;
         }
      }
      #endregion // IByteChannel Implementation

      #region IDisposable Implementation
      //
      // IDisposable Implementation
      //

      public void Dispose()
      {
         if ( _topStream != null )
         {
            _topStream.Close();
         }
      }

      #endregion // IDisposable Implementation

      #region Private and Helper Classes/Methods
      //
      // Private and Helper Classes/Methods
      //

      private byte[] AllocateBuffer()
      {
         return new byte[ReadBufferSize];
      }

      private static ByteBuffer WrapByteArray(byte[] bytes, int size)
      {
         ByteBuffer byteBuffer = ByteBuffer.Wrap(bytes);
         byteBuffer.Limit = size;
         byteBuffer.Flip();

         return byteBuffer;
      }


      private static void OnAsyncReadDone(IAsyncResult result)
      {
         ReadData rd = (ReadData) result.AsyncState;
         IAsyncResult wrapped = new WrappedAsyncResult(result, rd.Buffer);
         rd.Callback(wrapped);
      }

      class ReadData
      {
         private object _state;
         private AsyncCallback _callback;
         private byte[] _buffer;

         public object State
         {
            get { return _state; }
         }

         public AsyncCallback Callback
         {
            get { return _callback; }
         }

         public byte[] Buffer
         {
            get { return _buffer; }
         }

         public ReadData(AsyncCallback callback, object state, byte[] buffer)
         {
            _callback = callback;
            _state = state;
            _buffer = buffer;
         }
      }

      class WrappedAsyncResult : IAsyncResult
      {
         private IAsyncResult _innerResult;
         private byte[] _buffer;

         #region IAsyncResult Properties
         //
         // IAsyncResult Properties
         //
         public bool IsCompleted
         {
            get { return _innerResult.IsCompleted; }
         }

         public WaitHandle AsyncWaitHandle
         {
            get { return _innerResult.AsyncWaitHandle; }
         }

         public object AsyncState
         {
            get { return _innerResult.AsyncState; }
         }

         public bool CompletedSynchronously
         {
            get { return _innerResult.CompletedSynchronously; }
         }
         #endregion // IAsyncResult Properties

         public IAsyncResult InnerResult
         {
            get { return _innerResult; }
         }
         public byte[] Buffer
         {
            get { return _buffer; }
         }

         public WrappedAsyncResult(IAsyncResult result, byte[] buffer)
         {
            if ( result == null )
               throw new ArgumentNullException("result");
            if ( buffer == null )
               throw new ArgumentNullException("buffer");

            _innerResult = result;
            _buffer = buffer;
         }
      }

      #endregion // Private and Helper Classes/Methods
   }
}
