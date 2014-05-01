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
using System.Security.Cryptography;

namespace Apache.Qpid.Sasl
{
   /// <summary>
   /// Rough HMAC MD5 implementation as presented in
   /// RFC 2104. Used because the HMACMD5 class in the 
   /// .NET framework is not available in v1.1.
   /// </summary>
   public sealed class MD5HMAC : IDisposable
   {
      private const int BLOCK_LEN = 64;
      private MD5 _hash;
      private byte[] _key;
      private byte[] _ipad;
      private byte[] _opad;

      public MD5HMAC(byte[] key)
      {
         if ( key == null || key.Length == 0 )
            throw new ArgumentNullException("key");

         _hash = new MD5CryptoServiceProvider();

         byte[] theKey = key;
         if ( theKey.Length > BLOCK_LEN )
         {
            theKey = _hash.ComputeHash(theKey);
         }
         // pad key with 0's up to BLOCK_LEN
         _key = new byte[BLOCK_LEN];
         Array.Copy(theKey, _key, theKey.Length);

         CreatePads();
      }

      public byte[] ComputeHash(byte[] input)
      {
         // H(K XOR opad, H(K XOR ipad, text))
         return H(_opad, H(_ipad, input));
      }

      public void Dispose()
      {
         if ( _hash != null )
         {
            ((IDisposable)_hash).Dispose();
            _hash = null;
         }
      }

      #region Private Methods
      //
      // Private Methods
      //

      private void CreatePads()
      {
         _ipad = new byte[BLOCK_LEN];
         _opad = new byte[BLOCK_LEN];
         for ( int i = 0; i < BLOCK_LEN; i++ )
         {
            _ipad[i] = 0x36;
            _opad[i] = 0x5c;
         }

         XOR(_ipad, _key);
         XOR(_opad, _key);
      }

      private static void XOR(byte[] dest, byte[] other)
      {
         // assume both are same size
         for ( int i = 0; i < dest.Length; i++ )
         {
            dest[i] ^= other[i];
         }
      }

      private byte[] H(byte[] v1, byte[] v2)
      {
         byte[] total = new byte[v1.Length + v2.Length];
         Array.Copy(v1, total, v1.Length);
         Array.Copy(v2, 0, total, v1.Length, v2.Length);

         return _hash.ComputeHash(total);
      }

      #endregion // Private Methods

   } // class MD5HMAC

} // namespace Apache.Qpid.Sasl
