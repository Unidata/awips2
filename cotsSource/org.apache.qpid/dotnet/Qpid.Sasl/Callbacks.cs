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
using System.Text;
using System.Globalization;
using System.Security.Cryptography;

namespace Apache.Qpid.Sasl
{
   /// <summary>
   /// Marker interface for Sasl Callbacks
   /// </summary>
   public interface ISaslCallback
   {
   } // interface ISaslCallback

   public abstract class TextSaslCallback : ISaslCallback
   {
      private string _prompt;
      private string _text;
      private string _defaultText;

      public string Prompt
      {
         get { return _prompt; }
         set { _prompt = value; }
      }

      public string Text
      {
         get {
            if ( _text == null || _text.Length == 0 )
               return DefaultText;
            else
               return _text;
         }
         set { _text = value; }
      }

      public string DefaultText
      {
         get { return _defaultText; }
         set { _defaultText = value; }
      }

      protected TextSaslCallback(string prompt, string text, string defaultText)
      {
         _prompt = prompt;
         _text = text;
         _defaultText = defaultText;
      }

   } // class TextSaslCallback

   public class NameCallback : TextSaslCallback
   {
      public NameCallback()
         : this(Environment.UserName)
      {
      }
      public NameCallback(string defaultText)
         : base("username:", "", defaultText)
      {
      }
   } // class NameCallback

   public class PasswordCallback : TextSaslCallback
   {
      public PasswordCallback()
         : base("password:", "", "")
      {
      }

      public byte[] HashedText
      {
          get
          {
            string _text = this.Text;
            System.Security.Cryptography.MD5CryptoServiceProvider x = new System.Security.Cryptography.MD5CryptoServiceProvider();
            byte[] bs = x.ComputeHash(Encoding.UTF8.GetBytes(_text));
            return bs;
          }

      }
   } // class PasswordCallback

   public class HashedPasswordCallback : TextSaslCallback
   {
       public HashedPasswordCallback()
           : base("password:", "", "")
       {
       }

       public byte[] HashedText
       {
        get {
               string _text = this.Text;
               System.Security.Cryptography.MD5CryptoServiceProvider x = new System.Security.Cryptography.MD5CryptoServiceProvider();
               _text = _text.PadRight(16, '\0');
               byte[] bs = x.ComputeHash(Encoding.UTF8.GetBytes(_text));
               return bs;
        }
       }
   } // class PasswordCallback

   public class RealmCallback : TextSaslCallback
   {
      public RealmCallback()
         : this("localhost")
      {
      }
      public RealmCallback(string defaultText)
         : base("realm:", "", defaultText)
      {
      }
   } // class RealmCallback

} // namespace Apache.Qpid.Sasl


