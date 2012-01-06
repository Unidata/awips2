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
using System.Collections;
using System.Collections.Specialized;
using System.Globalization;
using System.Security.Cryptography;
using System.Text;

namespace Apache.Qpid.Sasl.Mechanisms
{

   /// <summary>
   /// Implements the DIGEST MD5 authentication mechanism
   /// as outlined in RFC 2831
   /// </summary>
   public class DigestSaslClient : SaslClient
   {
      public const string Mechanism = "DIGEST-MD5";
      private static readonly MD5 _md5 = new MD5CryptoServiceProvider();
      private int _state;
      private string _cnonce;
      private Encoding _encoding = Encoding.UTF8;

      public string Cnonce
      {
         get { return _cnonce; }
         set { _cnonce = value; }
      }

      public DigestSaslClient(
         string authid, string serverName, string protocol, 
         IDictionary properties, ISaslCallbackHandler handler)
         : base(authid, serverName, protocol, properties, handler)
      {
         _cnonce = Guid.NewGuid().ToString("N");
      }

      #region ISaslClient Implementation
      //
      // ISaslClient Implementation
      //

      public override string MechanismName
      {
         get { return Mechanism; }
      }

      public override bool HasInitialResponse
      {
         get { return false; }
      }

      public override byte[] EvaluateChallenge(byte[] challenge)
      {
         if ( challenge == null || challenge.Length <= 0 )
            throw new ArgumentNullException("challenge");

         switch ( _state++ )
         {
         case 0: return OnInitialChallenge(challenge);
         case 1: return OnFinalResponse(challenge);
         }
         throw new SaslException("Invalid State for authentication");
      }

      #endregion // ISaslClient Implementation

      
      #region Private Methods
      //
      // Private Methods
      //

      /// <summary>
      /// Process the first challenge from the server
      /// and calculate a response
      /// </summary>
      /// <param name="challenge">The server issued challenge</param>
      /// <returns>Client response</returns>
      private byte[] OnInitialChallenge(byte[] challenge)
      {
         DigestChallenge dch = 
            DigestChallenge.Parse(_encoding.GetString(challenge));
         // validate input challenge
         if ( dch.Nonce == null || dch.Nonce.Length == 0 )
            throw new SaslException("Nonce value missing in server challenge");
         if ( dch.Algorithm != "md5-sess" )
            throw new SaslException("Invalid or missing algorithm value in server challenge");


         NameCallback nameCB = new NameCallback(AuthorizationId);
         PasswordCallback pwdCB = new PasswordCallback();
         RealmCallback realmCB = new RealmCallback(dch.Realm);
         ISaslCallback[] callbacks = { nameCB, pwdCB, realmCB };
         Handler.Handle(callbacks);

         DigestResponse response = new DigestResponse();
         response.Username = nameCB.Text;
         response.Realm = realmCB.Text;
         response.Nonce = dch.Nonce;
         response.Cnonce = Cnonce;
         response.NonceCount = 1;
         response.Qop = DigestQop.Auth; // only auth supported for now
         response.DigestUri = Protocol.ToLower() + "/" + ServerName;
         response.MaxBuffer = dch.MaxBuffer;
         response.Charset = dch.Charset;
         response.Cipher = null; // not supported for now
         response.Authzid = AuthorizationId;
         response.AuthParam = dch.AuthParam;

         response.Response = CalculateResponse(
            nameCB.Text, realmCB.Text, pwdCB.Text, 
            dch.Nonce, response.NonceCount, response.Qop, response.DigestUri
            );

         return _encoding.GetBytes(response.ToString());
      }

      /// <summary>
      /// Process the second server challenge
      /// </summary>
      /// <param name="challenge">Server issued challenge</param>
      /// <returns>The client response</returns>
      private byte[] OnFinalResponse(byte[] challenge)
      {
         DigestChallenge dch = 
            DigestChallenge.Parse(_encoding.GetString(challenge));

         if ( dch.Rspauth == null || dch.Rspauth.Length == 0 )
            throw new SaslException("Expected 'rspauth' in server challenge not found");

         SetComplete();
         return new byte[0];
      }

      
      
      /// <summary>
      /// Calculate the response field of the client response
      /// </summary>
      /// <param name="username">The user name</param>
      /// <param name="realm">The realm</param>
      /// <param name="passwd">The user's password</param>
      /// <param name="nonce">Server nonce value</param>
      /// <param name="nc">Client nonce count (always 1)</param>
      /// <param name="qop">Quality of Protection</param>
      /// <param name="digestUri">Digest-URI</param>
      /// <returns>The value for the response field</returns>
      private string CalculateResponse(
         string username, string realm, string passwd, 
         string nonce, int nc, string qop, string digestUri
         )
      {
         string a1 = CalcHexA1(username, realm, passwd, nonce);
         string a2 = CalcHexA2(digestUri, qop);

         string ncs = nc.ToString("x8", CultureInfo.InvariantCulture);
         StringBuilder prekd = new StringBuilder();
         prekd.Append(a1).Append(':').Append(nonce).Append(':')
            .Append(ncs).Append(':').Append(Cnonce)
            .Append(':').Append(qop).Append(':').Append(a2);

         return ToHex(CalcH(_encoding.GetBytes(prekd.ToString())));
      }

      private string CalcHexA1(
         string username, string realm, 
         string passwd, string nonce
         )
      {
         bool hasAuthId = AuthorizationId != null && AuthorizationId.Length > 0;

         string premd = username + ":" + realm + ":" + passwd;
         byte[] temp1 = CalcH(_encoding.GetBytes(premd));
         

         int a1len = 16 + 1 + nonce.Length + 1 + Cnonce.Length;
         if ( hasAuthId )
            a1len += 1 + AuthorizationId.Length;

         byte[] buffer = new byte[a1len];
         Array.Copy(temp1, buffer, temp1.Length);

         string p2 = ":" + nonce + ":" + Cnonce;
         if ( hasAuthId )
            p2 += ":" + AuthorizationId;

         byte[] temp2 = _encoding.GetBytes(p2);
         Array.Copy(temp2, 0, buffer, 16, temp2.Length);

         return ToHex(CalcH(buffer));
      }

      private string CalcHexA2(string digestUri, string qop)
      {
         string a2 = "AUTHENTICATE:" + digestUri;
         if ( qop != DigestQop.Auth )
            a2 += ":00000000000000000000000000000000";
         return ToHex(CalcH(_encoding.GetBytes(a2)));
      }

      private static byte[] CalcH(byte[] value)
      {
         return _md5.ComputeHash(value);
      }

      #endregion // Private Methods


   } // class DigestSaslClient


   /// <summary>
   /// Available QOP options in the DIGEST scheme
   /// </summary>
   public sealed class DigestQop
   {
      public const string Auth = "auth";
      public const string AuthInt = "auth-int";
      public const string AuthConf = "auth-conf";
   } // class DigestQop


   /// <summary>
   /// Represents and parses a digest server challenge
   /// </summary>
   public class DigestChallenge
   {
      private string _realm = "localhost";
      private string _nonce;
      private string[] _qopOptions = { DigestQop.Auth };
      private bool _stale;
      private int _maxBuffer = 65536;
      private string _charset = "ISO 8859-1";
      private string _algorithm;
      private string[] _cipherOptions;
      private string _authParam;
      private string _rspauth;

      #region Properties
      //
      // Properties
      //

      public string Realm
      {
         get { return _realm; }
      }

      public string Nonce
      {
         get { return _nonce; }
      }

      public string[] QopOptions
      {
         get { return _qopOptions; }
      }

      public bool Stale
      {
         get { return _stale; }
      }

      public int MaxBuffer
      {
         get { return _maxBuffer; }
         set { _maxBuffer = value; }
      }

      public string Charset
      {
         get { return _charset; }
      }

      public string Algorithm
      {
         get { return _algorithm; }
      }

      public string[] CipherOptions
      {
         get { return _cipherOptions; }
      }

      public string AuthParam
      {
         get { return _authParam; }
      }

      public string Rspauth
      {
         get { return _rspauth; }
      }

      #endregion // Properties

      public static DigestChallenge Parse(string challenge)
      {
         DigestChallenge parsed = new DigestChallenge();
         StringDictionary parts = ParseParameters(challenge);
         foreach ( string optname in parts.Keys )
         {
            switch ( optname )
            {
            case "realm":
               parsed._realm = parts[optname];
               break;
            case "nonce":
               parsed._nonce = parts[optname];
               break;
            case "qop-options":
               parsed._qopOptions = GetOptions(parts[optname]);
               break;
            case "cipher-opts":
               parsed._cipherOptions = GetOptions(parts[optname]);
               break;
            case "stale":
               parsed._stale = Convert.ToBoolean(parts[optname], CultureInfo.InvariantCulture);
               break;
            case "maxbuf":
               parsed._maxBuffer = Convert.ToInt32(parts[optname], CultureInfo.InvariantCulture);
               break;
            case "charset":
               parsed._charset = parts[optname];
               break;
            case "algorithm":
               parsed._algorithm = parts[optname];
               break;
            case "auth-param":
               parsed._authParam = parts[optname];
               break;
            case "rspauth":
               parsed._rspauth = parts[optname];
               break;
            }
         }

         return parsed;
      }


      public static StringDictionary ParseParameters(string source)
      {
         if ( source == null )
            throw new ArgumentNullException("source");

         StringDictionary ret = new StringDictionary();

         string remaining = source.Trim();
         while ( remaining.Length > 0 )
         {
            int equals = remaining.IndexOf('=');
            if ( equals < 0 )
               break;

            string optname = remaining.Substring(0, equals).Trim();
            remaining = remaining.Substring(equals + 1);

            string value = ParseQuoted(ref remaining);
            ret[optname] = value.Trim();
         }
         return ret;
      }

      private static string ParseQuoted(ref string str)
      {
         string ns = str.TrimStart();

         int start = 0;
         bool quoted = ns[0] == '\"';
         if ( quoted ) start++;
         bool inquotes = quoted;
         bool escaped = false;

         int pos = start;
         for ( ; pos < ns.Length; pos++ )
         {
            if ( !inquotes && ns[pos] == ',' )
               break;

            // at end of quotes?
            if ( quoted && !escaped && ns[pos] == '\"' ) 
               inquotes = false;
            // is this char an escape for the next one?
            escaped = inquotes && ns[pos] == '\\';
         }
         // pos has end of string
         string value = ns.Substring(start, pos-start).Trim();
         if ( quoted )
         {
            // remove trailing quote
            value = value.Substring(0, value.Length - 1);
         }
         str = ns.Substring(pos < ns.Length-1 ? pos+1 : pos);
         return value;
      }

      private static string[] GetOptions(string value)
      {
         return value.Split(' ');
      }

   } // class DigestChallenge


   /// <summary>
   /// Represents and knows how to write a 
   /// digest client response
   /// </summary>
   public class DigestResponse
   {
      private string _username;
      private string _realm;
      private string _nonce;
      private string _cnonce;
      private int _nonceCount;
      private string _qop;
      private string _digestUri;
      private string _response;
      private int _maxBuffer;
      private string _charset;
      private string _cipher;
      private string _authzid;
      private string _authParam;

      #region Properties
      //
      // Properties
      //

      public string Username
      {
         get { return _username; }
         set { _username = value; }
      }

      public string Realm
      {
         get { return _realm; }
         set { _realm = value; }
      }

      public string Nonce
      {
         get { return _nonce; }
         set { _nonce = value; }
      }

      public string Cnonce
      {
         get { return _cnonce; }
         set { _cnonce = value; }
      }

      public int NonceCount
      {
         get { return _nonceCount; }
         set { _nonceCount = value; }
      }

      public string Qop
      {
         get { return _qop; }
         set { _qop = value; }
      }

      public string DigestUri
      {
         get { return _digestUri; }
         set { _digestUri = value; }
      }

      public string Response
      {
         get { return _response; }
         set { _response = value; }
      }

      public int MaxBuffer
      {
         get { return _maxBuffer; }
         set { _maxBuffer = value; }
      }

      public string Charset
      {
         get { return _charset; }
         set { _charset = value; }
      }

      public string Cipher
      {
         get { return _cipher; }
         set { _cipher = value; }
      }

      public string Authzid
      {
         get { return _authzid; }
         set { _authzid = value; }
      }

      public string AuthParam
      {
         get { return _authParam; }
         set { _authParam = value; }
      }

      #endregion // Properties


      public override string ToString()
      {
         StringBuilder buffer = new StringBuilder();
         Pair(buffer, "username", Username, true);
         Pair(buffer, "realm", Realm, true);
         Pair(buffer, "nonce", Nonce, true);
         Pair(buffer, "cnonce", Cnonce, true);
         string nc = NonceCount.ToString("x8", CultureInfo.InvariantCulture);
         Pair(buffer, "nc", nc, false);
         Pair(buffer, "qop", Qop, false);
         Pair(buffer, "digest-uri", DigestUri, true);
         Pair(buffer, "response", Response, true);
         string maxBuffer = MaxBuffer.ToString(CultureInfo.InvariantCulture);
         Pair(buffer, "maxbuf", maxBuffer, false);
         Pair(buffer, "charset", Charset, false);
         Pair(buffer, "cipher", Cipher, false);
         Pair(buffer, "authzid", Authzid, true);
         Pair(buffer, "auth-param", AuthParam, true);

         return buffer.ToString().TrimEnd(',');
      }

      private static void Pair(StringBuilder buffer, string name, string value, bool quoted)
      {
         if ( value != null && value.Length > 0 )
         {
            buffer.Append(name);
            buffer.Append('=');
            if ( quoted )
            {
               buffer.Append('\"');
               buffer.Append(value.Replace("\"", "\\\""));
               buffer.Append('\"');
            } else
            {
               buffer.Append(value);
            }
            buffer.Append(',');
         }
      }

   } // class DigestResponse

} // namespace Apache.Qpid.Sasl.Mechanisms
