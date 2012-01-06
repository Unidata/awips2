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

namespace Apache.Qpid.Protocol
{
   public sealed class AMQConstant
   {
      private int _code;
      private string _name;
      private static Hashtable _codeMap = new Hashtable();

      public int Code
      {
         get { return _code; }
      }

      public string Name
      {
         get { return _name; }
      }

      private AMQConstant(int code, string name, bool map)
      {
         _code = code;
         _name = name;

         if ( map )
         {
            _codeMap.Add(code, this);
         }
      }

      public override string ToString()
      {
         return string.Format("{0}: {1}", Code, Name);
      }

      public static AMQConstant GetConstant(int code)
      {
         AMQConstant c = (AMQConstant)_codeMap[code];
         if ( c == null )
         {
            c = new AMQConstant(code, "unknown code", false);
         }
         return c;
      }

      #region Constants
      //
      // Constants
      //
      public static readonly AMQConstant FRAME_MIN_SIZE = new AMQConstant(4096, "frame min size", true);
      public static readonly AMQConstant FRAME_END = new AMQConstant(206, "frame end", true);
      public static readonly AMQConstant REPLY_SUCCESS = new AMQConstant(200, "reply success", true);
      public static readonly AMQConstant NOT_DELIVERED = new AMQConstant(310, "not delivered", true);
      public static readonly AMQConstant MESSAGE_TOO_LARGE = new AMQConstant(311, "message too large", true);
      public static readonly AMQConstant NO_ROUTE = new AMQConstant(312, "no route", true);
      public static readonly AMQConstant NO_CONSUMERS = new AMQConstant(313, "no consumers", true);
      public static readonly AMQConstant CONTEXT_IN_USE = new AMQConstant(320, "context in use", true);
      public static readonly AMQConstant INVALID_PATH = new AMQConstant(402, "invalid path", true);
      public static readonly AMQConstant ACCESS_REFUSED = new AMQConstant(403, "access refused", true);
      public static readonly AMQConstant NOT_FOUND = new AMQConstant(404, "not found", true);
      public static readonly AMQConstant ALREADY_EXISTS = new AMQConstant(405, "already exists", true);
      public static readonly AMQConstant IN_USE = new AMQConstant(406, "in use", true);
      public static readonly AMQConstant INVALID_ROUTING_KEY = new AMQConstant(407, "routing key invalid", true);
      public static readonly AMQConstant REQUEST_TIMEOUT = new AMQConstant(408, "request timeout", true);
      public static readonly AMQConstant INVALID_ARGUMENT = new AMQConstant(409, "argument invalid", true);
      public static readonly AMQConstant FRAME_ERROR = new AMQConstant(501, "frame error", true);
      public static readonly AMQConstant SYNTAX_ERROR = new AMQConstant(502, "syntax error", true);
      public static readonly AMQConstant COMMAND_INVALID = new AMQConstant(503, "command invalid", true);
      public static readonly AMQConstant CHANNEL_ERROR = new AMQConstant(504, "channel error", true);
      public static readonly AMQConstant RESOURCE_ERROR = new AMQConstant(506, "resource error", true);
      public static readonly AMQConstant NOT_ALLOWED = new AMQConstant(530, "not allowed", true);
      public static readonly AMQConstant NOT_IMPLEMENTED = new AMQConstant(540, "not implemented", true);
      public static readonly AMQConstant INTERNAL_ERROR = new AMQConstant(541, "internal error", true);

      #endregion // Constants

   }
}
