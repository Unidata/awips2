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
using log4net;
using Apache.Qpid.Buffer;
using Apache.Qpid.Messaging;

namespace Apache.Qpid.Framing
{
   public class BasicContentHeaderProperties : IContentHeaderProperties
   {
      private static readonly ILog _log = LogManager.GetLogger(typeof(BasicContentHeaderProperties));

      private string _contentType;
      private string _encoding;
      private FieldTable _headers;
      private byte _deliveryMode;
      private byte _priority;
      private string _correlationId;
      private long _expiration;
      private string _replyTo;
      private string _messageId;
      private ulong _timestamp;
      private string _type;
      private string _userId;
      private string _appId;
      private string _clusterId;


      #region Properties
      //
      // Properties
      //

      /// <summary>
      /// The MIME Content Type
      /// </summary>
      public string ContentType
      {
         get { return _contentType; }
         set { _contentType = value; }
      }

      /// <summary>
      /// The MIME Content Encoding
      /// </summary>
      public string Encoding
      {
         get { return _encoding; }
         set { _encoding = value; }
      }

      /// <summary>
      /// Message headers
      /// </summary>
      public FieldTable Headers
      {
         get { return _headers; }
         set { _headers = value; }
      }

      /// <summary>
      /// Non-persistent (1) or persistent (2)
      /// </summary>
      public byte DeliveryMode
      {
         get { return _deliveryMode; }
         set { _deliveryMode = value; }
      }

      /// <summary>
      /// The message priority, 0 to 9
      /// </summary>
      public byte Priority
      {
         get { return _priority; }
         set { _priority = value; }
      }

      /// <summary>
      /// The application correlation identifier
      /// </summary>
      public string CorrelationId
      {
         get { return _correlationId; }
         set { _correlationId = value; }
      }

      /// <summary>
      /// Message expiration specification
      /// </summary>
      // TODO: Should be string according to spec
      public long Expiration
      {
         get { return _expiration; }
         set { _expiration = value; }
      }

      /// <summary>
      /// The destination to reply to
      /// </summary>
      public string ReplyTo
      {
         get { return _replyTo; }
         set { _replyTo = value; }
      }

      /// <summary>
      /// The application message identifier
      /// </summary>
      public string MessageId
      {
         get { return _messageId; }
         set { _messageId = value; }
      }

      /// <summary>
      /// The message timestamp
      /// </summary>
      public ulong Timestamp
      {
         get { return _timestamp; }
         set { _timestamp = value; }
      }

      /// <summary>
      /// The message type name
      /// </summary>
      public string Type
      {
         get { return _type; }
         set { _type = value; }
      }

      /// <summary>
      /// The creating user id
      /// </summary>
      public string UserId
      {
         get { return _userId; }
         set { _userId = value; }
      }

      /// <summary>
      /// The creating application id
      /// </summary>
      public string AppId
      {
         get { return _appId; }
         set { _appId = value; }
      }

      /// <summary>
      /// Intra-cluster routing identifier
      /// </summary>
      public string ClusterId
      {
         get { return _clusterId; }
         set { _clusterId = value; }
      }

      #endregion // Properties


      public BasicContentHeaderProperties()
      {
      }

      public uint PropertyListSize
      {
         get
         {
            return (uint)(EncodingUtils.EncodedShortStringLength(ContentType) +
                   EncodingUtils.EncodedShortStringLength(Encoding) +
                   EncodingUtils.EncodedFieldTableLength(Headers) +
                   1 + 1 +
                   EncodingUtils.EncodedShortStringLength(CorrelationId) +
                   EncodingUtils.EncodedShortStringLength(ReplyTo) +
                   EncodingUtils.EncodedShortStringLength(String.Format("{0:D}", Expiration)) +
                   EncodingUtils.EncodedShortStringLength(MessageId) +
                   8 +
                   EncodingUtils.EncodedShortStringLength(Type) +
                   EncodingUtils.EncodedShortStringLength(UserId) +
                   EncodingUtils.EncodedShortStringLength(AppId) +
                   EncodingUtils.EncodedShortStringLength(ClusterId));

         }
      }

      public ushort PropertyFlags
      {
         get
         {
            int value = 0;

            // for now we just blast in all properties
            for ( int i = 0; i < 14; i++ )
            {
               value += (1 << (15 - i));
            }
            return (ushort)value;
         }
      }

      public void WritePropertyListPayload(ByteBuffer buffer)
      {
         EncodingUtils.WriteShortStringBytes(buffer, ContentType);
         EncodingUtils.WriteShortStringBytes(buffer, Encoding);
         EncodingUtils.WriteFieldTableBytes(buffer, Headers);
         buffer.Put(DeliveryMode);
         buffer.Put(Priority);
         EncodingUtils.WriteShortStringBytes(buffer, CorrelationId);
         EncodingUtils.WriteShortStringBytes(buffer, ReplyTo);
         EncodingUtils.WriteShortStringBytes(buffer, String.Format("{0:D}", Expiration));
         EncodingUtils.WriteShortStringBytes(buffer, MessageId);
         buffer.Put(Timestamp);
         EncodingUtils.WriteShortStringBytes(buffer, Type);
         EncodingUtils.WriteShortStringBytes(buffer, UserId);
         EncodingUtils.WriteShortStringBytes(buffer, AppId);
         EncodingUtils.WriteShortStringBytes(buffer, ClusterId);
      }

      public void PopulatePropertiesFromBuffer(ByteBuffer buffer, ushort propertyFlags)
      {
         _log.Debug("Property flags: " + propertyFlags);
         if ( (propertyFlags & (1 << 15)) > 0 )
            ContentType = EncodingUtils.ReadShortString(buffer);
         if ( (propertyFlags & (1 << 14)) > 0 )
            Encoding = EncodingUtils.ReadShortString(buffer);
         if ( (propertyFlags & (1 << 13)) > 0 )
            Headers = EncodingUtils.ReadFieldTable(buffer);
         if ( (propertyFlags & (1 << 12)) > 0 )
            DeliveryMode = buffer.GetByte();
         if ( (propertyFlags & (1 << 11)) > 0 )
            Priority = buffer.GetByte();
         if ( (propertyFlags & (1 << 10)) > 0 )
            CorrelationId = EncodingUtils.ReadShortString(buffer);
         if ( (propertyFlags & (1 << 9)) > 0 )
            ReplyTo = EncodingUtils.ReadShortString(buffer);
         if ( (propertyFlags & (1 << 8)) > 0 )
            Expiration = EncodingUtils.ReadLongAsShortString(buffer);
         if ( (propertyFlags & (1 << 7)) > 0 )
            MessageId = EncodingUtils.ReadShortString(buffer);
         if ( (propertyFlags & (1 << 6)) > 0 )
            Timestamp = buffer.GetUInt64();
         if ( (propertyFlags & (1 << 5)) > 0 )
            Type = EncodingUtils.ReadShortString(buffer);
         if ( (propertyFlags & (1 << 4)) > 0 )
            UserId = EncodingUtils.ReadShortString(buffer);
         if ( (propertyFlags & (1 << 3)) > 0 )
            AppId = EncodingUtils.ReadShortString(buffer);
         if ( (propertyFlags & (1 << 2)) > 0 )
            ClusterId = EncodingUtils.ReadShortString(buffer);
      }

      public void SetDeliveryMode(DeliveryMode deliveryMode)
      {
         if ( deliveryMode == Messaging.DeliveryMode.NonPersistent )
         {
            DeliveryMode = 1;
         } else
         {
            DeliveryMode = 2;
         }
      }

      public override string ToString()
      {
         return "Properties: " + ContentType + " " + Encoding + " " + Timestamp + " " + Type;
      }
   }
}
