/*
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
*/

namespace Apache.Qpid.AmqpTypes
{
    using System;
    using System.IO;
    using System.Collections.Generic;
    using System.Text;

    public sealed class PropertyName
    {
        public const string Priority = "amqpx.priority";
        public const string ContentType = "amqp.content-type";
        public const string ReplyTo = "amqp.reply-to";
        public const string ReplyToExchange = "amqpx.qpid0-10.reply-to-exchange";
        public const string RoutingKey = "amqpx.qpid0-10.routing-key";
    }
}
