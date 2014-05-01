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

package org.apache.qpid.framing;

public interface CommonContentHeaderProperties extends ContentHeaderProperties
{
    AMQShortString getContentType();

    void setContentType(AMQShortString contentType);

    FieldTable getHeaders();

    void setHeaders(FieldTable headers);

    byte getDeliveryMode();

    void setDeliveryMode(byte deliveryMode);

    byte getPriority();

    void setPriority(byte priority);

    AMQShortString getCorrelationId();

    void setCorrelationId(AMQShortString correlationId);

    AMQShortString getReplyTo();

    void setReplyTo(AMQShortString replyTo);

    long getExpiration();

    void setExpiration(long expiration);

    AMQShortString getMessageId();

    void setMessageId(AMQShortString messageId);

    long getTimestamp();

    void setTimestamp(long timestamp);

    AMQShortString getType();

    void setType(AMQShortString type);

    AMQShortString getUserId();

    void setUserId(AMQShortString userId);

    AMQShortString getAppId();

    void setAppId(AMQShortString appId);

    AMQShortString getClusterId();

    void setClusterId(AMQShortString clusterId);

    AMQShortString getEncoding();

    void setEncoding(AMQShortString encoding);
}
