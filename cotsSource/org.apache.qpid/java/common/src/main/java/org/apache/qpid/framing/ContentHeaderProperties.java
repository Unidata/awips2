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

import org.apache.mina.common.ByteBuffer;

/**
 * There will be an implementation of this interface for each content type. All content types have associated
 * header properties and this provides a way to encode and decode them.
 */
public interface ContentHeaderProperties
{
    /**
     * Writes the property list to the buffer, in a suitably encoded form.
     * @param buffer The buffer to write to
     */
    void writePropertyListPayload(ByteBuffer buffer);

    /**
     * Populates the properties from buffer.
     * @param buffer The buffer to read from.
     * @param propertyFlags he property flags.
     * @throws AMQFrameDecodingException when the buffer does not contain valid data
     */
    void populatePropertiesFromBuffer(ByteBuffer buffer, int propertyFlags, int size)
        throws AMQFrameDecodingException;

    /**
     * @return the size of the encoded property list in bytes.
     */
    int getPropertyListSize();

    /**
     * Gets the property flags. Property flags indicate which properties are set in the list. The
     * position and meaning of each flag is defined in the protocol specification for the particular
     * content type with which these properties are associated.
     * @return flags
     */
    int getPropertyFlags();

    void updated();
}
