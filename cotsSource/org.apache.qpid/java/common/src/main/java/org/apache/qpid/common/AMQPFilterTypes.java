/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 *
 *
 */
package org.apache.qpid.common;

import org.apache.qpid.framing.AMQShortString;

/**
 * Specifies the different filter types for consumers that filter their messages.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Represent different consumer filter types.
 * </table>
 */
public enum AMQPFilterTypes
{
    JMS_SELECTOR("x-filter-jms-selector"),
    NO_CONSUME("x-filter-no-consume"),
    AUTO_CLOSE("x-filter-auto-close");

    /** The identifying string for the filter type. */
    private final AMQShortString _value;

    /**
     * Creates a new filter type from its identifying string.
     *
     * @param value The identifying string.
     */
    AMQPFilterTypes(String value)
    {
        _value = new AMQShortString(value);
    }

    /**
     * Gets the identifying string of the filter type.
     *
     * @return The identifying string of the filter type.
     */
    public AMQShortString getValue()
    {
        return _value;
    }
}
