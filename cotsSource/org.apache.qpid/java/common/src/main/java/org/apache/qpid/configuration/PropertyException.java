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
package org.apache.qpid.configuration;

import org.apache.qpid.AMQException;
import org.apache.qpid.protocol.AMQConstant;

/**
 * Indicates a failure to parse a property expansion. See {@link PropertyUtils} for the code that does property
 * expansions.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaboration
 * <tr><td> Represent failure to expand a property name into a value.
 * </table>
 *
 * @todo Not an AMQP exception as no status code.
 */
public class PropertyException extends AMQException
{
    public PropertyException(String message, Throwable cause)
    {
        super(null, message, cause);
    }
}
