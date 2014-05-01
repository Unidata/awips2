package org.apache.qpid.util.concurrent;
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


/**
 * SynchException is used to encapsulate exceptions with the data elements that caused them in order to send exceptions
 * back from the consumers of a {@link BatchSynchQueue} to producers. The underlying exception should be retrieved from
 * the {@link #getCause} method.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Encapsulate a data element and exception.
 * </table>
 */
public class SynchException extends Exception
{
    /** Holds the data element that is in error. */
    Object element;

    /**
     * Creates a new BaseApplicationException object.
     *
     * @param message The exception message.
     * @param cause   The underlying throwable cause. This may be null.
     */
    public SynchException(String message, Throwable cause, Object element)
    {
        super(message, cause);

        // Keep the data element that was in error.
        this.element = element;
    }
}
