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
package org.apache.qpid.test.framework;

import java.util.LinkedList;
import java.util.List;

/**
 * AssertionBase is a base class for implenmenting assertions. It provides a mechanism to store error messages, and
 * report all error messages when its {@link #toString()} method is called.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Collect error messages.
 * </table>
 */
public abstract class AssertionBase implements Assertion
{
    /** Holds the error messages. */
    List<String> errors = new LinkedList<String>();

    /**
     * Adds an error message to the assertion.
     *
     * @param error An error message to add to the assertion.
     */
    public void addError(String error)
    {
        errors.add(error);
    }

    /**
     * Prints all of the error messages in the assertion into a string.
     *
     * @return All of the error messages in the assertion as a string.
     */
    public String toString()
    {
        String result = "";

        for (String error : errors)
        {
            result += error;
        }

        return result;
    }
}
