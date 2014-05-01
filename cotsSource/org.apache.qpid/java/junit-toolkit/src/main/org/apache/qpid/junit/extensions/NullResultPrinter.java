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
package org.apache.qpid.junit.extensions;

import junit.framework.AssertionFailedError;
import junit.framework.Test;

import junit.textui.ResultPrinter;

import java.io.PrintStream;

/**
 * A ResultPrinter that prints nothing. This exists, in order to provide a replacement to JUnit's ResultPrinter, which
 * is refered to directly by JUnit code, rather that as an abstracted TestListener. JUnit's text ui TestRunner must
 * have a ResultPrinter. This provides an implementation of it that prints nothing, so that a better mechanism can
 * be used for providing feedback to the console instead.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td>
 * </table>
 *
 * @todo See todo in TKTestRunner about completely replacing the test ui runner. Doing things like this in order to
 *       extend JUnit is not nice, and there needs to be a better way to do it. Delete this class and use a listener
 *       instead.
 *
 * @author Rupert Smith
 */
public class NullResultPrinter extends ResultPrinter
{
    /**
     * Builds a fake ResultPrinter that prints nothing.
     *
     * @param writer The writer to send output to.
     */
    public NullResultPrinter(PrintStream writer)
    {
        super(writer);
    }

    /**
     * Does nothing.
     *
     * @param test Ignored.
     * @param t    Ignored.
     */
    public void addError(Test test, Throwable t)
    { }

    /**
     * Does nothing.
     *
     * @param test Ignored.
     * @param t    Ignored.
     */
    public void addFailure(Test test, AssertionFailedError t)
    { }

    /**
     * Does nothing.
     *
     * @param test Ignored.
     */
    public void endTest(Test test)
    { }

    /**
     * Does nothing.
     *
     * @param test Ignored.
     */
    public void startTest(Test test)
    { }
}
