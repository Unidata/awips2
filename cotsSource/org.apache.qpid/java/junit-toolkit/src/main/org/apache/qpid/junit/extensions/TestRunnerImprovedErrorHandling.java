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

import junit.framework.Test;
import junit.framework.TestResult;

import junit.runner.Version;

import junit.textui.ResultPrinter;
import junit.textui.TestRunner;

import org.apache.log4j.Logger;

import java.io.PrintStream;

/**
 * The {@link junit.textui.TestRunner} does not provide very good error handling. It does not wrap exceptions and
 * does not print out stack traces, losing valuable error tracing information. This class overrides methods in it
 * in order to improve their error handling. The {@link TKTestRunner} is then built on top of this.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * </table>
 *
 * @author Rupert Smith
 */
public class TestRunnerImprovedErrorHandling extends TestRunner
{
    /** Used for logging. */
    Logger log = Logger.getLogger(TestRunnerImprovedErrorHandling.class);

    /**
     * Delegates to the super constructor.
     */
    public TestRunnerImprovedErrorHandling()
    {
        super();
    }

    /**
     * Delegates to the super constructor.
     *
     * @param printStream The location to write test results to.
     */
    public TestRunnerImprovedErrorHandling(PrintStream printStream)
    {
        super(printStream);
    }

    /**
     * Delegates to the super constructor.
     *
     * @param resultPrinter The location to write test results to.
     */
    public TestRunnerImprovedErrorHandling(ResultPrinter resultPrinter)
    {
        super(resultPrinter);
    }

    /**
     * Starts a test run. Analyzes the command line arguments
     * and runs the given test suite.
     *
     * @param args The command line arguments.
     *
     * @return The test results.
     *
     * @throws Exception Any exceptions falling through the tests are wrapped in Exception and rethrown.
     */
    public TestResult start(String[] args) throws Exception
    {
        String testCase = "";
        boolean wait = false;

        for (int i = 0; i < args.length; i++)
        {
            if (args[i].equals("-wait"))
            {
                wait = true;
            }
            else if (args[i].equals("-c"))
            {
                testCase = extractClassName(args[++i]);
            }
            else if (args[i].equals("-v"))
            {
                System.err.println("JUnit " + Version.id() + " by Kent Beck and Erich Gamma");
            }
            else
            {
                testCase = args[i];
            }
        }

        if (testCase.equals(""))
        {
            throw new Exception("Usage: TestRunner [-wait] testCaseName, where name is the name of the TestCase class");
        }

        try
        {
            Test suite = getTest(testCase);

            return doRun(suite, wait);
        }
        catch (Exception e)
        {
            log.warn("Got exception whilst creating and running test suite.", e);
            throw new Exception("Could not create and run the test suite.", e);
        }
    }
}
