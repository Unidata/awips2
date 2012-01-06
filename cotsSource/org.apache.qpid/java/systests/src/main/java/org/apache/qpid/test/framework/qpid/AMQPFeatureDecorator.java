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
package org.apache.qpid.test.framework.qpid;

import junit.framework.Test;
import junit.framework.TestResult;

import org.apache.qpid.test.framework.FrameworkBaseCase;
import org.apache.qpid.test.framework.LocalAMQPCircuitFactory;

import org.apache.qpid.junit.extensions.WrappedSuiteTestDecorator;

/**
 * AMQPFeatureDecorator applies decorations to {@link FrameworkBaseCase} tests, so that they may use Qpid/AMQP specific
 * features, not available through JMS. For example, the immediate and mandatory flags. This decorator replaces the
 * standard test circuit factory on the base class with one that allows these features to be used.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Substitute the circuit factory with an AMQP/Qpid specific one.
 * </table>
 *
 * @todo This wrapper substitutes in a LocalAMQPCircuitFactory, which is fine for local tests. For distributed tests
 *       the Fanout or Interop factories are substituted in by their decorators instead. These actually use
 *       distributed circuit static create methods to build the circuits, which should actually be changed to a factory,
 *       so that static methods do not need to be used. The distributed circuit creater delegates the circuit
 *       construction to remote test nodes. This decorator should not be used with distributed tests, or should be made
 *       aware of them, in which case it might ensure that an AMQP feature (implied already by other properties) flag
 *       is passed out to the remote test nodes, and provide a mechansim for them to decorate their circuit creation
 *       with AMQP features too. Add factory substituion/decoration mechansim for test clients, here or in a seperate
 *       class.
 */
public class AMQPFeatureDecorator extends WrappedSuiteTestDecorator
{
    /** The test suite to run. */
    private Test test;

    /**
     * Creates a wrapped test test decorator from another one.
     *
     * @param test The test test.
     */
    public AMQPFeatureDecorator(WrappedSuiteTestDecorator test)
    {
        super(test);
        this.test = test;
    }

    /**
     * Runs the tests with a LocalAMQPCircuitFactory. Only tests that extend FrameworkBaseCase are decorated.
     *
     * @param testResult The the results object to monitor the test results with.
     */
    public void run(TestResult testResult)
    {
        for (Test test : getAllUnderlyingTests())
        {
            if (test instanceof FrameworkBaseCase)
            {
                FrameworkBaseCase frameworkTest = (FrameworkBaseCase) test;
                frameworkTest.setCircuitFactory(new LocalAMQPCircuitFactory());
            }
        }

        // Run the test.
        test.run(testResult);
    }

    /**
     * Prints the name of the test for debugging purposes.
     *
     * @return The name of the test.
     */
    public String toString()
    {
        return "AMQPFeatureDecorator: [test = \"" + test + "\"]";
    }
}
