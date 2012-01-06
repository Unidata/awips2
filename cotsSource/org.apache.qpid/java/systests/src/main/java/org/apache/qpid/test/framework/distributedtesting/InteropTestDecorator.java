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
package org.apache.qpid.test.framework.distributedtesting;

import junit.framework.Test;
import junit.framework.TestResult;

import org.apache.log4j.Logger;

import org.apache.qpid.test.framework.FrameworkBaseCase;
import org.apache.qpid.test.framework.TestClientDetails;
import org.apache.qpid.test.framework.sequencers.CircuitFactory;
import org.apache.qpid.test.framework.sequencers.InteropCircuitFactory;
import org.apache.qpid.test.utils.ConversationFactory;

import org.apache.qpid.junit.extensions.WrappedSuiteTestDecorator;

import javax.jms.Connection;

import java.util.*;

/**
 * DistributedTestDecorator is a test decorator, written to implement the interop test specification. Given a list
 * of enlisted test clients, that are available to run interop tests, this decorator invites them to participate
 * in each test in the wrapped test suite. Amongst all the clients that respond to the invite, all pairs are formed,
 * and each pairing (in both directions, but excluding the reflexive pairings) is split into a sender and receivers
 * role and a test case run between them. Any enlisted combinations that do not accept a test invite are automatically
 * failed.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Broadcast test invitations and collect enlists. <td> {@link org.apache.qpid.test.utils.ConversationFactory}.
 * <tr><td> Output test failures for clients unwilling to run the test case. <td> {@link Coordinator}
 * <tr><td> Execute distributed test cases. <td> {@link FrameworkBaseCase}
 * <tr><td> Fail non-participating pairings. <td> {@link OptOutTestCase}
 * </table>
 */
public class InteropTestDecorator extends DistributedTestDecorator
{
    /** Used for debugging. */
    private static final Logger log = Logger.getLogger(InteropTestDecorator.class);

    /**
     * Creates a wrapped suite test decorator from another one.
     *
     * @param suite               The test suite.
     * @param availableClients    The list of all clients that responded to the compulsory invite.
     * @param controlConversation The conversation helper for the control level, test coordination conversation.
     * @param controlConnection   The connection that the coordination messages are sent over.
     */
    public InteropTestDecorator(WrappedSuiteTestDecorator suite, Set<TestClientDetails> availableClients,
        ConversationFactory controlConversation, Connection controlConnection)
    {
        super(suite, availableClients, controlConversation, controlConnection);
    }

    /**
     * Broadcasts a test invitation and accetps enlisting from participating clients. The wrapped test case is
     * then repeated for every combination of test clients (provided the wrapped test case extends
     * {@link FrameworkBaseCase}.
     *
     * <p/>Any JMSExceptions during the invite/enlist conversation will be allowed to fall through as runtime exceptions,
     * resulting in the non-completion of the test run.
     *
     * @todo Better error recovery for failure of the invite/enlist conversation could be added.
     *
     * @param testResult The the results object to monitor the test results with.
     */
    public void run(TestResult testResult)
    {
        log.debug("public void run(TestResult testResult): called");

        Collection<Test> tests = testSuite.getAllUnderlyingTests();

        for (Test test : getAllUnderlyingTests())
        {
            FrameworkBaseCase coordTest = (FrameworkBaseCase) test;

            // Broadcast the invitation to find out what clients are available to test.
            Set<TestClientDetails> enlists = signupClients(coordTest);

            // Compare the list of willing clients to the list of all available.
            Set<TestClientDetails> optOuts = new HashSet<TestClientDetails>(allClients);
            optOuts.removeAll(enlists);

            // Output test failures for clients that will not particpate in the test.
            Set<List<TestClientDetails>> failPairs = allPairs(optOuts, allClients);

            for (List<TestClientDetails> failPair : failPairs)
            {
                // Create a distributed test circuit factory for the test.
                CircuitFactory circuitFactory = getTestSequencer();

                // Create an automatic failure test for the opted out test pair.
                FrameworkBaseCase failTest = new OptOutTestCase("testOptOut");
                circuitFactory.setSender(failPair.get(0));
                circuitFactory.setReceiver(failPair.get(1));
                failTest.setCircuitFactory(circuitFactory);

                failTest.run(testResult);
            }

            // Loop over all combinations of clients, willing to run the test.
            Set<List<TestClientDetails>> enlistedPairs = allPairs(enlists, enlists);

            for (List<TestClientDetails> enlistedPair : enlistedPairs)
            {
                // Create a distributed test circuit factory for the test.
                CircuitFactory circuitFactory = getTestSequencer();

                // Set the sending and receiving client details on the test circuitFactory.
                circuitFactory.setSender(enlistedPair.get(0));
                circuitFactory.setReceiver(enlistedPair.get(1));

                // Pass down the connection to hold the coordination conversation over.
                circuitFactory.setConversationFactory(conversationFactory);

                // Execute the test case.
                coordTest.setCircuitFactory(circuitFactory);
                coordTest.run(testResult);
            }
        }
    }

    /**
     * Should provide the distributed test sequencer to pass to {@link org.apache.qpid.test.framework.FrameworkBaseCase}
     * tests.
     *
     * @return A distributed test sequencer.
     */
    public CircuitFactory getTestSequencer()
    {
        return new InteropCircuitFactory();
    }

    /**
     * Produces all pairs of combinations of elements from two sets. The ordering of the elements in the pair is
     * important, that is the pair <l, r> is distinct from <r, l>; both pairs are generated. For any element, i, in
     * both the left and right sets, the reflexive pair <i, i> is not generated.
     *
     * @param left  The left set.
     * @param right The right set.
     * @param <E>   The type of the content of the pairs.
     *
     * @return All pairs formed from the permutations of all elements of the left and right sets.
     */
    private <E> Set<List<E>> allPairs(Set<E> left, Set<E> right)
    {
        log.debug("private <E> Set<List<E>> allPairs(Set<E> left = " + left + ", Set<E> right = " + right + "): called");

        Set<List<E>> results = new HashSet<List<E>>();

        // Form all pairs from left to right.
        // Form all pairs from right to left.
        for (E le : left)
        {
            for (E re : right)
            {
                if (!le.equals(re))
                {
                    results.add(new Pair<E>(le, re));
                    results.add(new Pair<E>(re, le));
                }
            }
        }

        log.debug("results = " + results);

        return results;
    }

    /**
     * A simple implementation of a pair, using a list.
     */
    private class Pair<T> extends ArrayList<T>
    {
        /**
         * Creates a new pair of elements.
         *
         * @param first  The first element.
         * @param second The second element.
         */
        public Pair(T first, T second)
        {
            super();
            super.add(first);
            super.add(second);
        }
    }
}

