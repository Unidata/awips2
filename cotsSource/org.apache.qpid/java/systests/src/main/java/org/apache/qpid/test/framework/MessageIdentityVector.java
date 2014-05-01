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

/**
 * MessageIdentityVector provides a message identification scheme, that matches individual messages with test cases.
 * Test messages are being sent by a number of test clients, sending messages over a set of routes, and being received
 * by another set of test clients. Each test is itself, being run within a test cycle, of which there could be many. It
 * is the job of the test coordinator to request and receive reports from the available test clients, on what has been
 * sent, what has been received, and what errors may have occurred, and to reconcile this information against the
 * assertions being applied by the test case. In order to be able to figure out which messages belong to which test,
 * there needs to be an identification scheme, that the coordinator can use to correlate messages in senders and
 * receiver reports. Every message sent in a test can be associated with this information.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Identify a test case, a handling client id, a circuit end within the client, and a test cycle number.
 * </table>
 */
public class MessageIdentityVector
{
    /** Holds the test case vector component of the message identity vector. */
    private TestCaseVector testCaseVector;

    /** The unique client id. */
    private String clientId;

    /** The unique circuit end number within the client id. */
    private int circuitEndId;

    /**
     * Creates a new identity vector for test messages.
     *
     * @param testCase        The name of the test case generating the messages.
     * @param clientId        The unique id of the client implementing a circuit end that is handling the messages.
     * @param circuitEndId    The unique id number of the circuit end within the client.
     * @param testCycleNumber The cycle iteration number of the test case.
     */
    public MessageIdentityVector(String testCase, String clientId, int circuitEndId, int testCycleNumber)
    {
        this.testCaseVector = new TestCaseVector(testCase, testCycleNumber);
        this.clientId = clientId;
        this.circuitEndId = circuitEndId;
    }

    /**
     * Reports the test case vector component of the message identity vector.
     *
     * @return The test case vector component of the message identity vector.
     */
    public TestCaseVector getTestCaseVector()
    {
        return testCaseVector;
    }

    /**
     * Reports the name of the test case.
     *
     * @return The name of the test case.
     */
    public String getTestCase()
    {
        return testCaseVector.getTestCase();
    }

    /**
     * Reports the test iteration cycle number within the test case.
     *
     * @return The test iteration cycle number within the test case.
     */
    public int getTestCycleNumber()
    {
        return testCaseVector.getTestCycleNumber();
    }

    /**
     * Resports the client id.
     *
     * @return The client id.
     */
    public String getClientId()
    {
        return clientId;
    }

    /**
     * Reports the circuit end number within the test client.
     *
     * @return The circuit end number within the test client.
     */
    public int getCircuitEndId()
    {
        return circuitEndId;
    }

    /**
     * Compares this identity vector with another for equality. All fields must match.
     *
     * @param o The identity vector to compare with.
     *
     * @return <tt>true</tt> if the identity vector is identical to this one by all fields, <tt>false</tt> otherwise.
     */
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }

        if ((o == null) || (getClass() != o.getClass()))
        {
            return false;
        }

        MessageIdentityVector that = (MessageIdentityVector) o;

        if (circuitEndId != that.circuitEndId)
        {
            return false;
        }

        if ((clientId != null) ? (!clientId.equals(that.clientId)) : (that.clientId != null))
        {
            return false;
        }

        if ((testCaseVector != null) ? (!testCaseVector.equals(that.testCaseVector)) : (that.testCaseVector != null))
        {
            return false;
        }

        return true;
    }

    /**
    * Computes a hash code for this identity vector based on all fields.
    *
    * @return A hash code for this identity vector based on all fields.
    */
    public int hashCode()
    {
        int result;
        result = ((testCaseVector != null) ? testCaseVector.hashCode() : 0);
        result = (31 * result) + ((clientId != null) ? clientId.hashCode() : 0);
        result = (31 * result) + circuitEndId;

        return result;
    }
}
