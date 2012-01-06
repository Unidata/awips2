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
 * TestClientDetails is used to encapsulate information about an interop test client. It pairs together the unique
 * name of the client, and the route on which it listens to its control messages.
 *
 * <p><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Record test clients control addresses together with their names.
 * </table>
 */
public class TestClientDetails
{
    /** The test clients name. */
    public String clientName;

    /* The test clients unique sequence number. Not currently used. */

    /** The routing key of the test clients control topic. */
    public String privateControlKey;

    /**
     * Two TestClientDetails are considered to be equal, iff they have the same client name.
     *
     * @param o The object to compare to.
     *
     * @return <tt>If the object to compare to is a TestClientDetails equal to this one, <tt>false</tt> otherwise.
     */
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }

        if (!(o instanceof TestClientDetails))
        {
            return false;
        }

        final TestClientDetails testClientDetails = (TestClientDetails) o;

        return !((clientName != null) ? (!clientName.equals(testClientDetails.clientName))
                                      : (testClientDetails.clientName != null));
    }

    /**
     * Computes a hash code compatible with the equals method; based on the client name alone.
     *
     * @return A hash code for this.
     */
    public int hashCode()
    {
        return ((clientName != null) ? clientName.hashCode() : 0);
    }

    /**
     * Outputs the client name and address details. Mostly used for debugging purposes.
     *
     * @return The client name and address.
     */
    public String toString()
    {
        return "TestClientDetails: [ clientName = " + clientName + ", privateControlKey = " + privateControlKey + " ]";
    }
}
