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

/**
 * This interface can be implemented by tests that want to know if they are being run concurrently. It provides
 * lifecycle notification events to tell the test implementation when test threads are being created and destroyed.
 * This can assist tests in creating and destroying resources that exist over the life of a test thread. A single
 * test thread can excute the same test many times, and often it is convenient to keep resources, for example network
 * connections, open over many test calls.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities
 * <tr><td> Set up per thread test fixtures.
 * <tr><td> Clean up per thread test fixtures.
 * </table>
 *
 * @author Rupert Smith
 */
public interface TestThreadAware
{
    /**
     * Called when a test thread is created.
     */
    public void threadSetUp();

    /**
     * Called when a test thread is destroyed.
     */
    public void threadTearDown();
}
