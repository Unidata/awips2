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
 * SetupTaskAware is an interface that tests that can accept injectable setup tasks may implement. Typically this
 * is used by configurable decorator stack to inject setup tasks into tests. It is then up to the test case to run
 * the tasks in the setup or threadSetup methods as it chooses.
 *
 * <p/>Set up tasks should be chained so that they are executed in the order that they are applied. Tear down tasks
 * should be chained so that they are executed in the reverse order to which they are applied. That way the set up and
 * tear down tasks act as a 'task' stack, with nested setups and tear downs.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities.
 * <tr><td> Handle injection of set up tasks.
 * <tr><td> Handle injection of tear down tasks.
 * </table>
 *
 * @author Rupert Smith
 */
public interface SetupTaskAware
{
    /**
     * Adds the specified task to the tests setup.
     *
     * @param task The task to add to the tests setup.
     */
    public void chainSetupTask(Runnable task);

    /**
     * Adds the specified task to the tests tear down.
     *
     * @param task The task to add to the tests tear down.
     */
    public void chainTearDownTask(Runnable task);
}
