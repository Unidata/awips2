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

import org.apache.qpid.junit.extensions.util.StackQueue;

import java.util.LinkedList;
import java.util.Queue;

/**
 * SetupTaskHandler implements a task stack. It can be used, by delegation, as a base implementation for tests that want
 * to have configurable setup/teardown task stacks. Typically it is up to the test implementation to decide whether the
 * stack is executed in the setup/teardown methods or in the threadSetup/threadTeaddown methods.
 *
 * <p/><table id="crc"><caption>CRC Card</caption>
 * <tr><th> Responsibilities <th> Collaborations
 * <tr><td> Handle injection of set up tasks.
 * <tr><td> Handle injection of tear down tasks.
 * <tr><td> Run set up tasks in chain order.
 * <tr><td> Run tear down tasks in reverse chain order.
 * </table>
 *
 * @author Rupert Smith
 */
public class SetupTaskHandler implements SetupTaskAware
{
    /** Holds the set up tasks. */
    Queue<Runnable> setups = new LinkedList<Runnable>();

    /** Holds the tear down tasks. */
    Queue<Runnable> teardowns = new StackQueue<Runnable>();

    /**
     * Adds the specified task to the tests setup.
     *
     * @param task The task to add to the tests setup.
     */
    public void chainSetupTask(Runnable task)
    {
        setups.offer(task);
    }

    /**
     * Adds the specified task to the tests tear down.
     *
     * @param task The task to add to the tests tear down.
     */
    public void chainTearDownTask(Runnable task)
    {
        teardowns.offer(task);
    }

    /**
     * Runs the set up tasks in the order that they way chained.
     */
    public void runSetupTasks()
    {
        while (!setups.isEmpty())
        {
            setups.remove().run();
        }
    }

    /**
     * Runs the tear down tasks in the reverse of the order in which they were chained.
     */
    public void runTearDownTasks()
    {
        while (!teardowns.isEmpty())
        {
            teardowns.remove().run();
        }
    }
}
