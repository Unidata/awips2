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
package org.apache.qpid.server.util;

import org.apache.qpid.server.util.TimedRun;

import java.util.concurrent.Callable;
import java.util.Collection;

public class AveragedRun implements Callable<RunStats>
{
    private final RunStats stats = new RunStats();
    private final TimedRun test;
    private final int iterations;

    public AveragedRun(TimedRun test, int iterations)
    {
        this.test = test;
        this.iterations = iterations;
    }

    public RunStats call() throws Exception
    {
        for (int i = 0; i < iterations; i++)
        {
            stats.record(test.call());
        }
        return stats;
    }

    public void run() throws Exception
    {
        System.out.println(test + ": " + call());
    }

    public String toString()
    {
        return test.toString();
    }

    static void run(Collection<AveragedRun> tests) throws Exception
    {
        for(AveragedRun test : tests)
        {
            test.run();
        }
    }
}
