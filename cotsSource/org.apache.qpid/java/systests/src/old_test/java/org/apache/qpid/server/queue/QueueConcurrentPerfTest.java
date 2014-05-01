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
package org.apache.qpid.server.queue;

import org.apache.qpid.server.util.AveragedRun;
import org.apache.qpid.server.util.ConcurrentTest;

public class QueueConcurrentPerfTest extends QueuePerfTest
{
    QueueConcurrentPerfTest(Factory factory, int queueCount, int messages)
    {
        super(factory, queueCount, messages);
    }

    public static void main(String[] argv) throws Exception
    {
        Factory[] factories = new Factory[]{SYNCHRONIZED, CONCURRENT};
        int iterations = 5;
        String label = argv.length > 0 ? argv[0]: null;
        System.out.println((label == null ? "" : "Label, ") + "Queue Type, No. of Queues, No. of Operations, Avg Time, Min Time, Max Time");
        //vary number of queues:
        for(Factory f : factories)
        {
            run(label, new AveragedRun(new ConcurrentTest(new QueuePerfTest(f, 100, 10000), iterations), 5));
            run(label, new AveragedRun(new ConcurrentTest(new QueuePerfTest(f, 1000, 10000), iterations), 5));
            run(label, new AveragedRun(new ConcurrentTest(new QueuePerfTest(f, 10000, 10000), iterations), 5));
            run(label, new AveragedRun(new ConcurrentTest(new QueuePerfTest(f, 1000, 1000), iterations), 5));
            run(label, new AveragedRun(new ConcurrentTest(new QueuePerfTest(f, 1000, 100000), iterations), 5));
        }
    }
}
