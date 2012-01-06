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

public class ConcurrentTest extends TimedRun
{
    private final TimedRun _test;
    private final Thread[] _threads;

    public ConcurrentTest(TimedRun test, int threads)
    {
        super(test.toString());
        _test = test;
        _threads = new Thread[threads];
    }

    protected void setup() throws Exception
    {
        _test.setup();
        for(int i = 0; i < _threads.length; i++)
        {
            _threads[i] = new Thread(new Runner());
        }
    }

    protected void teardown() throws Exception
    {
        _test.teardown();
    }

    protected void run() throws Exception
    {
        for(Thread t : _threads)
        {
            t.start();
        }
        for(Thread t : _threads)
        {
            t.join();
        }
    }

    private class Runner implements Runnable
    {
        private Exception error;

        public void run()
        {
            try
            {
                _test.run();
            }
            catch(Exception e)
            {
                error = e;
                e.printStackTrace();
            }
        }
    }

}
