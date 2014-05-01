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
package org.apache.qpid.test.unit.client.forwardall;

import org.apache.qpid.test.utils.QpidTestCase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Runs the Service's and Client parts of the test in the same process
 * as the broker
 */
public class CombinedTest extends QpidTestCase
{
    private static final Logger _logger = LoggerFactory.getLogger(CombinedTest.class);
    private int run = 0;

    protected void setUp() throws Exception
    {
        super.setUp();
        Service.setQTC(this);
        Client.setQTC(this);
    }

    protected void tearDown() throws Exception
    {
        ServiceCreator.closeAll();
        super.tearDown();
    }

    public void testForwardAll() throws Exception
    {
        while (run < 10)
        {
            int services =1;
            ServiceCreator.start("vm://:1", services);

            _logger.info("Starting " + ++run + " client...");

            new Client("vm://:1", services).shutdownWhenComplete();


            _logger.info("Completed " + run + " successfully!");
        }
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(CombinedTest.class);
    }
}
