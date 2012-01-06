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
package org.apache.qpid.utils;

import java.util.ArrayList;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class TestJMXConfiguration
{
    CommandLineOptionParser clop;
    JMXConfiguration jmc;
    CommandLineOption option;
    String[] input;

    @Before
    public void setup()
    {
        String temp = "command -h 127.0.0.1 -p 1234";
        input = temp.split(" ");
        clop = new CommandLineOptionParser(input);
        jmc = new JMXConfiguration(clop.getAlloptions());
    }

    @Test
    public void TestLoadOption()
    {
        ArrayList list = new ArrayList();
        list.add("127.0.0.1");
        option = new CommandLineOption("-h", list);
        CommandLineOption expect = jmc.loadoption("h", clop.getAlloptions());
        Assert.assertEquals(expect.getOptionType(), option.getOptionType());
        Assert.assertEquals(expect.getOptionValue(), option.getOptionValue());
    }

    @After
    public void cleanup()
    {
        clop = null;
        jmc = null;
        option = null;
    }
}
