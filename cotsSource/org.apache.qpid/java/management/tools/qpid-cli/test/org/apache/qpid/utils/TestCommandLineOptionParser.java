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
import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class TestCommandLineOptionParser
{
    CommandLineOptionParser parser;
    String[] input;
    CommandLineOption option1;
    CommandLineOption option2;
    ArrayList list1;
    ArrayList list2;

    @Before
    public void setup()
    {
        String temp = "run -h localhost -p 23232";
        input = temp.split(" ");
        parser = new CommandLineOptionParser(input);
        list1 = new ArrayList();
        list2 = new ArrayList();
    }

    @Test
    public void TestParse()
    {
        Map hash = new HashMap();

        list1.add("localhost");
        list2.add("23232");
        option1 = new CommandLineOption("h", list1);
        option2 = new CommandLineOption("p", list2);
        hash.put("h", option1);
        hash.put("p", option2);
        option1 = (CommandLineOption) parser.parse(input).get("h");
        Assert.assertEquals(option1.getOptionType(), "h");
        Assert.assertEquals(option1.getOptionValue(), "localhost");
        option1 = (CommandLineOption) parser.parse(input).get("p");
        Assert.assertEquals(option1.getOptionType(), "p");
        Assert.assertEquals(option1.getOptionValue(), "23232");
        Assert.assertEquals(parser.parse(input).size(), hash.size());
    }

    @After
    public void cleanup()
    {
        parser = null;
        option1 = null;
        option2 = null;

    }
}
