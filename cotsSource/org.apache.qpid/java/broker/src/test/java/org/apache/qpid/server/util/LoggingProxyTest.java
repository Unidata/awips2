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

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import junit.framework.TestCase;

public class LoggingProxyTest extends TestCase
{
    static interface IFoo {
        void foo();
        void foo(int i, Collection c);
        String bar();
        String bar(String s, List l);
    }

    static class Foo implements IFoo {
        public void foo()
        {
        }

        public void foo(int i, Collection c)
        {
        }

        public String bar()
        {
            return null;
        }

        public String bar(String s, List l)
        {
            return "ha";
        }
    }

    public void testSimple() {
        LoggingProxy proxy = new LoggingProxy(new Foo(), 20);
        IFoo foo = (IFoo)proxy.getProxy(IFoo.class);
        foo.foo();
        assertEquals(2, proxy.getBufferSize());
        assertTrue(proxy.getBuffer().get(0).toString().matches(".*: foo\\(\\) entered$"));
        assertTrue(proxy.getBuffer().get(1).toString().matches(".*: foo\\(\\) returned$"));

        foo.foo(3, Arrays.asList(0, 1, 2));
        assertEquals(4, proxy.getBufferSize());
        assertTrue(proxy.getBuffer().get(2).toString().matches(".*: foo\\(\\[3, \\[0, 1, 2\\]\\]\\) entered$"));
        assertTrue(proxy.getBuffer().get(3).toString().matches(".*: foo\\(\\) returned$"));

        foo.bar();
        assertEquals(6, proxy.getBufferSize());
        assertTrue(proxy.getBuffer().get(4).toString().matches(".*: bar\\(\\) entered$"));
        assertTrue(proxy.getBuffer().get(5).toString().matches(".*: bar\\(\\) returned null$"));

        foo.bar("hello", Arrays.asList(1, 2, 3));
        assertEquals(8, proxy.getBufferSize());
        assertTrue(proxy.getBuffer().get(6).toString().matches(".*: bar\\(\\[hello, \\[1, 2, 3\\]\\]\\) entered$"));
        assertTrue(proxy.getBuffer().get(7).toString().matches(".*: bar\\(\\) returned ha$"));

        proxy.dump();
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(LoggingProxyTest.class);
    }
}
