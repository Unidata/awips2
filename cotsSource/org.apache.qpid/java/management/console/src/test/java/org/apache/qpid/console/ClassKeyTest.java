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
package org.apache.qpid.console;

import junit.framework.TestCase;

public class ClassKeyTest extends TestCase
{
    public void testCreation()
    {
        ClassKey key = new ClassKey(
                "some.package:Class(00000001-00000002-00000003-00000004)");
        assertEquals("some.package", key.getPackageName());
        assertEquals("Class", key.getClassName());
        assertEquals("00000001-00000002-00000003-00000004", key.getHashString());
        assertEquals(1, key.getHash()[0]);
        assertEquals(2, key.getHash()[1]);
        assertEquals(3, key.getHash()[2]);
        assertEquals(4, key.getHash()[3]);
    }
}
