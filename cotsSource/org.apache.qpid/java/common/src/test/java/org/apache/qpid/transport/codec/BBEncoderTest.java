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
package org.apache.qpid.transport.codec;

import junit.framework.TestCase;

import java.nio.ByteBuffer;

/**
 * BBEncoderTest
 *
 */

public class BBEncoderTest extends TestCase
{

    public void testGrow()
    {
        BBEncoder enc = new BBEncoder(4);
        enc.writeInt32(0xDEADBEEF);
        ByteBuffer buf = enc.buffer();
        assertEquals(0xDEADBEEF, buf.getInt(0));
        enc.writeInt32(0xBEEFDEAD);
        buf = enc.buffer();
        assertEquals(0xDEADBEEF, buf.getInt(0));
        assertEquals(0xBEEFDEAD, buf.getInt(4));
    }

}
