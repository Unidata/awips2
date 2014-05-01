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
package org.apache.qpid.util;

import java.nio.ByteBuffer;
import java.util.UUID;


/**
 * NameUUIDGen
 *
 */

public final class NameUUIDGen implements UUIDGen
{

    private static final int WIDTH = 8;

    final private byte[] seed;
    final private ByteBuffer seedBuf;
    private long counter;

    public NameUUIDGen()
    {
        String namespace = UUID.randomUUID().toString();
        this.seed = new byte[namespace.length() + WIDTH];
        for (int i = WIDTH; i < seed.length; i++)
        {
            seed[i] = (byte) namespace.charAt(i - WIDTH);
        }
        this.seedBuf = ByteBuffer.wrap(seed);
        this.counter = 0;
    }

    public UUID generate()
    {
        seedBuf.putLong(0, counter++);
        return UUID.nameUUIDFromBytes(seed);
    }

}
