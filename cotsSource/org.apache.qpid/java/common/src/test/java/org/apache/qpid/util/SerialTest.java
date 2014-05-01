package org.apache.qpid.util;
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


import junit.framework.TestCase;

import java.util.Random;

import org.apache.qpid.SerialException;

/**
 *Junit tests for the Serial class 
 */
public class SerialTest extends TestCase
{

    /**
     * Test the key boundaries where wraparound occurs.
     */
    public void testBoundaries()
    {
        assertTrue(Serial.gt(1, 0));
        assertTrue(Serial.lt(0, 1));

        assertTrue(Serial.gt(Integer.MAX_VALUE+1, Integer.MAX_VALUE));
        assertTrue(Serial.lt(Integer.MAX_VALUE, Integer.MAX_VALUE+1));

        assertTrue(Serial.gt(0xFFFFFFFF + 1, 0xFFFFFFFF));
        assertTrue(Serial.lt(0xFFFFFFFF, 0xFFFFFFFF + 1));
    }

    /**
     * Test the first Corollary of RFC 1982
     * For any sequence number s and any integer n such that addition of n
     * to s is well defined, (s + n) >= s.  Further (s + n) == s only when
     * n == 0, in all other defined cases, (s + n) > s.
     */
    public void testCorollary1()
    {
        int wrapcount = 0;

        int s = 0;

        for (int i = 0; i < 67108664; i++)
        {
            for (int n = 1; n < 4096; n += 512)
            {
                assertTrue(Serial.gt(s+n, s));
                assertTrue(Serial.lt(s, s+n));
            }

            s += 1024;

            if (s == 0)
            {
                wrapcount += 1;
            }
        }

        assertTrue(wrapcount > 0);
    }

}
