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
package org.apache.qpid.management.ui;

import junit.framework.TestCase;

public class ApiVersionTest extends TestCase
{

    public void testGetMajor()
    {
        ApiVersion ver = new ApiVersion(1,3);
        assertEquals(1, ver.getMajor());
    }

    public void testGetMinor()
    {
        ApiVersion ver = new ApiVersion(1,3);
        assertEquals(3, ver.getMinor());
    }

    public void testGreaterThanOrEqualTo()
    {
        ApiVersion ver = new ApiVersion(1,3);
        
        //equal
        assertTrue(ver.greaterThanOrEqualTo(1, 3));
        //same major, higher minor
        assertFalse(ver.greaterThanOrEqualTo(1, 4));
        //same major, lower minor
        assertTrue(ver.greaterThanOrEqualTo(1, 2));
        
        //higher major, lower minor
        assertFalse(ver.greaterThanOrEqualTo(2, 0));
        //higher major, same minor
        assertFalse(ver.greaterThanOrEqualTo(2, 3));
        //higher major, higher minor
        assertFalse(ver.greaterThanOrEqualTo(2, 4));
        
        //lower major, higher minor
        assertTrue(ver.greaterThanOrEqualTo(0, 9));
        //lower major, lower minor
        assertTrue(ver.greaterThanOrEqualTo(0, 2));
        //lower major, same minor
        assertTrue(ver.greaterThanOrEqualTo(0, 3));
    }

    public void testLessThanOrEqualTo()
    {
        ApiVersion ver = new ApiVersion(1,3);
        
        //equal
        assertTrue(ver.lessThanOrEqualTo(1, 3));
        //same major, higher minor
        assertTrue(ver.lessThanOrEqualTo(1, 4));
        //same major, lower minor
        assertFalse(ver.lessThanOrEqualTo(1, 2));
        
        //higher major, lower minor
        assertTrue(ver.lessThanOrEqualTo(2, 0));
        //higher major, same minor
        assertTrue(ver.lessThanOrEqualTo(2, 3));
        //higher major, higher minor
        assertTrue(ver.lessThanOrEqualTo(2, 4));
        
        //lower major, higher minor
        assertFalse(ver.lessThanOrEqualTo(0, 9));
        //lower major, lower minor
        assertFalse(ver.lessThanOrEqualTo(0, 2));
        //lower major, same minor
        assertFalse(ver.lessThanOrEqualTo(0, 3));
    }

    public void testGreaterThan()
    {
        ApiVersion ver = new ApiVersion(1,3);
        
        //equal
        assertFalse(ver.greaterThan(1, 3));
        //same major, higher minor
        assertFalse(ver.greaterThan(1, 4));
        //same major, lower minor
        assertTrue(ver.greaterThan(1, 2));
        
        //higher major, lower minor
        assertFalse(ver.greaterThan(2, 0));
        //higher major, same minor
        assertFalse(ver.greaterThan(2, 3));
        //higher major, higher minor
        assertFalse(ver.greaterThan(2, 4));
        
        //lower major, higher minor
        assertTrue(ver.greaterThan(0, 9));
        //lower major, lower minor
        assertTrue(ver.greaterThan(0, 2));
        //lower major, same minor
        assertTrue(ver.greaterThan(0, 3));
    }

    public void testLessThan()
    {
        ApiVersion ver = new ApiVersion(1,3);
        
        //equal
        assertFalse(ver.lessThan(1, 3));
        //same major, higher minor
        assertTrue(ver.lessThan(1, 4));
        //same major, lower minor
        assertFalse(ver.lessThan(1, 2));
        
        //higher major, lower minor
        assertTrue(ver.lessThan(2, 0));
        //higher major, same minor
        assertTrue(ver.lessThan(2, 3));
        //higher major, higher minor
        assertTrue(ver.lessThan(2, 4));
        
        //lower major, higher minor
        assertFalse(ver.lessThan(0, 9));
        //lower major, lower minor
        assertFalse(ver.lessThan(0, 2));
        //lower major, same minor
        assertFalse(ver.lessThan(0, 3));
    }

    public void testEqualsIntInt()
    {
        ApiVersion ver = new ApiVersion(1,3);
        
        //equal
        assertTrue(ver.equals(1, 3));
        //same major, higher minor
        assertFalse(ver.equals(1, 4));
        //same major, lower minor
        assertFalse(ver.equals(1, 2));
        
        //higher major, lower minor
        assertFalse(ver.equals(2, 0));
        //higher major, same minor
        assertFalse(ver.equals(2, 3));
        //higher major, higher minor
        assertFalse(ver.equals(2, 4));
        
        //lower major, higher minor
        assertFalse(ver.equals(0, 9));
        //lower major, lower minor
        assertFalse(ver.equals(0, 2));
        //lower major, same minor
        assertFalse(ver.equals(0, 3));
    }

    public void testToString()
    {
        ApiVersion ver = new ApiVersion(1,3);

        assertEquals("major=1,minor=3", ver.toString());
    }

}
