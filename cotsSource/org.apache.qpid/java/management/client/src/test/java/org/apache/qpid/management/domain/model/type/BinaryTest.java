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
package org.apache.qpid.management.domain.model.type;

import junit.framework.TestCase;

/**
 * Test case for "Binary" type.
 * 
 * @author Andrea Gazzarini
 */
public class BinaryTest extends TestCase
{
    /**
     * Tests the lazy & once hash code computation behaviour of the binary type.
     */
    public void testHashCodeComputation(){
        Binary binary = new Binary(new byte[]{1,2,3,4,5,6,7,6,3,3});
        assertSame(binary.state,binary.hashCodeNotYetComputed);
        
        int firstResult = binary.hashCode();
        assertSame(binary.state,binary.hashCodeAlreadyComputed);

        int secondResult = binary.hashCode();
        assertSame(binary.state,binary.hashCodeAlreadyComputed);
        assertEquals(firstResult,secondResult);
    }
    
    /**
     * Tests the equals() method of the binary class.
     * Two binary must be equals only if they contain the same array (that is, two arrays with the same size & content)
     */
    public void testIdentity() {
        Binary binary = new Binary(new byte[]{1,2,3,4,5,6,7,6,3,3});
        Binary theSame= new Binary(new byte[]{1,2,3,4,5,6,7,6,3,3});
        Binary aDifferentOne = new Binary(new byte[]{4,2,3,3,4,4,5,3,3,2});
        
        assertTrue(binary.equals(theSame));
        assertFalse(binary.equals(aDifferentOne));
    }
}