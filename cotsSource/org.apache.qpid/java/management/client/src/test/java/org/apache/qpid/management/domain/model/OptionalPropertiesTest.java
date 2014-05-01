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
package org.apache.qpid.management.domain.model;

import java.nio.ByteBuffer;
import java.util.LinkedList;
import java.util.List;

import junit.framework.TestCase;

import org.apache.qpid.management.domain.model.type.Uint64;
import org.apache.qpid.transport.codec.BBDecoder;

public class OptionalPropertiesTest extends TestCase
{
    public void testDecoderStateChange()
    {
        QpidProperty property = new QpidProperty();
        assertSame(
                "Default decoder for properties is the one for mandatory properties.",
                property._mandatoryPropertyDecoder,
                property._decoder);
        
        property.markAsOptional(1);
        assertSame(
                "After a property has been marked as optional the corresponding decoder must be installed.",
                property._optionalPropertyDecoder,
                property._decoder);
    }
    
    /**
     * Tests the execution of the decode() method when the current property is optional but in the presence bitmask 
     * there's no the corresponding bit set.
     * 
     * <br>precondition : property is optional and corresponding presence bit is not set.
     * <br>postcondition : result must be null.
     */
    public void testDecodeValueWithOptionalPropertyAndMissingValue() 
    {
        byte [] presenceBytes = {2};
        
        QpidProperty property = new QpidProperty();
        
        // We don't need a decoder so in order to be sure that it won't be invoked set it to null.
        BBDecoder nullDecoder = null;
        
        for (int i = 0; i < 8; i++)
        {
            // Property number 1 is declaring a value so skip it!
            if (i != 1)
            {
                property.markAsOptional(i);
                assertNull(property.decodeValue(nullDecoder, presenceBytes));
            }
        }
    }
    
    /**
     * Tests the execution of the decode() method when the current property is optional but in the presence bitmask 
     * there's no the corresponding bit set.
     * 
     * <br>precondition : property is optional and corresponding presence bit is not set.
     * <br>postcondition : result must be null.
     */
    public void testDecodeValueWithOptionalPropertyAndDeclaredValue() 
    {
        byte [] presenceBytes = {4};
        Long _44 = new Long(44);
        
        QpidProperty property = new QpidProperty();
        property.setType(new Uint64());
        property.markAsOptional(2);
        
        ByteBuffer buffer = ByteBuffer.allocate(8);
        buffer.putLong(_44);
        buffer.rewind();
        BBDecoder decoder = new BBDecoder();
        
        decoder.init(buffer);
        assertEquals(_44,property.decodeValue(decoder, presenceBytes));
    }
    
    /**
     * Tests the execution of the decode() method with a real scenario where there are mandatory and optional
     * properties. 
     */
    public void testDecodeValueWithOptionalAndMandatoryProperties() 
    {
        // With this bitset :
        // 
        // 1th opt property is null;
        // 2th opt property is null;
        // 3th opt property is not null;
        // 4th opt property is null;
        // 5th opt propertyis null;
        // 6th opt property is null;
        // 7th opt property is null;
        // 8th opt property is not null;
        byte [] presenceBytes = {4,1};

        List<QpidProperty> properties = new LinkedList<QpidProperty>();
        
        properties.add(createProperty(false, -1));
        properties.add(createProperty(false, -1));
        properties.add(createProperty(true, 0));
        properties.add(createProperty(false, -1));
        properties.add(createProperty(false, -1));
        properties.add(createProperty(true, 1));
        properties.add(createProperty(false, -1));
        properties.add(createProperty(false, -1));
        properties.add(createProperty(true, 2));
        properties.add(createProperty(true, 3));
        properties.add(createProperty(true, 4));
        properties.add(createProperty(true, 5));
        properties.add(createProperty(true, 6));
        properties.add(createProperty(true, 7));
        properties.add(createProperty(false, -1));
        properties.add(createProperty(true, 8));

        Long expectedResults [] = {
                1L,           // p1
                22L,         // p2
                null,       // p3
                232L,      // p4
                211L,     // p5
                null,       // p6
                232L,      // p7
                211L,     // p8
                999L,       // p9
                null,       // p10
                null,       // p11
                null,       // p12
                null,       // p13
                null,       // p14
                626L,       // p15
                969L        // p16
                };
        
        
        ByteBuffer buffer = ByteBuffer.allocate(expectedResults.length * 8);
        for (Long expected : expectedResults)
        {
            if (expected != null)
            {
                buffer.putLong(expected);
            }
        }
        buffer.rewind();
        BBDecoder decoder = new BBDecoder();
        
        decoder.init(buffer);
        int index = 0;
        for (QpidProperty property : properties)
        {
            assertEquals(expectedResults[index++],property.decodeValue(decoder, presenceBytes));            
        }
    }
    
    private QpidProperty createProperty(boolean isOptional, int optionalIndex)
    {
        QpidProperty property = new QpidProperty();
        property.setType(new Uint64());
        if (isOptional) 
        {
            property.markAsOptional(optionalIndex);
        }
        return property;
    }    
}