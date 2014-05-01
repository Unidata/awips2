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
package org.apache.qpid.framing;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.apache.mina.common.ByteBuffer;

import org.apache.qpid.AMQPInvalidClassException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PropertyFieldTableTest extends TestCase
{
    private static final Logger _logger = LoggerFactory.getLogger(PropertyFieldTableTest.class);

    /**
     * Test that setting a similar named value replaces any previous value set on that name
     */
    public void testReplacement()
    {
        FieldTable table1 = new FieldTable();
        // Set a boolean value
        table1.setBoolean("value", true);
        // Check length of table is correct (<Value length> + <type> + <Boolean length>)
        int size = EncodingUtils.encodedShortStringLength("value") + 1 + EncodingUtils.encodedBooleanLength();
        Assert.assertEquals(size, table1.getEncodedSize());

        // reset value to an integer
        table1.setInteger("value", Integer.MAX_VALUE);

        // Check the length has changed accordingly   (<Value length> + <type> + <Integer length>)
        size = EncodingUtils.encodedShortStringLength("value") + 1 + EncodingUtils.encodedIntegerLength();
        Assert.assertEquals(size, table1.getEncodedSize());

        // Check boolean value is null
        Assert.assertEquals(null, table1.getBoolean("value"));
        // ... and integer value is good
        Assert.assertEquals((Integer) Integer.MAX_VALUE, table1.getInteger("value"));
    }

    /**
     * Set a boolean and check that we can only get it back as a boolean and a string
     * Check that attempting to lookup a non existent value returns null
     */
    public void testBoolean()
    {
        FieldTable table1 = new FieldTable();
        table1.setBoolean("value", true);
        Assert.assertTrue(table1.propertyExists("value"));

        // Test Getting right value back
        Assert.assertEquals((Boolean) true, table1.getBoolean("value"));

        // Check we don't get anything back for other gets
        Assert.assertEquals(null, table1.getByte("value"));
        Assert.assertEquals(null, table1.getByte("value"));
        Assert.assertEquals(null, table1.getShort("value"));
        Assert.assertEquals(null, table1.getCharacter("value"));
        Assert.assertEquals(null, table1.getDouble("value"));
        Assert.assertEquals(null, table1.getFloat("value"));
        Assert.assertEquals(null, table1.getInteger("value"));
        Assert.assertEquals(null, table1.getLong("value"));
        Assert.assertEquals(null, table1.getBytes("value"));

        // except value as a string
        Assert.assertEquals("true", table1.getString("value"));

        table1.remove("value");

        // Table should now have zero length for encoding
        checkEmpty(table1);

        // Looking up an invalid value returns null
        Assert.assertEquals(null, table1.getBoolean("Rubbish"));
    }

    /**
     * Set a byte and check that we can only get it back as a byte and a string
     * Check that attempting to lookup a non existent value returns null
     */
    public void testByte()
    {
        FieldTable table1 = new FieldTable();
        table1.setByte("value", Byte.MAX_VALUE);
        Assert.assertTrue(table1.propertyExists("value"));

        // Tets lookups we shouldn't get anything back for other gets
        // we should get right value back for this type ....
        Assert.assertEquals(null, table1.getBoolean("value"));
        Assert.assertEquals(Byte.MAX_VALUE, (byte) table1.getByte("value"));
        Assert.assertEquals(null, table1.getShort("value"));
        Assert.assertEquals(null, table1.getCharacter("value"));
        Assert.assertEquals(null, table1.getDouble("value"));
        Assert.assertEquals(null, table1.getFloat("value"));
        Assert.assertEquals(null, table1.getInteger("value"));
        Assert.assertEquals(null, table1.getLong("value"));
        Assert.assertEquals(null, table1.getBytes("value"));

        // ... and a the string value of it.
        Assert.assertEquals("" + Byte.MAX_VALUE, table1.getString("value"));

        table1.remove("value");
        // Table should now have zero length for encoding
        checkEmpty(table1);

        // Looking up an invalid value returns null
        Assert.assertEquals(null, table1.getByte("Rubbish"));
    }

    /**
     * Set a short and check that we can only get it back as a short and a string
     * Check that attempting to lookup a non existent value returns null
     */
    public void testShort()
    {
        FieldTable table1 = new FieldTable();
        table1.setShort("value", Short.MAX_VALUE);
        Assert.assertTrue(table1.propertyExists("value"));

        // Tets lookups we shouldn't get anything back for other gets
        // we should get right value back for this type ....
        Assert.assertEquals(null, table1.getBoolean("value"));
        Assert.assertEquals(null, table1.getByte("value"));
        Assert.assertEquals(Short.MAX_VALUE, (short) table1.getShort("value"));
        Assert.assertEquals(null, table1.getCharacter("value"));
        Assert.assertEquals(null, table1.getDouble("value"));
        Assert.assertEquals(null, table1.getFloat("value"));
        Assert.assertEquals(null, table1.getInteger("value"));
        Assert.assertEquals(null, table1.getLong("value"));
        Assert.assertEquals(null, table1.getBytes("value"));

        // ... and a the string value of it.
        Assert.assertEquals("" + Short.MAX_VALUE, table1.getString("value"));

        table1.remove("value");
        // Table should now have zero length for encoding
        checkEmpty(table1);

        // Looking up an invalid value returns null
        Assert.assertEquals(null, table1.getShort("Rubbish"));
    }

    /**
     * Set a char and check that we can only get it back as a char
     * Check that attempting to lookup a non existent value returns null
     */
    public void testChar()
    {
        FieldTable table1 = new FieldTable();
        table1.setChar("value", 'c');
        Assert.assertTrue(table1.propertyExists("value"));

        // Tets lookups we shouldn't get anything back for other gets
        // we should get right value back for this type ....
        Assert.assertEquals(null, table1.getBoolean("value"));
        Assert.assertEquals(null, table1.getByte("value"));
        Assert.assertEquals(null, table1.getShort("value"));
        Assert.assertEquals('c', (char) table1.getCharacter("value"));
        Assert.assertEquals(null, table1.getDouble("value"));
        Assert.assertEquals(null, table1.getFloat("value"));
        Assert.assertEquals(null, table1.getInteger("value"));
        Assert.assertEquals(null, table1.getLong("value"));
        Assert.assertEquals(null, table1.getBytes("value"));

        // ... and a the string value of it.
        Assert.assertEquals("c", table1.getString("value"));

        table1.remove("value");

        // Table should now have zero length for encoding
        checkEmpty(table1);

        // Looking up an invalid value returns null
        Assert.assertEquals(null, table1.getCharacter("Rubbish"));
    }

    /**
     * Set a double and check that we can only get it back as a double
     * Check that attempting to lookup a non existent value returns null
     */
    public void testDouble()
    {
        FieldTable table1 = new FieldTable();
        table1.setDouble("value", Double.MAX_VALUE);
        Assert.assertTrue(table1.propertyExists("value"));

        // Tets lookups we shouldn't get anything back for other gets
        // we should get right value back for this type ....
        Assert.assertEquals(null, table1.getBoolean("value"));
        Assert.assertEquals(null, table1.getByte("value"));
        Assert.assertEquals(null, table1.getShort("value"));
        Assert.assertEquals(null, table1.getCharacter("value"));
        Assert.assertEquals(Double.MAX_VALUE, (double) table1.getDouble("value"));
        Assert.assertEquals(null, table1.getFloat("value"));
        Assert.assertEquals(null, table1.getInteger("value"));
        Assert.assertEquals(null, table1.getLong("value"));
        Assert.assertEquals(null, table1.getBytes("value"));

        // ... and a the string value of it.
        Assert.assertEquals("" + Double.MAX_VALUE, table1.getString("value"));
        table1.remove("value");
        // but after a removeKey it doesn't
        Assert.assertFalse(table1.containsKey("value"));

        // Table should now have zero length for encoding
        checkEmpty(table1);

        // Looking up an invalid value returns null
        Assert.assertEquals(null, table1.getDouble("Rubbish"));
    }

    /**
     * Set a float and check that we can only get it back as a float
     * Check that attempting to lookup a non existent value returns null
     */
    public void testFloat()
    {
        FieldTable table1 = new FieldTable();
        table1.setFloat("value", Float.MAX_VALUE);
        Assert.assertTrue(table1.propertyExists("value"));

        // Tets lookups we shouldn't get anything back for other gets
        // we should get right value back for this type ....
        Assert.assertEquals(null, table1.getBoolean("value"));
        Assert.assertEquals(null, table1.getByte("value"));
        Assert.assertEquals(null, table1.getShort("value"));
        Assert.assertEquals(null, table1.getCharacter("value"));
        Assert.assertEquals(null, table1.getDouble("value"));
        Assert.assertEquals(Float.MAX_VALUE, (float) table1.getFloat("value"));
        Assert.assertEquals(null, table1.getInteger("value"));
        Assert.assertEquals(null, table1.getLong("value"));
        Assert.assertEquals(null, table1.getBytes("value"));

        // ... and a the string value of it.
        Assert.assertEquals("" + Float.MAX_VALUE, table1.getString("value"));

        table1.remove("value");
        // but after a removeKey it doesn't
        Assert.assertFalse(table1.containsKey("value"));

        // Table should now have zero length for encoding
        checkEmpty(table1);

        // Looking up an invalid value returns null
        Assert.assertEquals(null, table1.getFloat("Rubbish"));
    }

    /**
     * Set an int and check that we can only get it back as an int
     * Check that attempting to lookup a non existent value returns null
     */
    public void testInt()
    {
        FieldTable table1 = new FieldTable();
        table1.setInteger("value", Integer.MAX_VALUE);
        Assert.assertTrue(table1.propertyExists("value"));

        // Tets lookups we shouldn't get anything back for other gets
        // we should get right value back for this type ....
        Assert.assertEquals(null, table1.getBoolean("value"));
        Assert.assertEquals(null, table1.getByte("value"));
        Assert.assertEquals(null, table1.getShort("value"));
        Assert.assertEquals(null, table1.getCharacter("value"));
        Assert.assertEquals(null, table1.getDouble("value"));
        Assert.assertEquals(null, table1.getFloat("value"));
        Assert.assertEquals(Integer.MAX_VALUE, (int) table1.getInteger("value"));
        Assert.assertEquals(null, table1.getLong("value"));
        Assert.assertEquals(null, table1.getBytes("value"));

        // ... and a the string value of it.
        Assert.assertEquals("" + Integer.MAX_VALUE, table1.getString("value"));

        table1.remove("value");
        // but after a removeKey it doesn't
        Assert.assertFalse(table1.containsKey("value"));

        // Table should now have zero length for encoding
        checkEmpty(table1);

        // Looking up an invalid value returns null
        Assert.assertEquals(null, table1.getInteger("Rubbish"));
    }

    /**
     * Set a long and check that we can only get it back as a long
     * Check that attempting to lookup a non existent value returns null
     */
    public void testLong()
    {
        FieldTable table1 = new FieldTable();
        table1.setLong("value", Long.MAX_VALUE);
        Assert.assertTrue(table1.propertyExists("value"));

        // Tets lookups we shouldn't get anything back for other gets
        // we should get right value back for this type ....
        Assert.assertEquals(null, table1.getBoolean("value"));
        Assert.assertEquals(null, table1.getByte("value"));
        Assert.assertEquals(null, table1.getShort("value"));
        Assert.assertEquals(null, table1.getCharacter("value"));
        Assert.assertEquals(null, table1.getDouble("value"));
        Assert.assertEquals(null, table1.getFloat("value"));
        Assert.assertEquals(null, table1.getInteger("value"));
        Assert.assertEquals(Long.MAX_VALUE, (long) table1.getLong("value"));
        Assert.assertEquals(null, table1.getBytes("value"));

        // ... and a the string value of it.
        Assert.assertEquals("" + Long.MAX_VALUE, table1.getString("value"));

        table1.remove("value");
        // but after a removeKey it doesn't
        Assert.assertFalse(table1.containsKey("value"));

        // Table should now have zero length for encoding
        checkEmpty(table1);

        // Looking up an invalid value returns null
        Assert.assertEquals(null, table1.getLong("Rubbish"));
    }

    /**
     * Set a double and check that we can only get it back as a double
     * Check that attempting to lookup a non existent value returns null
     */
    public void testBytes()
    {
        byte[] bytes = { 99, 98, 97, 96, 95 };

        FieldTable table1 = new FieldTable();
        table1.setBytes("value", bytes);
        Assert.assertTrue(table1.propertyExists("value"));

        // Tets lookups we shouldn't get anything back for other gets
        // we should get right value back for this type ....
        Assert.assertEquals(null, table1.getBoolean("value"));
        Assert.assertEquals(null, table1.getByte("value"));
        Assert.assertEquals(null, table1.getShort("value"));
        Assert.assertEquals(null, table1.getCharacter("value"));
        Assert.assertEquals(null, table1.getDouble("value"));
        Assert.assertEquals(null, table1.getFloat("value"));
        Assert.assertEquals(null, table1.getInteger("value"));
        Assert.assertEquals(null, table1.getLong("value"));
        assertBytesEqual(bytes, table1.getBytes("value"));

        // ... and a the string value of it is null
        Assert.assertEquals(null, table1.getString("value"));

        table1.remove("value");
        // but after a removeKey it doesn't
        Assert.assertFalse(table1.containsKey("value"));

        // Table should now have zero length for encoding
        checkEmpty(table1);

        // Looking up an invalid value returns null
        Assert.assertEquals(null, table1.getBytes("Rubbish"));
    }

    /**
     * Calls all methods that can be used to check the table is empty
     * - getEncodedSize
     * - isEmpty
     * - length
     *
     * @param table to check is empty
     */
    private void checkEmpty(FieldTable table)
    {
        Assert.assertEquals(0, table.getEncodedSize());
        Assert.assertTrue(table.isEmpty());
        Assert.assertEquals(0, table.size());

        Assert.assertEquals(0, table.keySet().size());
    }

    /**
     * Set a String and check that we can only get it back as a String
     * Check that attempting to lookup a non existent value returns null
     */
    public void testString()
    {
        FieldTable table1 = new FieldTable();
        table1.setString("value", "Hello");
        Assert.assertTrue(table1.propertyExists("value"));

        // Tets lookups we shouldn't get anything back for other gets
        // we should get right value back for this type ....
        Assert.assertEquals(null, table1.getBoolean("value"));
        Assert.assertEquals(null, table1.getByte("value"));
        Assert.assertEquals(null, table1.getShort("value"));
        Assert.assertEquals(null, table1.getCharacter("value"));
        Assert.assertEquals(null, table1.getDouble("value"));
        Assert.assertEquals(null, table1.getFloat("value"));
        Assert.assertEquals(null, table1.getInteger("value"));
        Assert.assertEquals(null, table1.getLong("value"));
        Assert.assertEquals(null, table1.getBytes("value"));
        Assert.assertEquals("Hello", table1.getString("value"));

        // Try setting a null value and read it back
        table1.setString("value", null);

        Assert.assertEquals(null, table1.getString("value"));

        // but still contains the value
        Assert.assertTrue(table1.containsKey("value"));

        table1.remove("value");
        // but after a removeKey it doesn't
        Assert.assertFalse(table1.containsKey("value"));

        checkEmpty(table1);

        // Looking up an invalid value returns null
        Assert.assertEquals(null, table1.getString("Rubbish"));

        // Additional Test that haven't been covered for string
        table1.setObject("value", "Hello");
        // Check that it was set correctly
        Assert.assertEquals("Hello", table1.getString("value"));
    }

    /** Check that a nested field table parameter correctly encodes and decodes to a byte buffer. */
    public void testNestedFieldTable()
    {
        byte[] testBytes = new byte[] { 0, 1, 2, 3, 4, 5 };

        FieldTable outerTable = new FieldTable();
        FieldTable innerTable = new FieldTable();

        // Put some stuff in the inner table.
        innerTable.setBoolean("bool", true);
        innerTable.setByte("byte", Byte.MAX_VALUE);
        innerTable.setBytes("bytes", testBytes);
        innerTable.setChar("char", 'c');
        innerTable.setDouble("double", Double.MAX_VALUE);
        innerTable.setFloat("float", Float.MAX_VALUE);
        innerTable.setInteger("int", Integer.MAX_VALUE);
        innerTable.setLong("long", Long.MAX_VALUE);
        innerTable.setShort("short", Short.MAX_VALUE);
        innerTable.setString("string", "hello");
        innerTable.setString("null-string", null);

        // Put the inner table in the outer one.
        outerTable.setFieldTable("innerTable", innerTable);

        // Write the outer table into the buffer.
        final ByteBuffer buffer = ByteBuffer.allocate((int) outerTable.getEncodedSize() + 4);
        outerTable.writeToBuffer(buffer);
        buffer.flip();

        // Extract the table back from the buffer again.
        try
        {
            FieldTable extractedOuterTable = EncodingUtils.readFieldTable(buffer);

            FieldTable extractedTable = extractedOuterTable.getFieldTable("innerTable");

            Assert.assertEquals((Boolean) true, extractedTable.getBoolean("bool"));
            Assert.assertEquals((Byte) Byte.MAX_VALUE, extractedTable.getByte("byte"));
            assertBytesEqual(testBytes, extractedTable.getBytes("bytes"));
            Assert.assertEquals((Character) 'c', extractedTable.getCharacter("char"));
            Assert.assertEquals(Double.MAX_VALUE, extractedTable.getDouble("double"));
            Assert.assertEquals(Float.MAX_VALUE, extractedTable.getFloat("float"));
            Assert.assertEquals((Integer) Integer.MAX_VALUE, extractedTable.getInteger("int"));
            Assert.assertEquals((Long) Long.MAX_VALUE, extractedTable.getLong("long"));
            Assert.assertEquals((Short) Short.MAX_VALUE, extractedTable.getShort("short"));
            Assert.assertEquals("hello", extractedTable.getString("string"));
            Assert.assertEquals(null, extractedTable.getString("null-string"));
        }
        catch (AMQFrameDecodingException e)
        {
            fail("Failed to decode field table with nested inner table.");
        }
    }

    public void testValues()
    {
        FieldTable table = new FieldTable();
        table.setBoolean("bool", true);
        table.setByte("byte", Byte.MAX_VALUE);
        byte[] bytes = { 99, 98, 97, 96, 95 };
        table.setBytes("bytes", bytes);
        table.setChar("char", 'c');
        table.setDouble("double", Double.MAX_VALUE);
        table.setFloat("float", Float.MAX_VALUE);
        table.setInteger("int", Integer.MAX_VALUE);
        table.setLong("long", Long.MAX_VALUE);
        table.setShort("short", Short.MAX_VALUE);
        table.setString("string", "Hello");
        table.setString("null-string", null);

        table.setObject("object-bool", true);
        table.setObject("object-byte", Byte.MAX_VALUE);
        table.setObject("object-bytes", bytes);
        table.setObject("object-char", 'c');
        table.setObject("object-double", Double.MAX_VALUE);
        table.setObject("object-float", Float.MAX_VALUE);
        table.setObject("object-int", Integer.MAX_VALUE);
        table.setObject("object-long", Long.MAX_VALUE);
        table.setObject("object-short", Short.MAX_VALUE);
        table.setObject("object-string", "Hello");

        Assert.assertEquals((Boolean) true, table.getBoolean("bool"));
        Assert.assertEquals((Byte) Byte.MAX_VALUE, table.getByte("byte"));
        assertBytesEqual(bytes, table.getBytes("bytes"));
        Assert.assertEquals((Character) 'c', table.getCharacter("char"));
        Assert.assertEquals(Double.MAX_VALUE, table.getDouble("double"));
        Assert.assertEquals(Float.MAX_VALUE, table.getFloat("float"));
        Assert.assertEquals((Integer) Integer.MAX_VALUE, table.getInteger("int"));
        Assert.assertEquals((Long) Long.MAX_VALUE, table.getLong("long"));
        Assert.assertEquals((Short) Short.MAX_VALUE, table.getShort("short"));
        Assert.assertEquals("Hello", table.getString("string"));
        Assert.assertEquals(null, table.getString("null-string"));

        Assert.assertEquals(true, table.getObject("object-bool"));
        Assert.assertEquals(Byte.MAX_VALUE, table.getObject("object-byte"));
        assertBytesEqual(bytes, (byte[]) table.getObject("object-bytes"));
        Assert.assertEquals('c', table.getObject("object-char"));
        Assert.assertEquals(Double.MAX_VALUE, table.getObject("object-double"));
        Assert.assertEquals(Float.MAX_VALUE, table.getObject("object-float"));
        Assert.assertEquals(Integer.MAX_VALUE, table.getObject("object-int"));
        Assert.assertEquals(Long.MAX_VALUE, table.getObject("object-long"));
        Assert.assertEquals(Short.MAX_VALUE, table.getObject("object-short"));
        Assert.assertEquals("Hello", table.getObject("object-string"));
    }

    public void testwriteBuffer()
    {
        byte[] bytes = { 99, 98, 97, 96, 95 };

        FieldTable table = new FieldTable();
        table.setBoolean("bool", true);
        table.setByte("byte", Byte.MAX_VALUE);

        table.setBytes("bytes", bytes);
        table.setChar("char", 'c');
        table.setDouble("double", Double.MAX_VALUE);
        table.setFloat("float", Float.MAX_VALUE);
        table.setInteger("int", Integer.MAX_VALUE);
        table.setLong("long", Long.MAX_VALUE);
        table.setShort("short", Short.MAX_VALUE);
        table.setString("string", "hello");
        table.setString("null-string", null);

        final ByteBuffer buffer = ByteBuffer.allocate((int) table.getEncodedSize() + 4); // FIXME XXX: Is cast a problem?

        table.writeToBuffer(buffer);

        buffer.flip();

        long length = buffer.getUnsignedInt();

        try
        {
            FieldTable table2 = new FieldTable(buffer, length);

            Assert.assertEquals((Boolean) true, table2.getBoolean("bool"));
            Assert.assertEquals((Byte) Byte.MAX_VALUE, table2.getByte("byte"));
            assertBytesEqual(bytes, table2.getBytes("bytes"));
            Assert.assertEquals((Character) 'c', table2.getCharacter("char"));
            Assert.assertEquals(Double.MAX_VALUE, table2.getDouble("double"));
            Assert.assertEquals(Float.MAX_VALUE, table2.getFloat("float"));
            Assert.assertEquals((Integer) Integer.MAX_VALUE, table2.getInteger("int"));
            Assert.assertEquals((Long) Long.MAX_VALUE, table2.getLong("long"));
            Assert.assertEquals((Short) Short.MAX_VALUE, table2.getShort("short"));
            Assert.assertEquals("hello", table2.getString("string"));
            Assert.assertEquals(null, table2.getString("null-string"));

        }
        catch (AMQFrameDecodingException e)
        {
            e.printStackTrace();
            fail("PFT should be instantiated from bytes." + e.getCause());
        }
    }

    public void testEncodingSize()
    {
        FieldTable result = new FieldTable();
        int size = 0;

        result.setBoolean("boolean", true);
        size += 1 + EncodingUtils.encodedShortStringLength("boolean") + EncodingUtils.encodedBooleanLength();
        Assert.assertEquals(size, result.getEncodedSize());

        result.setByte("byte", (byte) Byte.MAX_VALUE);
        size += 1 + EncodingUtils.encodedShortStringLength("byte") + EncodingUtils.encodedByteLength();
        Assert.assertEquals(size, result.getEncodedSize());

        byte[] _bytes = { 99, 98, 97, 96, 95 };

        result.setBytes("bytes", _bytes);
        size += 1 + EncodingUtils.encodedShortStringLength("bytes") + 4 + _bytes.length;
        Assert.assertEquals(size, result.getEncodedSize());

        result.setChar("char", (char) 'c');
        size += 1 + EncodingUtils.encodedShortStringLength("char") + EncodingUtils.encodedCharLength();
        Assert.assertEquals(size, result.getEncodedSize());

        result.setDouble("double", (double) Double.MAX_VALUE);
        size += 1 + EncodingUtils.encodedShortStringLength("double") + EncodingUtils.encodedDoubleLength();
        Assert.assertEquals(size, result.getEncodedSize());

        result.setFloat("float", (float) Float.MAX_VALUE);
        size += 1 + EncodingUtils.encodedShortStringLength("float") + EncodingUtils.encodedFloatLength();
        Assert.assertEquals(size, result.getEncodedSize());

        result.setInteger("int", (int) Integer.MAX_VALUE);
        size += 1 + EncodingUtils.encodedShortStringLength("int") + EncodingUtils.encodedIntegerLength();
        Assert.assertEquals(size, result.getEncodedSize());

        result.setLong("long", (long) Long.MAX_VALUE);
        size += 1 + EncodingUtils.encodedShortStringLength("long") + EncodingUtils.encodedLongLength();
        Assert.assertEquals(size, result.getEncodedSize());

        result.setShort("short", (short) Short.MAX_VALUE);
        size += 1 + EncodingUtils.encodedShortStringLength("short") + EncodingUtils.encodedShortLength();
        Assert.assertEquals(size, result.getEncodedSize());

        result.setString("result", "Hello");
        size += 1 + EncodingUtils.encodedShortStringLength("result") + EncodingUtils.encodedLongStringLength("Hello");
        Assert.assertEquals(size, result.getEncodedSize());

        result.setObject("object-bool", true);
        size += 1 + EncodingUtils.encodedShortStringLength("object-bool") + EncodingUtils.encodedBooleanLength();
        Assert.assertEquals(size, result.getEncodedSize());

        result.setObject("object-byte", Byte.MAX_VALUE);
        size += 1 + EncodingUtils.encodedShortStringLength("object-byte") + EncodingUtils.encodedByteLength();
        Assert.assertEquals(size, result.getEncodedSize());

        result.setObject("object-bytes", _bytes);
        size += 1 + EncodingUtils.encodedShortStringLength("object-bytes") + 4 + _bytes.length;
        Assert.assertEquals(size, result.getEncodedSize());

        result.setObject("object-char", 'c');
        size += 1 + EncodingUtils.encodedShortStringLength("object-char") + EncodingUtils.encodedCharLength();
        Assert.assertEquals(size, result.getEncodedSize());

        result.setObject("object-double", Double.MAX_VALUE);
        size += 1 + EncodingUtils.encodedShortStringLength("object-double") + EncodingUtils.encodedDoubleLength();
        Assert.assertEquals(size, result.getEncodedSize());

        result.setObject("object-float", Float.MAX_VALUE);
        size += 1 + EncodingUtils.encodedShortStringLength("object-float") + EncodingUtils.encodedFloatLength();
        Assert.assertEquals(size, result.getEncodedSize());

        result.setObject("object-int", Integer.MAX_VALUE);
        size += 1 + EncodingUtils.encodedShortStringLength("object-int") + EncodingUtils.encodedIntegerLength();
        Assert.assertEquals(size, result.getEncodedSize());

        result.setObject("object-long", Long.MAX_VALUE);
        size += 1 + EncodingUtils.encodedShortStringLength("object-long") + EncodingUtils.encodedLongLength();
        Assert.assertEquals(size, result.getEncodedSize());

        result.setObject("object-short", Short.MAX_VALUE);
        size += 1 + EncodingUtils.encodedShortStringLength("object-short") + EncodingUtils.encodedShortLength();
        Assert.assertEquals(size, result.getEncodedSize());

    }

    // public void testEncodingSize1()
    // {
    // PropertyFieldTable table = new PropertyFieldTable();
    // int length = 0;
    // result.put("one", 1L);
    // length = EncodingUtils.encodedShortStringLength("one");
    // length += 1 + EncodingUtils.encodedLongLength();
    // assertEquals(length, result.getEncodedSize());
    //
    // result.put("two", 2L);
    // length += EncodingUtils.encodedShortStringLength("two");
    // length += 1 + EncodingUtils.encodedLongLength();
    // assertEquals(length, result.getEncodedSize());
    //
    // result.put("three", 3L);
    // length += EncodingUtils.encodedShortStringLength("three");
    // length += 1 + EncodingUtils.encodedLongLength();
    // assertEquals(length, result.getEncodedSize());
    //
    // result.put("four", 4L);
    // length += EncodingUtils.encodedShortStringLength("four");
    // length += 1 + EncodingUtils.encodedLongLength();
    // assertEquals(length, result.getEncodedSize());
    //
    // result.put("five", 5L);
    // length += EncodingUtils.encodedShortStringLength("five");
    // length += 1 + EncodingUtils.encodedLongLength();
    // assertEquals(length, result.getEncodedSize());
    //
    // //fixme should perhaps be expanded to incorporate all types.
    //
    // final ByteBuffer buffer = ByteBuffer.allocate((int) result.getEncodedSize()); // FIXME XXX: Is cast a problem?
    //
    // result.writeToBuffer(buffer);
    //
    // buffer.flip();
    //
    // long length = buffer.getUnsignedInt();
    //
    // try
    // {
    // PropertyFieldTable table2 = new PropertyFieldTable(buffer, length);
    //
    // Assert.assertEquals((Long) 1L, table2.getLong("one"));
    // Assert.assertEquals((Long) 2L, table2.getLong("two"));
    // Assert.assertEquals((Long) 3L, table2.getLong("three"));
    // Assert.assertEquals((Long) 4L, table2.getLong("four"));
    // Assert.assertEquals((Long) 5L, table2.getLong("five"));
    // }
    // catch (AMQFrameDecodingException e)
    // {
    // e.printStackTrace();
    // fail("PFT should be instantiated from bytes." + e.getCause());
    // }
    //
    // }

    /**
     * Additional test for setObject
     */
    public void testSetObject()
    {
        FieldTable table = new FieldTable();

        // Try setting a non primative object

        try
        {
            table.setObject("value", this);
            fail("Only primative values allowed in setObject");
        }
        catch (AMQPInvalidClassException iae)
        {
            // normal path
        }
        // so length should be zero
        Assert.assertEquals(0, table.getEncodedSize());
    }

    /**
     * Additional test checkPropertyName doesn't accept Null
     */
    public void testCheckPropertyNameasNull()
    {
        FieldTable table = new FieldTable();

        try
        {
            table.setObject((String) null, "String");
            fail("Null property name is not allowed");
        }
        catch (IllegalArgumentException iae)
        {
            // normal path
        }
        // so length should be zero
        Assert.assertEquals(0, table.getEncodedSize());
    }

    /**
     * Additional test checkPropertyName doesn't accept an empty String
     */
    public void testCheckPropertyNameasEmptyString()
    {
        FieldTable table = new FieldTable();

        try
        {
            table.setObject("", "String");
            fail("empty property name is not allowed");
        }
        catch (IllegalArgumentException iae)
        {
            // normal path
        }
        // so length should be zero
        Assert.assertEquals(0, table.getEncodedSize());
    }

    /**
     * Additional test checkPropertyName doesn't accept an empty String
     */
    public void testCheckPropertyNamehasMaxLength()
    {
        String oldVal = System.getProperty("STRICT_AMQP");
        System.setProperty("STRICT_AMQP", "true");
        FieldTable table = new FieldTable();

        StringBuffer longPropertyName = new StringBuffer(129);

        for (int i = 0; i < 129; i++)
        {
            longPropertyName.append("x");
        }

        try
        {
            table.setObject(longPropertyName.toString(), "String");
            fail("property name must be < 128 characters");
        }
        catch (IllegalArgumentException iae)
        {
            // normal path
        }
        // so length should be zero
        Assert.assertEquals(0, table.getEncodedSize());
        if (oldVal != null)
        {
            System.setProperty("STRICT_AMQP", oldVal);
        }
        else
        {
            System.clearProperty("STRICT_AMQP");
        }
    }

    /**
     * Additional test checkPropertyName starts with a letter
     */
    public void testCheckPropertyNameStartCharacterIsLetter()
    {
        String oldVal = System.getProperty("STRICT_AMQP");
        System.setProperty("STRICT_AMQP", "true");
        FieldTable table = new FieldTable();

        // Try a name that starts with a number
        try
        {
            table.setObject("1", "String");
            fail("property name must start with a letter");
        }
        catch (IllegalArgumentException iae)
        {
            // normal path
        }
        // so length should be zero
        Assert.assertEquals(0, table.getEncodedSize());
        if (oldVal != null)
        {
            System.setProperty("STRICT_AMQP", oldVal);
        }
        else
        {
            System.clearProperty("STRICT_AMQP");
        }
    }

    /**
     * Additional test checkPropertyName starts with a hash or a dollar
     */
    public void testCheckPropertyNameStartCharacterIsHashorDollar()
    {
        String oldVal = System.getProperty("STRICT_AMQP");
        System.setProperty("STRICT_AMQP", "true");
        FieldTable table = new FieldTable();

        // Try a name that starts with a number
        try
        {
            table.setObject("#", "String");
            table.setObject("$", "String");
        }
        catch (IllegalArgumentException iae)
        {
            fail("property name are allowed to start with # and $s");
        }

        if (oldVal != null)
        {
            System.setProperty("STRICT_AMQP", oldVal);
        }
        else
        {
            System.clearProperty("STRICT_AMQP");
        }
    }

    /**
     * Additional test to test the contents of the table
     */
    public void testContents()
    {
        FieldTable table = new FieldTable();

        table.setObject("StringProperty", "String");

        Assert.assertEquals("String", table.getString("StringProperty"));

        // Test Clear

        table.clear();

        checkEmpty(table);
    }

    /**
     * Test the contents of the sets
     */
    public void testSets()
    {

        FieldTable table = new FieldTable();

        table.setObject("n1", "1");
        table.setObject("n2", "2");
        table.setObject("n3", "3");

        Assert.assertEquals("1", table.getObject("n1"));
        Assert.assertEquals("2", table.getObject("n2"));
        Assert.assertEquals("3", table.getObject("n3"));

    }

    private void assertBytesEqual(byte[] expected, byte[] actual)
    {
        Assert.assertEquals(expected.length, actual.length);

        for (int index = 0; index < expected.length; index++)
        {
            Assert.assertEquals(expected[index], actual[index]);
        }
    }

    private void assertBytesNotEqual(byte[] expected, byte[] actual)
    {
        Assert.assertEquals(expected.length, actual.length);

        for (int index = 0; index < expected.length; index++)
        {
            Assert.assertFalse(expected[index] == actual[index]);
        }
    }

    public static junit.framework.Test suite()
    {
        return new junit.framework.TestSuite(PropertyFieldTableTest.class);
    }

}
