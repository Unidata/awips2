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

import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.qpid.transport.RangeSet;
import org.apache.qpid.transport.Struct;


/**
 * Encoder interface.
 * Each concrete implementor must specify how to encode given values.
 *
 * @author Rafael H. Schloming
 */
public interface Encoder
{
	/**
	 * The uint8 type is an 8-bit unsigned integral value.
	 * 
	 * @param b the unsigned integer to be encoded.
	 */
    void writeUint8(short b);
    
	/**
	 *The uint16 type is a 16-bit unsigned integral value encoded in network byte order.
	 * 
	 * @param s the unsigned integer to be encoded.
	 */    
    void writeUint16(int s);

	/**
	 *The uint32 type is a 32-bit unsigned integral value encoded in network byte order.
	 * 
	 * @param i the unsigned integer to be encoded.
	 */    
    void writeUint32(long i);
    
	/**
	 * The uint64 type is a 64-bit unsigned integral value encoded in network byte order.
	 * 
	 * @param b the unsigned integer to be encoded.
	 */    
    void writeUint64(long l);

	/**
	 * The datetime type encodes a date and time using the 64 bit POSIX time_t format.
	 * 
	 * @param l the datetime (as long) to be encoded.
	 */    
    void writeDatetime(long l);
    
	/**
	 * The uuid type encodes a universally unique id as defined by RFC-4122. 
	 * The format and operations for this type can be found in section 4.1.2 of RFC-4122.
	 * 
	 * @param uuid the uuid to be encoded.
	 */    
    void writeUuid(UUID uuid);

	/**
	 *The sequence-no type encodes, in network byte order, a serial number as defined in RFC-1982. 
	 * 
	 * @param s the sequence number to be encoded.
	 */    
    void writeSequenceNo(int s);
    
    void writeSequenceSet(RangeSet ranges); // XXX
    void writeByteRanges(RangeSet ranges); // XXX

	/**
	 * The str8 type encodes up to 255 octets worth of UTF-8 unicode. 
	 * The number of octets of unicode is first encoded as an 8-bit unsigned integral value. 
	 * This is followed by the actual UTF-8 unicode. 
	 * Note that the encoded size refers to the number of octets of unicode, not necessarily the number of characters since 
	 * the UTF-8 unicode may include multi-byte character sequences.
	 * 
	 * @param s the string to be encoded.
	 */    
    void writeStr8(String s);

	/**
	 * The str16 type encodes up to 65535 octets worth of UTF-8 unicode. 
	 * The number of octets is first encoded as a 16-bit unsigned integral value in network byte order. 
	 * This is followed by the actual UTF-8 unicode. 
	 * Note that the encoded size refers to the number of octets of unicode, not necessarily the number of unicode 
	 * characters since the UTF-8 unicode may include multi-byte character sequences.
	 * 
	 * @param s the string to be encoded.
	 */    
    void writeStr16(String s);

	/**
	 * The vbin8 type encodes up to 255 octets of opaque binary data. 
	 * The number of octets is first encoded as an 8-bit unsigned integral value. 
	 * This is followed by the actual data.
	 * 
	 * @param bytes the byte array to be encoded.
	 */    
    void writeVbin8(byte[] bytes);
    
    /**
     * The vbin16 type encodes up to 65535 octets of opaque binary data. 
     * The number of octets is first encoded as a 16-bit unsigned integral value in network byte order. 
     * This is followed by the actual data.
     * 
     * @param bytes the byte array to be encoded.
     */
    void writeVbin16(byte[] bytes);
    
    /**
     * The vbin32 type encodes up to 4294967295 octets of opaque binary data. 
     * The number of octets is first encoded as a 32-bit unsigned integral value in network byte order. 
     * This is followed by the actual data.
     * 
     * @param bytes the byte array to be encoded.
     */
    void writeVbin32(byte[] bytes);

    /**
     * The struct32 type describes any coded struct with a 32-bit (4 octet) size. 
     * The type is restricted to be only coded structs with a 32-bit size, consequently the first six octets of any encoded 
     * value for this type MUST always contain the size, class-code, and struct-code in that order.
     * The size is encoded as a 32-bit unsigned integral value in network byte order that is equal to the size of the 
     * encoded field-data, packing-flags, class-code, and struct-code. The class-code is a single octet that may be set to any 
     * valid class code. 
     * The struct-code is a single octet that may be set to any valid struct code within the given class-code. 
     * The first six octets are then followed by the packing flags and encoded field data. 
     * The presence and quantity of packingflags, as well as the specific fields are determined by the struct definition 
     * identified with the encoded class-code and struct-code.
     * 
     * @param struct the struct to be encoded.
     */
    void writeStruct32(Struct struct);
    
    /**
     * A map is a set of distinct keys where each key has an associated (type,value) pair. 
     * The triple of the key, type, and value, form an entry within a map. Each entry within a given map MUST have a 
     * distinct key. 
     * A map is encoded as a size in octets, a count of the number of entries, followed by the encoded entries themselves.
     * An encoded map may contain up to (4294967295 - 4) octets worth of encoded entries. 
     * The size is encoded as a 32-bit unsigned integral value in network byte order equal to the number of octets worth of 
     * encoded entries plus 4. (The extra 4 octets is added for the entry count.) 
     * The size is then followed by the number of entries encoded as a 32-bit unsigned integral value in network byte order. 
     * Finally the entries are encoded sequentially. 
     * An entry is encoded as the key, followed by the type, and then the value. The key is always a string encoded as a str8.
     * The type is a single octet that may contain any valid AMQP type code. 
     * The value is encoded according to the rules defined by the type code for that entry.
     * 
     * @param map the map to be encoded.
     */
    void writeMap(Map<String,Object> map);
    
    /**
     * A list is an ordered sequence of (type, value) pairs. The (type, value) pair forms an item within the list. 
     * The list may contain items of many distinct types. A list is encoded as a size in octets, followed by a count of the 
     * number of items, followed by the items themselves encoded in their defined order.
     * An encoded list may contain up to (4294967295 - 4) octets worth of encoded items. 
     * The size is encoded as a 32-bit unsigned integral value in network byte order equal to the number of octets worth 
     * of encoded items plus 4. (The extra4 octets is added for the item count.) 
     * The size is then followed by the number of items encoded as a 32-bit unsigned integral value in network byte order. 
     * Finally the items are encoded sequentially in their defined order. 
     * An item is encoded as the type followed by the value. The type is a single octet that may contain any valid AMQP type 
     * code. 
     * The value is encoded according to the rules defined by the type code for that item.
     * 
     * @param list the list to be encoded.
     */
    void writeList(List<Object> list);
    
    /**
     * An array is an ordered sequence of values of the same type. 
     * The array is encoded in as a size in octets, followed by a type code, then a count of the number values in the array, 
     * and finally the values encoded in their defined order.
     * An encoded array may contain up to (4294967295 - 5) octets worth of encoded values. 
     * The size is encoded as a 32-bit unsigned integral value in network byte order equal to the number of octets worth of 
     * encoded values plus 5. (The extra 5 octets consist of 4 octets for the count of the number of values, and one octet to 
     * hold the type code for the items inthe array.) 
     * The size is then followed by a single octet that may contain any valid AMQP type code. 
     * The type code is then followed by the number of values encoded as a 32-bit unsigned integral value in network byte 
     * order. 
     * Finally the values are encoded sequentially in their defined order according to the rules defined by the type code for 
     * the array.
     * 
     * @param array the array to be encoded.
     */
    void writeArray(List<Object> array);

    /**
     * The struct32 type describes any coded struct with a 32-bit (4 octet) size. 
     * The type is restricted to be only coded structs with a 32-bit size, consequently the first six octets of any encoded 
     * value for this type MUST always contain the size, class-code, and struct-code in that order.
     * The size is encoded as a 32-bit unsigned integral value in network byte order that is equal to the size of the 
     * encoded field-data, packing-flags, class-code, and struct-code. The class-code is a single octet that may be set to any 
     * valid class code. 
     * The struct-code is a single octet that may be set to any valid struct code within the given class-code. 
     * The first six octets are then followed by the packing flags and encoded field data. 
     * The presence and quantity of packingflags, as well as the specific fields are determined by the struct definition 
     * identified with the encoded class-code and struct-code.
     * 
     * @param type the type of the struct.
     * @param struct the struct to be encoded.
     */
    void writeStruct(int type, Struct struct);
    
    /**
     * The float type encodes a single precision 32-bit floating point number. 
     * The format and operations are defined by the IEEE 754 standard for 32-bit single precision floating point numbers.
     * 
     * @param aFloat the float to be encoded.
     */
    void writeFloat(float aFloat);
    
    /**
     * The double type encodes a double precision 64-bit floating point number. 
     * The format and operations are defined by the IEEE 754 standard for 64-bit double precision floating point numbers.
     * 
     * @param aDouble the double to be encoded.
     */
    void writeDouble(double aDouble);
    
    /**
     * The int8 type is a signed integral value encoded using an 8-bit two's complement representation.
     * 
     * @param aByte the integer to be encoded.
     */
    void writeInt8(byte aByte);
    
    /**
     * The int16 type is a signed integral value encoded using a 16-bit two's complement representation in network byte order.
     * 
     * @param aShort the integer to be encoded.
     */
    void writeInt16(short aShort);
    
    /**
     * The int32 type is a signed integral value encoded using a 32-bit two's complement representation in network byte order.
     * 
     * @param anInt the integer to be encoded.
     */
    void writeInt32(int anInt);
    
    /**
     * The int64 type is a signed integral value encoded using a 64-bit two's complement representation in network byte order.
     * 
     * @param aLong the integer to be encoded.
     */
    void writeInt64(long aLong);
    
    /**
     * The bin128 type consists of 16 consecutive octets of opaque binary data.
     * 
     * @param bytes the bytes array to be encoded.
     */
    void writeBin128(byte [] bytes);
    
    /**
     * Encodes the AMQP magic number.
     */
    void writeMagicNumber();
}