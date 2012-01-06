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
 * Decoder interface.
 * Each concrete implementor must specify how to decode given values.
 * 
 * @author Rafael H. Schloming
 */
public interface Decoder
{
	/**
	 * Tells whether there are any remaining byte(s) to be read.
	 * 
	 * @return true if there are remaining bytes, false otherwise.
	 */
    boolean hasRemaining();

	/**
	 * The uint8 type is an 8-bit unsigned integral value.
	 * 
	 * @return an 8-bit unsigned integral value.
	 */
    short readUint8();
    
	/**
	 *The uint16 type is a 16-bit unsigned integral value encoded in network byte order.
	 * 
	 * @return a 16-bit unsigned integral value encoded in network byte order.
	 */    
    int readUint16();
    
	/**
	 *The uint32 type is a 32-bit unsigned integral value encoded in network byte order.
	 * 
	 * @return a 32-bit unsigned integral value encoded in network byte order.
	 */  
    long readUint32();
    
	/**
	 * The uint64 type is a 64-bit unsigned integral value encoded in network byte order.
	 * 
	 * @return a 64-bit unsigned integral value encoded in network byte order.
	 */    
    long readUint64();

	/**
	 * The datetime type encodes a date and time using the 64 bit POSIX time_t format.
	 * 
	 * @return a date and time using the 64 bit POSIX time_t format.
	 */ 
    long readDatetime();
    
	/**
	 * The uuid type encodes a universally unique id as defined by RFC-4122. 
	 * The format and operations for this type can be found in section 4.1.2 of RFC-4122.
	 * 
	 * return a universally unique id as defined by RFC-4122.
	 */    
    UUID readUuid();

	/**
//	 *The sequence-no type encodes, in network byte order, a serial number as defined in RFC-1982. 
	 * 
	 * @return a serial number as defined in RFC-1982. 
	 */    
    int readSequenceNo();
    
    RangeSet readSequenceSet(); // XXX
    RangeSet readByteRanges(); // XXX

	/**
	 * The str8 type encodes up to 255 octets worth of UTF-8 unicode. 
	 * The number of octets of unicode is first encoded as an 8-bit unsigned integral value. 
	 * This is followed by the actual UTF-8 unicode. 
	 * Note that the encoded size refers to the number of octets of unicode, not necessarily the number of characters since 
	 * the UTF-8 unicode may include multi-byte character sequences.
	 * 
	 * @return a string.
	 */
    String readStr8();
    
	/**
	 * The str16 type encodes up to 65535 octets worth of UTF-8 unicode. 
	 * The number of octets is first encoded as a 16-bit unsigned integral value in network byte order. 
	 * This is followed by the actual UTF-8 unicode. 
	 * Note that the encoded size refers to the number of octets of unicode, not necessarily the number of unicode 
	 * characters since the UTF-8 unicode may include multi-byte character sequences.
	 * 
	 * return a string.
	 */
    String readStr16();

	/**
	 * The vbin8 type encodes up to 255 octets of opaque binary data. 
	 * 
	 * return a byte array.
	 */
    byte[] readVbin8();
    
    /**
     * The vbin16 type encodes up to 65535 octets of opaque binary data.
     *  
     * @return the corresponding byte array.
     */    
    byte[] readVbin16();
    
    /**
     *  The vbin32 type encodes up to 4294967295 octets of opaque binary data. 
     *  
     * @return the corresponding byte array.
     */
    byte[] readVbin32();

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
     * @return the decoded struct.
     */
    Struct readStruct32();
    
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
     * @return the decoded map.
     */
    Map<String,Object> readMap();
    
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
     * @return the decoded list.
     */
    List<Object> readList();
    
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
     * @return the decoded array.
     */
    List<Object> readArray();

    /**
     * 
     * @param type the type of the struct.
     * @return the decoded struct.
     */
    Struct readStruct(int type);
    
    /**
     * The float type encodes a single precision 32-bit floating point number. 
     * The format and operations are defined by the IEEE 754 standard for 32-bit single precision floating point numbers.
     * 
     * @return the decoded float.
     */
    float readFloat();
    
    /**
     * The double type encodes a double precision 64-bit floating point number. 
     * The format and operations are defined by the IEEE 754 standard for 64-bit double precision floating point numbers.
     * 
     * @return the decoded double
     */
    double readDouble();
    
    /**
     * The int8 type is a signed integral value encoded using an 8-bit two's complement representation.
     * 
     * @return the decoded integer.
     */
    byte readInt8();
    
    /**
     * The int16 type is a signed integral value encoded using a 16-bit two's complement representation in network byte order.
     * 
     * @return the decoded integer.
     */
    short readInt16();
    
    /**
     * The int32 type is a signed integral value encoded using a 32-bit two's complement representation in network byte order.
     * 
     * @return the decoded integer.
     */
    int readInt32();
    
    /**
     * The int64 type is a signed integral value encoded using a 64-bit two's complement representation in network byte order.
     * 
     * @return the decoded integer (as long).
     */
    long readInt64();
    
    /**
     * The bin128 type consists of 16 consecutive octets of opaque binary data.
     * 
     * @return the decoded byte array.
     */
    byte [] readBin128();    
    
    /**
     * Reads the remaining bytes on the underlying buffer.
     * 
     * @return the remaining bytes on the underlying buffer.
     */
    byte[] readReaminingBytes ();
    
    /**
     * Reads the given number of bytes.
     * 
     * @param howManyBytes how many bytes need to be read?
     * @return a byte array containing the requested data.
     */
    byte[] readBytes (int howManyBytes);
}