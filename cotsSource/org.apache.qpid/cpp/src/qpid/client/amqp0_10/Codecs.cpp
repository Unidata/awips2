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
#include "qpid/client/amqp0_10/Codecs.h"
#include "qpid/messaging/Variant.h"
#include "qpid/framing/Array.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/framing/FieldValue.h"
#include "qpid/framing/List.h"
#include <algorithm>
#include <functional>

using namespace qpid::framing;
using namespace qpid::messaging;

namespace qpid {
namespace client {
namespace amqp0_10 {

namespace {
const std::string iso885915("iso-8859-15");
const std::string utf8("utf8");
const std::string utf16("utf16");
const std::string amqp0_10_binary("amqp0-10:binary");
const std::string amqp0_10_bit("amqp0-10:bit");
const std::string amqp0_10_datetime("amqp0-10:datetime");
const std::string amqp0_10_struct("amqp0-10:struct");
}

template <class T, class U, class F> void convert(const T& from, U& to, F f)
{
    std::transform(from.begin(), from.end(), std::inserter(to, to.begin()), f);
}

Variant::Map::value_type toVariantMapEntry(const FieldTable::value_type& in);
FieldTable::value_type toFieldTableEntry(const Variant::Map::value_type& in);
Variant toVariant(boost::shared_ptr<FieldValue> in);
boost::shared_ptr<FieldValue> toFieldValue(const Variant& in);

template <class T, class U, class F> void translate(boost::shared_ptr<FieldValue> in, U& u, F f) 
{
    T t;
    getEncodedValue<T>(in, t);
    convert(t, u, f);
}

template <class T, class U, class F> T* toFieldValueCollection(const U& u, F f) 
{
    typename T::ValueType t;
    convert(u, t, f);
    return new T(t);
}

FieldTableValue* toFieldTableValue(const Variant::Map& map) 
{
    FieldTable ft;
    convert(map, ft, &toFieldTableEntry);
    return new FieldTableValue(ft);
}

ListValue* toListValue(const Variant::List& list) 
{
    List l;
    convert(list, l, &toFieldValue);
    return new ListValue(l);
}

void setEncodingFor(Variant& out, uint8_t code)
{
    switch(code){
      case 0x80: 
      case 0x90: 
      case 0xa0:
        out.setEncoding(amqp0_10_binary);
        break;
      case 0x84:
      case 0x94:
        out.setEncoding(iso885915);
        break;
      case 0x85:
      case 0x95:
        out.setEncoding(utf8);
        break;
      case 0x86:
      case 0x96: 
        out.setEncoding(utf16);
        break;
      case 0xab: 
        out.setEncoding(amqp0_10_struct);
        break;
      default:
        //do nothing
        break;
    }
}

Variant toVariant(boost::shared_ptr<FieldValue> in)
{
    Variant out;
    //based on AMQP 0-10 typecode, pick most appropriate variant type
    switch (in->getType()) {
        //Fixed Width types:
      case 0x01: out.setEncoding(amqp0_10_binary);
      case 0x02: out = in->getIntegerValue<int8_t, 1>(); break;
      case 0x03: out = in->getIntegerValue<uint8_t, 1>(); break;
      case 0x04: break; //TODO: iso-8859-15 char
      case 0x08: out = in->getIntegerValue<bool, 1>(); break;
      case 0x010: out.setEncoding(amqp0_10_binary);
      case 0x011: out = in->getIntegerValue<int16_t, 2>(); break;
      case 0x012: out = in->getIntegerValue<uint16_t, 2>(); break;
      case 0x020: out.setEncoding(amqp0_10_binary);
      case 0x021: out = in->getIntegerValue<int32_t, 4>(); break;
      case 0x022: out = in->getIntegerValue<uint32_t, 4>(); break;
      case 0x023: out = in->get<float>(); break;
      case 0x027: break; //TODO: utf-32 char
      case 0x030: out.setEncoding(amqp0_10_binary);
      case 0x031: out = in->getIntegerValue<int64_t, 8>(); break;
      case 0x038: out.setEncoding(amqp0_10_datetime); //treat datetime as uint64_t, but set encoding
      case 0x032: out = in->getIntegerValue<uint64_t, 8>(); break;
      case 0x033:out = in->get<double>(); break;

        //TODO: figure out whether and how to map values with codes 0x40-0xd8

      case 0xf0: break;//void, which is the default value for Variant
      case 0xf1: out.setEncoding(amqp0_10_bit); break;//treat 'bit' as void, which is the default value for Variant
        
        //Variable Width types:
        //strings:
      case 0x80: 
      case 0x84:
      case 0x85:
      case 0x86:
      case 0x90:
      case 0x94:
      case 0x95:
      case 0x96: 
      case 0xa0:
      case 0xab:
        setEncodingFor(out, in->getType());
        out = in->get<std::string>();
        break;

      case 0xa8:
        out = Variant::Map();
        translate<FieldTable>(in, out.asMap(), &toVariantMapEntry);
        break;

      case 0xa9:
        out = Variant::List();
        translate<List>(in, out.asList(), &toVariant);
        break;
      case 0xaa: //convert amqp0-10 array into variant list
        out = Variant::List();
        translate<Array>(in, out.asList(), &toVariant);
        break;

      default:
        //error?
        break;
    }
    return out;
}

boost::shared_ptr<FieldValue> toFieldValue(const Variant& in)
{
    boost::shared_ptr<FieldValue> out;
    switch (in.getType()) {
      case VAR_VOID: out = boost::shared_ptr<FieldValue>(new VoidValue()); break;
      case VAR_BOOL: out = boost::shared_ptr<FieldValue>(new BoolValue(in.asBool())); break;
      case VAR_UINT8: out = boost::shared_ptr<FieldValue>(new Unsigned8Value(in.asUint8())); break;
      case VAR_UINT16: out = boost::shared_ptr<FieldValue>(new Unsigned16Value(in.asUint16())); break;
      case VAR_UINT32: out = boost::shared_ptr<FieldValue>(new Unsigned32Value(in.asUint32())); break;
      case VAR_UINT64: out = boost::shared_ptr<FieldValue>(new Unsigned64Value(in.asUint64())); break;
      case VAR_INT8: out = boost::shared_ptr<FieldValue>(new Integer8Value(in.asInt8())); break;
      case VAR_INT16: out = boost::shared_ptr<FieldValue>(new Integer16Value(in.asInt16())); break;
      case VAR_INT32: out = boost::shared_ptr<FieldValue>(new Integer32Value(in.asInt32())); break;
      case VAR_INT64: out = boost::shared_ptr<FieldValue>(new Integer64Value(in.asInt64())); break;
      case VAR_FLOAT: out = boost::shared_ptr<FieldValue>(new FloatValue(in.asFloat())); break;
      case VAR_DOUBLE: out = boost::shared_ptr<FieldValue>(new DoubleValue(in.asDouble())); break;
        //TODO: check encoding (and length?) when deciding what AMQP type to treat string as
      case VAR_STRING: out = boost::shared_ptr<FieldValue>(new Str16Value(in.asString())); break;
      case VAR_MAP: 
        //out = boost::shared_ptr<FieldValue>(toFieldValueCollection<FieldTableValue>(in.asMap(), &toFieldTableEntry));
        out = boost::shared_ptr<FieldValue>(toFieldTableValue(in.asMap()));
        break;
      case VAR_LIST: 
        //out = boost::shared_ptr<FieldValue>(toFieldValueCollection<ListValue>(in.asList(), &toFieldValue));
        out = boost::shared_ptr<FieldValue>(toListValue(in.asList()));
        break;
    }
    return out;
}

Variant::Map::value_type toVariantMapEntry(const FieldTable::value_type& in)
{
    return Variant::Map::value_type(in.first, toVariant(in.second));
}

FieldTable::value_type toFieldTableEntry(const Variant::Map::value_type& in)
{
    return FieldTable::value_type(in.first, toFieldValue(in.second));
}

struct EncodeBuffer
{
    char* data;
    Buffer buffer;

    EncodeBuffer(size_t size) : data(new char[size]), buffer(data, size) {}
    ~EncodeBuffer() { delete[] data; }

    template <class T> void encode(T& t) { t.encode(buffer); }

    void getData(std::string& s) { 
        s.assign(data, buffer.getSize()); 
    }
};

struct DecodeBuffer
{
    Buffer buffer;

    DecodeBuffer(const std::string& s) : buffer(const_cast<char*>(s.data()), s.size()) {}

    template <class T> void decode(T& t) { t.decode(buffer); }
    
};

template <class T, class U, class F> void _encode(const U& value, std::string& data, F f)
{
    T t;
    convert(value, t, f);
    EncodeBuffer buffer(t.encodedSize());
    buffer.encode(t);
    buffer.getData(data);
}

template <class T, class U, class F> void _decode(const std::string& data, U& value, F f)
{
    T t;
    DecodeBuffer buffer(data);
    buffer.decode(t);
    convert(t, value, f);
}

void MapCodec::encode(const Variant& value, std::string& data)
{
    _encode<FieldTable>(value.asMap(), data, &toFieldTableEntry);
}

void MapCodec::decode(const std::string& data, Variant& value)
{
    value = Variant::Map();
    _decode<FieldTable>(data, value.asMap(), &toVariantMapEntry);
}

void ListCodec::encode(const Variant& value, std::string& data)
{
    _encode<List>(value.asList(), data, &toFieldValue);
}

void ListCodec::decode(const std::string& data, Variant& value)
{
    value = Variant::List();
    _decode<List>(data, value.asList(), &toVariant);
}

void translate(const Variant::Map& from, FieldTable& to)
{
    convert(from, to, &toFieldTableEntry);
}

void translate(const FieldTable& from, Variant::Map& to)
{
    convert(from, to, &toVariantMapEntry);
}

const std::string ListCodec::contentType("amqp/list");
const std::string MapCodec::contentType("amqp/map");

}}} // namespace qpid::client::amqp0_10
