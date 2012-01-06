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

#include "qpid/console/Value.h"
#include "qpid/framing/Buffer.h"

using namespace qpid;
using namespace qpid::console;
using namespace std;

string NullValue::str() const
{
    return "<Null>";
}

RefValue::RefValue(framing::Buffer& buffer)
{
    uint64_t first = buffer.getLongLong();
    uint64_t second = buffer.getLongLong();
    value.setValue(first, second);
}

string RefValue::str() const
{
    stringstream s;
    s << value;
    return s.str();
}

string UintValue::str() const
{
    stringstream s;
    s << value;
    return s.str();
}

string IntValue::str() const
{
    stringstream s;
    s << value;
    return s.str();
}

string Uint64Value::str() const
{
    stringstream s;
    s << value;
    return s.str();
}

string Int64Value::str() const
{
    stringstream s;
    s << value;
    return s.str();
}

StringValue::StringValue(framing::Buffer& buffer, int tc)
{
    if (tc == 6)
        buffer.getShortString(value);
    else
        buffer.getMediumString(value);
}

string BoolValue::str() const
{
    return value ? "T" : "F";
}

string FloatValue::str() const
{
    stringstream s;
    s << value;
    return s.str();
}

string DoubleValue::str() const
{
    stringstream s;
    s << value;
    return s.str();
}

UuidValue::UuidValue(framing::Buffer& buffer)
{
    value.decode(buffer);
}

string MapValue::str() const
{
    stringstream s;
    s << value;
    return s.str();
}

MapValue::MapValue(framing::Buffer& buffer)
{
    value.decode(buffer);
}


Value::Ptr ValueFactory::newValue(int typeCode, framing::Buffer& buffer)
{
    switch (typeCode) {
    case 1:  return Value::Ptr(new UintValue(buffer.getOctet()));      // U8
    case 2:  return Value::Ptr(new UintValue(buffer.getShort()));      // U16
    case 3:  return Value::Ptr(new UintValue(buffer.getLong()));       // U32
    case 4:  return Value::Ptr(new Uint64Value(buffer.getLongLong())); // U64
    case 6:  return Value::Ptr(new StringValue(buffer, 6));            // SSTR
    case 7:  return Value::Ptr(new StringValue(buffer, 7));            // LSTR
    case 8:  return Value::Ptr(new Int64Value(buffer.getLongLong()));  // ABSTIME
    case 9:  return Value::Ptr(new Uint64Value(buffer.getLongLong())); // DELTATIME
    case 10: return Value::Ptr(new RefValue(buffer));                  // REF
    case 11: return Value::Ptr(new BoolValue(buffer.getOctet()));      // BOOL
    case 12: return Value::Ptr(new FloatValue(buffer.getFloat()));     // FLOAT
    case 13: return Value::Ptr(new DoubleValue(buffer.getDouble()));   // DOUBLE
    case 14: return Value::Ptr(new UuidValue(buffer));                 // UUID
    case 15: return Value::Ptr(new MapValue(buffer));                  // MAP
    case 16: return Value::Ptr(new IntValue(buffer.getOctet()));       // S8
    case 17: return Value::Ptr(new IntValue(buffer.getShort()));       // S16
    case 18: return Value::Ptr(new IntValue(buffer.getLong()));        // S32
    case 19: return Value::Ptr(new Int64Value(buffer.getLongLong()));  // S64
    }

    return Value::Ptr();
}

void ValueFactory::encodeValue(int typeCode, Value::Ptr value, framing::Buffer& buffer)
{
    switch (typeCode) {
    case 1:  buffer.putOctet(value->asUint()); return;          // U8
    case 2:  buffer.putShort(value->asUint()); return;          // U16
    case 3:  buffer.putLong(value->asUint()); return;           // U32
    case 4:  buffer.putLongLong(value->asUint64()); return;     // U64
    case 6:  buffer.putShortString(value->asString()); return;  // SSTR
    case 7:  buffer.putMediumString(value->asString()); return; // LSTR
    case 8:  buffer.putLongLong(value->asInt64()); return;      // ABSTIME
    case 9:  buffer.putLongLong(value->asUint64()); return;     // DELTATIME
    case 10: value->asObjectId().encode(buffer); return;        // REF
    case 11: buffer.putOctet(value->asBool() ? 1 : 0); return;  // BOOL
    case 12: buffer.putFloat(value->asFloat()); return;         // FLOAT
    case 13: buffer.putDouble(value->asDouble()); return;       // DOUBLE
    case 14: value->asUuid().encode(buffer); return;            // UUID
    case 15: value->asMap().encode(buffer); return;             // MAP
    case 16: buffer.putOctet(value->asInt()); return;           // S8
    case 17: buffer.putShort(value->asInt()); return;           // S16
    case 18: buffer.putLong(value->asInt()); return;            // S32
    case 19: buffer.putLongLong(value->asInt64()); return;      // S64
    }
}
