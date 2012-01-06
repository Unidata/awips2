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
#include "qpid/framing/FieldTable.h"
#include "qpid/framing/Array.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/Endian.h"
#include "qpid/framing/FieldValue.h"
#include "qpid/Exception.h"
#include "qpid/framing/reply_exceptions.h"
#include <assert.h>

namespace qpid {
namespace framing {

FieldTable::FieldTable(const FieldTable& ft)
{
  *this = ft;
}

FieldTable& FieldTable::operator=(const FieldTable& ft)
{
  clear();
  values = ft.values;
  return *this;
}

FieldTable::~FieldTable() {}

uint32_t FieldTable::encodedSize() const {
    uint32_t len(4/*size field*/ + 4/*count field*/);
    for(ValueMap::const_iterator i = values.begin(); i != values.end(); ++i) {
        // shortstr_len_byte + key size + value size
	len += 1 + (i->first).size() + (i->second)->encodedSize();
    }
    return len;
}

int FieldTable::count() const {
    return values.size();
}

namespace 
{
std::ostream& operator<<(std::ostream& out, const FieldTable::ValueMap::value_type& i) {
    return out << i.first << ":" << *i.second;
}
}

std::ostream& operator<<(std::ostream& out, const FieldTable& t) {
    out << "{";
    FieldTable::ValueMap::const_iterator i = t.begin();
    if (i != t.end()) out << *i++;
    while (i != t.end()) 
    {
        out << "," << *i++;
    }
    return out << "}";
}

void FieldTable::set(const std::string& name, const ValuePtr& value){
    values[name] = value;
}

void FieldTable::setString(const std::string& name, const std::string& value){
    values[name] = ValuePtr(new Str16Value(value));
}

void FieldTable::setInt(const std::string& name, const int value){
    values[name] = ValuePtr(new IntegerValue(value));
}

void FieldTable::setInt64(const std::string& name, const int64_t value){
    values[name] = ValuePtr(new Integer64Value(value));
}

void FieldTable::setTimestamp(const std::string& name, const uint64_t value){
    values[name] = ValuePtr(new TimeValue(value));
}

void FieldTable::setUInt64(const std::string& name, const uint64_t value){
    values[name] = ValuePtr(new Unsigned64Value(value));
}

void FieldTable::setTable(const std::string& name, const FieldTable& value)
{
    values[name] = ValuePtr(new FieldTableValue(value));
}
void FieldTable::setArray(const std::string& name, const Array& value)
{
    values[name] = ValuePtr(new ArrayValue(value));
}

void FieldTable::setFloat(const std::string& name, const float value){
    values[name] = ValuePtr(new FloatValue(value));
}

void FieldTable::setDouble(const std::string& name, double value){
    values[name] = ValuePtr(new DoubleValue(value));
}

FieldTable::ValuePtr FieldTable::get(const std::string& name) const
{
    ValuePtr value;
    ValueMap::const_iterator i = values.find(name);
    if ( i!=values.end() )
        value = i->second;
    return value;
}

namespace {
    template <class T> T default_value() { return T(); }
    template <> int default_value<int>() { return 0; }
    template <> uint64_t default_value<uint64_t>() { return 0; }
}

template <class T>
T getValue(const FieldTable::ValuePtr value)
{
    if (!value || !value->convertsTo<T>())
        return default_value<T>();

    return value->get<T>();
}

std::string FieldTable::getAsString(const std::string& name) const {
    return getValue<std::string>(get(name));
}

int FieldTable::getAsInt(const std::string& name) const {
    return getValue<int>(get(name));
}

uint64_t FieldTable::getAsUInt64(const std::string& name) const {
    return static_cast<uint64_t>( getValue<int64_t>(get(name)));
}

int64_t FieldTable::getAsInt64(const std::string& name) const {
    return getValue<int64_t>(get(name));
}

bool FieldTable::getTable(const std::string& name, FieldTable& value) const {
    return getEncodedValue<FieldTable>(get(name), value);
}

bool FieldTable::getArray(const std::string& name, Array& value) const {
    return getEncodedValue<Array>(get(name), value);
}

template <class T, int width, uint8_t typecode>
bool getRawFixedWidthValue(FieldTable::ValuePtr vptr, T& value) 
{
    if (vptr && vptr->getType() == typecode) {
        value = vptr->get<T>();
        return true;
    }
    return false;
}

bool FieldTable::getFloat(const std::string& name, float& value) const {    
    return getRawFixedWidthValue<float, 4, 0x23>(get(name), value);
}

bool FieldTable::getDouble(const std::string& name, double& value) const {    
    return getRawFixedWidthValue<double, 8, 0x33>(get(name), value);
}

//uint64_t FieldTable::getTimestamp(const std::string& name) const {
//    return getValue<uint64_t>(name);
//}

void FieldTable::encode(Buffer& buffer) const {
    buffer.putLong(encodedSize() - 4);
    buffer.putLong(values.size());
    for (ValueMap::const_iterator i = values.begin(); i!=values.end(); ++i) {
        buffer.putShortString(i->first);
    	i->second->encode(buffer);
    }
}

void FieldTable::decode(Buffer& buffer){
    clear();
    uint32_t len = buffer.getLong();
    if (len) {
        uint32_t available = buffer.available();
        if (available < len)
            throw IllegalArgumentException(QPID_MSG("Not enough data for field table."));
        uint32_t count = buffer.getLong();
        uint32_t leftover = available - len;
        while(buffer.available() > leftover && count--){
            std::string name;
            ValuePtr value(new FieldValue);
            
            buffer.getShortString(name);
            value->decode(buffer);
            values[name] = ValuePtr(value);
        }    
    }
}

bool FieldTable::operator==(const FieldTable& x) const {
    if (values.size() != x.values.size()) return false;
    for (ValueMap::const_iterator i =  values.begin(); i != values.end(); ++i) {
        ValueMap::const_iterator j = x.values.find(i->first);
        if (j == x.values.end()) return false;
        if (*(i->second) != *(j->second)) return false;
    }
    return true;
}

void FieldTable::erase(const std::string& name) 
{
    if (values.find(name) != values.end()) 
        values.erase(name);
}

std::pair<FieldTable::ValueMap::iterator, bool> FieldTable::insert(const ValueMap::value_type& value)
{
    return values.insert(value);
}

FieldTable::ValueMap::iterator FieldTable::insert(ValueMap::iterator position, const ValueMap::value_type& value)
{
    return values.insert(position, value);
}


}
}
