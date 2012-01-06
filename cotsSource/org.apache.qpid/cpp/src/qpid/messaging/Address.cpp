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
#include "qpid/messaging/Address.h"
#include "qpid/framing/Uuid.h"
#include <sstream>
#include <boost/format.hpp>

namespace qpid {
namespace messaging {

namespace {
const std::string SUBJECT_DIVIDER = "/";
const std::string OPTIONS_DIVIDER = ";";
const std::string SPACE = " ";
const std::string TYPE = "type";
}
class AddressImpl
{
  public:
    std::string name;
    std::string subject;
    Variant::Map options;
 
    AddressImpl() {}
    AddressImpl(const std::string& n, const std::string& s, const Variant::Map& o) :
        name(n), subject(s), options(o) {}
};

class AddressParser
{
  public:
    AddressParser(const std::string&);
    bool parse(Address& address);
  private:
    const std::string& input;
    std::string::size_type current;
    static const std::string RESERVED;

    bool readChar(char c);
    bool readQuotedString(std::string& s);
    bool readQuotedValue(Variant& value);
    bool readString(std::string& value, char delimiter);
    bool readWord(std::string& word, const std::string& delims = RESERVED);
    bool readSimpleValue(Variant& word);
    bool readKey(std::string& key);
    bool readValue(Variant& value);
    bool readKeyValuePair(Variant::Map& map);
    bool readMap(Variant& value);
    bool readList(Variant& value);
    bool readName(std::string& name);
    bool readSubject(std::string& subject);
    bool error(const std::string& message);
    bool eos();
    bool iswhitespace();
    bool in(const std::string& delims);
    bool isreserved();
};

Address::Address() : impl(new AddressImpl()) {}
Address::Address(const std::string& address) : impl(new AddressImpl())
{ 
    AddressParser parser(address);
    parser.parse(*this);
}
Address::Address(const std::string& name, const std::string& subject, const Variant::Map& options,
                 const std::string& type)
    : impl(new AddressImpl(name, subject, options)) { setType(type); }
Address::Address(const Address& a) :
    impl(new AddressImpl(a.impl->name, a.impl->subject, a.impl->options)) {}
Address::~Address() { delete impl; }

Address& Address::operator=(const Address& a) { *impl = *a.impl; return *this; }


std::string Address::toStr() const
{
    std::stringstream out;
    out << impl->name;
    if (!impl->subject.empty()) out << SUBJECT_DIVIDER << impl->subject;
    if (!impl->options.empty()) out << OPTIONS_DIVIDER << impl->options;
    return out.str();
}
Address::operator bool() const { return !impl->name.empty(); }
bool Address::operator !() const { return impl->name.empty(); }

const std::string& Address::getName() const { return impl->name; }
void Address::setName(const std::string& name) { impl->name = name; }
const std::string& Address::getSubject() const { return impl->subject; }
bool Address::hasSubject() const { return !(impl->subject.empty()); }
void Address::setSubject(const std::string& subject) { impl->subject = subject; }
const Variant::Map& Address::getOptions() const { return impl->options; }
Variant::Map& Address::getOptions() { return impl->options; }
void Address::setOptions(const Variant::Map& options) { impl->options = options; }


namespace{
const Variant EMPTY_VARIANT;
const std::string EMPTY_STRING;
const std::string NODE_PROPERTIES="node-properties";
}

const Variant& find(const Variant::Map& map, const std::string& key)
{
    Variant::Map::const_iterator i = map.find(key);
    if (i == map.end()) return EMPTY_VARIANT;
    else return i->second;
}

std::string Address::getType() const
{
    const Variant& props = getOption(NODE_PROPERTIES);
    if (props.getType() == VAR_MAP) {
        const Variant& type = find(props.asMap(), TYPE);
        if (!type.isVoid()) return type.asString();
    }
    return EMPTY_STRING;
}

void Address::setType(const std::string& type)
{ 
    Variant& props = impl->options[NODE_PROPERTIES];
    if (props.isVoid()) props = Variant::Map();
    props.asMap()[TYPE] = type;
}

const Variant& Address::getOption(const std::string& key) const
{
    return find(impl->options, key);
}

std::ostream& operator<<(std::ostream& out, const Address& address)
{
    out << address.toStr();
    return out;
}

InvalidAddress::InvalidAddress(const std::string& msg) : Exception(msg) {}

MalformedAddress::MalformedAddress(const std::string& msg) : Exception(msg) {}

AddressParser::AddressParser(const std::string& s) : input(s), current(0) {}

bool AddressParser::error(const std::string& message)
{
    throw MalformedAddress((boost::format("%1%, character %2% of %3%") % message % current % input).str());
}

bool AddressParser::parse(Address& address)
{
    std::string name;
    if (readName(name)) {
        if (name.find('#') == 0) name = qpid::framing::Uuid(true).str() + name;
        address.setName(name);
        if (readChar('/')) {
            std::string subject;
            if (readSubject(subject)) {
                address.setSubject(subject);
            } else {
                return error("Expected subject after /");
            }
        }
        if (readChar(';')) {
            Variant options = Variant::Map();
            if (readMap(options)) {
                address.setOptions(options.asMap());
            }
        }
        //skip trailing whitespace
        while (!eos() && iswhitespace()) ++current;
        return eos() || error("Unexpected chars in address: " + input.substr(current));
    } else {
        return input.empty() || error("Expected name");
    }
}

bool AddressParser::readList(Variant& value)
{
    if (readChar('[')) {
        value = Variant::List();
        Variant item;
        while (readValue(item)) {
            value.asList().push_back(item);
            if (!readChar(',')) break;
        }
        return readChar(']') || error("Unmatched '['!");
    } else {
        return false;
    }
}

bool AddressParser::readMap(Variant& value)
{
    if (readChar('{')) {
        value = Variant::Map();
        while (readKeyValuePair(value.asMap()) && readChar(',')) {}
        return readChar('}') || error("Unmatched '{'!");
    } else {
        return false;
    }
}

bool AddressParser::readKeyValuePair(Variant::Map& map)
{
    std::string key;
    Variant value;
    if (readKey(key)) {
        if (readChar(':') && readValue(value)) {
            map[key] = value;
            return true;
        } else {
            return error("Bad key-value pair, expected ':'");
        }
    } else {
        return false;
    }
}

bool AddressParser::readKey(std::string& key)
{
    return readWord(key);
}

bool AddressParser::readValue(Variant& value)
{
    return readSimpleValue(value) || readQuotedValue(value) || 
        readMap(value)  || readList(value) || error("Expected value");
}

bool AddressParser::readString(std::string& value, char delimiter)
{
    if (readChar(delimiter)) {
        std::string::size_type start = current++;
        while (!eos()) {
            if (input.at(current) == delimiter) {
                if (current > start) {
                    value = input.substr(start, current - start);
                } else {
                    value = "";
                }
                ++current;
                return true;
            } else {
                ++current;
            }
        }
        return error("Unmatched delimiter");
    } else {
        return false;
    }
}

bool AddressParser::readName(std::string& name)
{
    return readQuotedString(name) || readWord(name, "/;");
}

bool AddressParser::readSubject(std::string& subject)
{
    return readQuotedString(subject) || readWord(subject, ";");
}

bool AddressParser::readQuotedString(std::string& s)
{
    return readString(s, '"') || readString(s, '\'');
}

bool AddressParser::readQuotedValue(Variant& value)
{
    std::string s;
    if (readQuotedString(s)) {
        value = s;
        return true;
    } else {
        return false;
    }
}

bool AddressParser::readSimpleValue(Variant& value)
{
    std::string s;
    if (readWord(s)) {
        value = s;
        try { value = value.asInt64(); return true; } catch (const InvalidConversion&) {}
        try { value = value.asDouble(); return true; } catch (const InvalidConversion&) {}
        return true;
    } else {
        return false;
    }
}

bool AddressParser::readWord(std::string& value, const std::string& delims)
{
    //skip leading whitespace
    while (!eos() && iswhitespace()) ++current;

    //read any number of non-whitespace, non-reserved chars into value
    std::string::size_type start = current;
    while (!eos() && !iswhitespace() && !in(delims)) ++current;
    
    if (current > start) {
        value = input.substr(start, current - start);
        return true;
    } else {
        return false;
    }
}

bool AddressParser::readChar(char c)
{
    while (!eos()) {
        if (iswhitespace()) { 
            ++current; 
        } else if (input.at(current) == c) {
            ++current;
            return true;
        } else {
            return false;
        }
    }
    return false;
}

bool AddressParser::iswhitespace()
{
    return ::isspace(input.at(current));
}

bool AddressParser::isreserved()
{
    return in(RESERVED);
}

bool AddressParser::in(const std::string& chars)
{
    return chars.find(input.at(current)) != std::string::npos;
}

bool AddressParser::eos()
{
    return current >= input.size();
}

const std::string AddressParser::RESERVED = "\'\"{}[],:/";
}} // namespace qpid::messaging
