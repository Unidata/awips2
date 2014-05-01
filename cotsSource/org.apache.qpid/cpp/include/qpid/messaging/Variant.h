#ifndef QPID_MESSAGING_VARIANT_H
#define QPID_MESSAGING_VARIANT_H

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
#include <list>
#include <map>
#include <ostream>
#include <string>
#include "qpid/Exception.h"
#include "qpid/sys/IntegerTypes.h"
#include "qpid/client/ClientImportExport.h"

namespace qpid {
namespace messaging {

/**
 * Thrown when an illegal conversion of a variant is attempted.
 */
struct InvalidConversion : public qpid::Exception 
{
    InvalidConversion(const std::string& msg);
};

enum VariantType {
    VAR_VOID = 0,
    VAR_BOOL,
    VAR_UINT8,
    VAR_UINT16,
    VAR_UINT32,
    VAR_UINT64,
    VAR_INT8,
    VAR_INT16,
    VAR_INT32,
    VAR_INT64,
    VAR_FLOAT,
    VAR_DOUBLE,
    VAR_STRING,
    VAR_MAP,
    VAR_LIST
};

class VariantImpl;

/**
 * Represents a value of variable type.
 */
class Variant
{
  public:
    typedef std::map<std::string, Variant> Map;
    typedef std::list<Variant> List;

    QPID_CLIENT_EXTERN Variant();
    QPID_CLIENT_EXTERN Variant(bool);
    QPID_CLIENT_EXTERN Variant(uint8_t);
    QPID_CLIENT_EXTERN Variant(uint16_t);
    QPID_CLIENT_EXTERN Variant(uint32_t);
    QPID_CLIENT_EXTERN Variant(uint64_t);
    QPID_CLIENT_EXTERN Variant(int8_t);
    QPID_CLIENT_EXTERN Variant(int16_t);
    QPID_CLIENT_EXTERN Variant(int32_t);
    QPID_CLIENT_EXTERN Variant(int64_t);
    QPID_CLIENT_EXTERN Variant(float);
    QPID_CLIENT_EXTERN Variant(double);
    QPID_CLIENT_EXTERN Variant(const std::string&);
    QPID_CLIENT_EXTERN Variant(const char*);
    QPID_CLIENT_EXTERN Variant(const Map&);
    QPID_CLIENT_EXTERN Variant(const List&);
    QPID_CLIENT_EXTERN Variant(const Variant&);

    QPID_CLIENT_EXTERN ~Variant();

    QPID_CLIENT_EXTERN VariantType getType() const;
    QPID_CLIENT_EXTERN bool isVoid() const;
    
    QPID_CLIENT_EXTERN Variant& operator=(bool);
    QPID_CLIENT_EXTERN Variant& operator=(uint8_t);
    QPID_CLIENT_EXTERN Variant& operator=(uint16_t);
    QPID_CLIENT_EXTERN Variant& operator=(uint32_t);
    QPID_CLIENT_EXTERN Variant& operator=(uint64_t);
    QPID_CLIENT_EXTERN Variant& operator=(int8_t);
    QPID_CLIENT_EXTERN Variant& operator=(int16_t);
    QPID_CLIENT_EXTERN Variant& operator=(int32_t);
    QPID_CLIENT_EXTERN Variant& operator=(int64_t);
    QPID_CLIENT_EXTERN Variant& operator=(float);
    QPID_CLIENT_EXTERN Variant& operator=(double);
    QPID_CLIENT_EXTERN Variant& operator=(const std::string&);
    QPID_CLIENT_EXTERN Variant& operator=(const char*);
    QPID_CLIENT_EXTERN Variant& operator=(const Map&);
    QPID_CLIENT_EXTERN Variant& operator=(const List&);
    QPID_CLIENT_EXTERN Variant& operator=(const Variant&);

    QPID_CLIENT_EXTERN bool asBool() const;
    QPID_CLIENT_EXTERN uint8_t asUint8() const;
    QPID_CLIENT_EXTERN uint16_t asUint16() const;
    QPID_CLIENT_EXTERN uint32_t asUint32() const;
    QPID_CLIENT_EXTERN uint64_t asUint64() const;
    QPID_CLIENT_EXTERN int8_t asInt8() const;
    QPID_CLIENT_EXTERN int16_t asInt16() const;
    QPID_CLIENT_EXTERN int32_t asInt32() const;
    QPID_CLIENT_EXTERN int64_t asInt64() const;
    QPID_CLIENT_EXTERN float asFloat() const;
    QPID_CLIENT_EXTERN double asDouble() const;
    QPID_CLIENT_EXTERN std::string asString() const;

    QPID_CLIENT_EXTERN operator bool() const;
    QPID_CLIENT_EXTERN operator uint8_t() const;
    QPID_CLIENT_EXTERN operator uint16_t() const;
    QPID_CLIENT_EXTERN operator uint32_t() const;
    QPID_CLIENT_EXTERN operator uint64_t() const;
    QPID_CLIENT_EXTERN operator int8_t() const;
    QPID_CLIENT_EXTERN operator int16_t() const;
    QPID_CLIENT_EXTERN operator int32_t() const;
    QPID_CLIENT_EXTERN operator int64_t() const;
    QPID_CLIENT_EXTERN operator float() const;
    QPID_CLIENT_EXTERN operator double() const;
    QPID_CLIENT_EXTERN operator const char*() const;

    QPID_CLIENT_EXTERN const Map& asMap() const;
    QPID_CLIENT_EXTERN Map& asMap();
    QPID_CLIENT_EXTERN const List& asList() const;
    QPID_CLIENT_EXTERN List& asList();
    /**
     * Unlike asString(), getString() will not do any conversions and
     * will throw InvalidConversion if the type is not STRING.
     */
    QPID_CLIENT_EXTERN const std::string& getString() const;
    QPID_CLIENT_EXTERN std::string& getString();

    QPID_CLIENT_EXTERN void setEncoding(const std::string&);
    QPID_CLIENT_EXTERN const std::string& getEncoding() const;

    QPID_CLIENT_EXTERN bool isEqualTo(const Variant& a) const;

    QPID_CLIENT_EXTERN void reset();    
  private:
    VariantImpl* impl;
};

QPID_CLIENT_EXTERN std::ostream& operator<<(std::ostream& out, const Variant& value);
QPID_CLIENT_EXTERN std::ostream& operator<<(std::ostream& out, const Variant::Map& map);
QPID_CLIENT_EXTERN std::ostream& operator<<(std::ostream& out, const Variant::List& list);
QPID_CLIENT_EXTERN bool operator==(const Variant& a, const Variant& b);

typedef Variant::Map VariantMap;

}} // namespace qpid::messaging

#endif  /*!QPID_MESSAGING_VARIANT_H*/
