#ifndef _framing_FieldValue_h
#define _framing_FieldValue_h
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

#include "qpid/Exception.h"
#include "qpid/framing/amqp_types.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/CommonImportExport.h"

#include <iostream>
#include <memory>
#include <vector>

#include <assert.h>

namespace qpid {
namespace framing {

/**
 * Exception that is the base exception for all field table errors.
 *
 * \ingroup clientapi
 */
class FieldValueException : public qpid::Exception {};

/**
 * Exception thrown when we can't perform requested conversion
 *
 * \ingroup clientapi
 */
struct InvalidConversionException : public FieldValueException {
    InvalidConversionException() {}
};

class List;

/**
 * Value that can appear in an AMQP field table
 *
 * \ingroup clientapi
 */
class FieldValue {
  public:
    /*
     * Abstract type for content of different types
     */
    class Data {
      public:
        virtual ~Data() {};
        virtual uint32_t encodedSize() const = 0;
        virtual void encode(Buffer& buffer) = 0;
        virtual void decode(Buffer& buffer) = 0;
        virtual bool operator==(const Data&) const = 0;

        virtual bool convertsToInt() const { return false; }
        virtual bool convertsToString() const { return false; }
        virtual int64_t getInt() const { throw InvalidConversionException();}
        virtual std::string getString() const { throw InvalidConversionException(); }

        virtual void print(std::ostream& out) const = 0;
    };

    FieldValue(): data(0) {};
    // Default assignment operator is fine
    void setType(uint8_t type);
    QPID_COMMON_EXTERN uint8_t getType();
    Data& getData() { return *data; }
    uint32_t encodedSize() const { return 1 + data->encodedSize(); };
    bool empty() const { return data.get() == 0; }
    void encode(Buffer& buffer);
    void decode(Buffer& buffer);
    QPID_COMMON_EXTERN bool operator==(const FieldValue&) const;
    QPID_COMMON_EXTERN bool operator!=(const FieldValue& v) const { return !(*this == v); }

    QPID_COMMON_EXTERN void print(std::ostream& out) const;

    template <typename T> bool convertsTo() const { return false; }
    template <typename T> T get() const { throw InvalidConversionException(); }

    template <class T, int W> T getIntegerValue() const;
    template <class T, int W> T getFloatingPointValue() const;
    template <class T> bool get(T&) const;

  protected:
    FieldValue(uint8_t t, Data* d): typeOctet(t), data(d) {}

    QPID_COMMON_EXTERN static uint8_t* convertIfRequired(uint8_t* const octets, int width);

  private:
    uint8_t typeOctet;
    std::auto_ptr<Data> data;

};

template <>
inline bool FieldValue::convertsTo<int>() const { return data->convertsToInt(); }

template <>
inline bool FieldValue::convertsTo<int64_t>() const { return data->convertsToInt(); }

template <>
inline bool FieldValue::convertsTo<std::string>() const { return data->convertsToString(); }

template <>
inline int FieldValue::get<int>() const { return data->getInt(); }

template <>
inline int64_t FieldValue::get<int64_t>() const { return data->getInt(); }

template <>
inline std::string FieldValue::get<std::string>() const { return data->getString(); }

inline std::ostream& operator<<(std::ostream& out, const FieldValue& v) {
    v.print(out);
    return out;
}

template <int width>
class FixedWidthValue : public FieldValue::Data {
    uint8_t octets[width];

  public:
    FixedWidthValue() {}
    FixedWidthValue(const uint8_t (&data)[width]) : octets(data) {}
    FixedWidthValue(const uint8_t* const data)
    {
        for (int i = 0; i < width; i++) octets[i] = data[i];
    }
    FixedWidthValue(uint64_t v)
    {
        for (int i = width; i > 1; --i) {
            octets[i-1] = (uint8_t) (0xFF & v); v >>= 8;
        }
        octets[0] = (uint8_t) (0xFF & v);
    }
    uint32_t encodedSize() const { return width; }
    void encode(Buffer& buffer) { buffer.putRawData(octets, width); }
    void decode(Buffer& buffer) { buffer.getRawData(octets, width); }
    bool operator==(const Data& d) const {
        const FixedWidthValue<width>* rhs = dynamic_cast< const FixedWidthValue<width>* >(&d);
        if (rhs == 0) return false;
        else return std::equal(&octets[0], &octets[width], &rhs->octets[0]);
    }

    bool convertsToInt() const { return true; }
    int64_t getInt() const
    {
        int64_t v = 0;
        for (int i = 0; i < width-1; ++i) {
            v |= octets[i]; v <<= 8;
        }
        v |= octets[width-1];
        return v;
    }
    uint8_t* rawOctets() { return octets; }
    uint8_t* rawOctets() const { return octets; }

    void print(std::ostream& o) const { o << "F" << width << ":"; };
};

template <class T, int W>
inline T FieldValue::getIntegerValue() const
{
    FixedWidthValue<W>* const fwv = dynamic_cast< FixedWidthValue<W>* const>(data.get());
    if (fwv) {
        uint8_t* octets = fwv->rawOctets();
        T v = 0;
        for (int i = 0; i < W-1; ++i) {
            v |= octets[i]; v <<= 8;
        }
        v |= octets[W-1];
        return v;
    } else {
        throw InvalidConversionException();
    }
}

template <class T, int W>
inline T FieldValue::getFloatingPointValue() const {
    FixedWidthValue<W>* const fwv = dynamic_cast< FixedWidthValue<W>* const>(data.get());
    if (fwv) {
        T value;
        uint8_t* const octets = convertIfRequired(fwv->rawOctets(), W);
        uint8_t* const target = reinterpret_cast<uint8_t*>(&value);
        for (uint i = 0; i < W; ++i) target[i] = octets[i];
        return value;
    } else {
        throw InvalidConversionException();
    }
}

template <>
inline float FieldValue::get<float>() const {
    return getFloatingPointValue<float, 4>();
}

template <>
inline double FieldValue::get<double>() const {
    return getFloatingPointValue<double, 8>();
}

template <>
class FixedWidthValue<0> : public FieldValue::Data {
  public:
    // Implicit default constructor is fine
    uint32_t encodedSize() const { return 0; }
    void encode(Buffer&) {};
    void decode(Buffer&) {};
    bool operator==(const Data& d) const {
        const FixedWidthValue<0>* rhs = dynamic_cast< const FixedWidthValue<0>* >(&d);
        return rhs != 0;
    }
    void print(std::ostream& o) const { o << "F0"; };
};

template <int lenwidth>
class VariableWidthValue : public FieldValue::Data {
    std::vector<uint8_t> octets;

  public:
    VariableWidthValue() {}
    VariableWidthValue(const std::vector<uint8_t>& data) : octets(data) {}
    VariableWidthValue(const uint8_t* start, const uint8_t* end) : octets(start, end) {}
    uint32_t encodedSize() const { return lenwidth + octets.size(); }
    void encode(Buffer& buffer) {
        buffer.putUInt<lenwidth>(octets.size());
        if (octets.size() > 0)
            buffer.putRawData(&octets[0], octets.size());
    };
    void decode(Buffer& buffer) {
        uint32_t len = buffer.getUInt<lenwidth>();
        octets.resize(len);
        if (len > 0)
            buffer.getRawData(&octets[0], len);
    }
    bool operator==(const Data& d) const {
        const VariableWidthValue<lenwidth>* rhs = dynamic_cast< const VariableWidthValue<lenwidth>* >(&d);
        if (rhs == 0) return false;
        else return octets==rhs->octets;
    }

    bool convertsToString() const { return true; }
    std::string getString() const { return std::string(octets.begin(), octets.end()); }

    void print(std::ostream& o) const { o << "V" << lenwidth << ":" << octets.size() << ":"; };
};

template <class T>
class EncodedValue : public FieldValue::Data {
    T value;
  public:

    EncodedValue() {}
    EncodedValue(const T& v) : value(v) {}

    T& getValue() { return value; }
    const T& getValue() const { return value; }

    uint32_t encodedSize() const { return value.encodedSize(); }

    void encode(Buffer& buffer) {
        value.encode(buffer);
    };
    void decode(Buffer& buffer) {
        value.decode(buffer);
    }
    bool operator==(const Data& d) const {
        const EncodedValue<T>* rhs = dynamic_cast< const EncodedValue<T>* >(&d);
        if (rhs == 0) return false;
        else return value==rhs->value;
    }

    void print(std::ostream& o) const { o << "[" << value << "]"; };
};

/**
 * Accessor that can be used to get values of type FieldTable, Array
 * and List.
 */
template <class T>
inline bool FieldValue::get(T& t) const
{
    const EncodedValue<T>* v = dynamic_cast< EncodedValue<T>* >(data.get());    
    if (v != 0) {
        t = v->getValue();
        return true;
    } else {
        try {
            t = get<T>();
            return true;
        } catch (const InvalidConversionException&) {
            return false;
        }
    }
}

class Str8Value : public FieldValue {
  public:
    QPID_COMMON_EXTERN Str8Value(const std::string& v);
};

class Str16Value : public FieldValue {
  public:
    QPID_COMMON_EXTERN Str16Value(const std::string& v);
};

class Struct32Value : public FieldValue {
  public:
    QPID_COMMON_EXTERN Struct32Value(const std::string& v);
};

class FloatValue : public FieldValue
{
  public:
    QPID_COMMON_EXTERN FloatValue(float f);
};
class DoubleValue : public FieldValue
{
  public:
    QPID_COMMON_EXTERN DoubleValue(double f);
};

/*
 * Basic integer value encodes as signed 32 bit
 */
class IntegerValue : public FieldValue {
  public:
    QPID_COMMON_EXTERN IntegerValue(int v);
};

class TimeValue : public FieldValue {
  public:
    QPID_COMMON_EXTERN TimeValue(uint64_t v);
};

class Integer64Value : public FieldValue {
  public:
    QPID_COMMON_EXTERN Integer64Value(int64_t v);
};

class Unsigned64Value : public FieldValue {
  public:
    QPID_COMMON_EXTERN Unsigned64Value(uint64_t v);
};

class FieldTableValue : public FieldValue {
  public:
    typedef FieldTable ValueType;
    QPID_COMMON_EXTERN FieldTableValue(const FieldTable&);
};

class ArrayValue : public FieldValue {
  public:
    QPID_COMMON_EXTERN ArrayValue(const Array&);
};

class VoidValue : public FieldValue {
  public:
    QPID_COMMON_EXTERN VoidValue();
};

class BoolValue : public FieldValue {
  public:
    QPID_COMMON_EXTERN BoolValue(bool);
};

class Unsigned8Value : public FieldValue {
  public:
    QPID_COMMON_EXTERN Unsigned8Value(uint8_t);
};

class Unsigned16Value : public FieldValue {
  public:
    QPID_COMMON_EXTERN Unsigned16Value(uint16_t);
};

class Unsigned32Value : public FieldValue {
  public:
    QPID_COMMON_EXTERN Unsigned32Value(uint32_t);
};

class Integer8Value : public FieldValue {
  public:
    QPID_COMMON_EXTERN Integer8Value(int8_t);
};

class Integer16Value : public FieldValue {
  public:
    QPID_COMMON_EXTERN Integer16Value(int16_t);
};

typedef IntegerValue Integer32Value;

class ListValue : public FieldValue {
  public:
    typedef List ValueType;
    QPID_COMMON_EXTERN ListValue(const List&);
};

template <class T>
bool getEncodedValue(FieldTable::ValuePtr vptr, T& value)
{
    if (vptr) {
        const EncodedValue<T>* ev = dynamic_cast< EncodedValue<T>* >(&(vptr->getData()));
        if (ev != 0) {
            value = ev->getValue();
            return true;
        }
    }
    return false;
}

}} // qpid::framing

#endif
