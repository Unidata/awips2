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
#ifndef _QPID_CONSOLE_VALUE_H_
#define _QPID_CONSOLE_VALUE_H_

#include "qpid/Exception.h"
#include "qpid/framing/Uuid.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/console/ObjectId.h"
#include <boost/shared_ptr.hpp>

namespace qpid {
namespace framing {
    class Buffer;
}
namespace console {

    /**
     * \ingroup qmfconsoleapi
     */
    class Value {

    public:
        typedef boost::shared_ptr<Value> Ptr;
        virtual ~Value() {}
        virtual std::string str() const = 0;

        virtual bool isNull() const { return false; }
        virtual bool isObjectId() const { return false; }
        virtual bool isUint() const { return false; }
        virtual bool isInt() const { return false; }
        virtual bool isUint64() const { return false; }
        virtual bool isInt64() const { return false; }
        virtual bool isString() const { return false; }
        virtual bool isBool() const { return false; }
        virtual bool isFloat() const { return false; }
        virtual bool isDouble() const { return false; }
        virtual bool isUuid() const { return false; }
        virtual bool isMap() const { return false; }

        virtual ObjectId asObjectId() const { incompatible(); return ObjectId(); }
        virtual uint32_t asUint() const { incompatible(); return 0; }
        virtual int32_t asInt() const { incompatible(); return 0; }
        virtual uint64_t asUint64() const { incompatible(); return 0; }
        virtual int64_t asInt64() const { incompatible(); return 0; }
        virtual std::string asString() const { incompatible(); return std::string(); }
        virtual bool asBool() const { incompatible(); return false; }
        virtual float asFloat() const { incompatible(); return 0.0; }
        virtual double asDouble() const { incompatible(); return 0.0; }
        virtual framing::Uuid asUuid() const { incompatible(); return framing::Uuid(); }
        virtual framing::FieldTable asMap() const { incompatible(); return framing::FieldTable(); }

    private:
        void incompatible() const {
            throw Exception("Incompatible Type");
        }
    };

    class NullValue : public Value {
    public:
        NullValue() {}
        std::string str() const;
        bool isNull() const { return true; }
    };

    class RefValue : public Value {
    public:
        RefValue(ObjectId v) : value(v) {}
        RefValue(framing::Buffer& buffer);
        std::string str() const;
        bool isObjectId() const { return true; }
        ObjectId asObjectId() const { return value; }
    private:
        ObjectId value;
    };

    class UintValue : public Value {
    public:
        UintValue(uint32_t v) : value(v) {}
        std::string str() const;
        bool isUint() const { return true; }
        uint32_t asUint() const { return value; }
        bool isUint64() const { return true; }
        uint64_t asUint64() const { return (uint64_t) value; }
    private:
        uint32_t value;
    };

    class IntValue : public Value {
    public:
        IntValue(int32_t v) : value(v) {}
        std::string str() const;
        bool isInt() const { return true; }
        int32_t asInt() const { return value; }
        bool isInt64() const { return true; }
        int64_t asInt64() const { return (int64_t) value; }
    private:
        int32_t value;
    };

    class Uint64Value : public Value {
    public:
        Uint64Value(uint64_t v) : value(v) {}
        std::string str() const;
        bool isUint64() const { return true; }
        uint64_t asUint64() const { return value; }
    private:
        uint64_t value;
    };

    class Int64Value : public Value {
    public:
        Int64Value(int64_t v) : value(v) {}
        std::string str() const;
        bool isInt64() const { return true; }
        int64_t asInt64() const { return value; }
    private:
        int64_t value;
    };

    class StringValue : public Value {
    public:
        StringValue(const std::string& v) : value(v) {}
        StringValue(framing::Buffer& buffer, int tc);
        std::string str() const { return value; }
        bool isString() const { return true; }
        std::string asString() const { return value; }
    private:
        std::string value;
    };

    class BoolValue : public Value {
    public:
        BoolValue(bool v) : value(v) {}
        BoolValue(uint8_t v) : value(v != 0) {}
        std::string str() const;
        bool isBool() const { return true; }
        bool asBool() const { return value; }
    private:
        bool value;
    };

    class FloatValue : public Value {
    public:
        FloatValue(float v) : value(v) {}
        std::string str() const;
        bool isFloat() const { return true; }
        float asFloat() const { return value; }
        bool isDouble() const { return true; }
        double asDouble() const { return (double) value; }
    private:
        float value;
    };

    class DoubleValue : public Value {
    public:
        DoubleValue(double v) : value(v) {}
        std::string str() const;
        bool isDouble() const { return true; }
        double asDouble() const { return value; }
    private:
        double value;
    };

    class UuidValue : public Value {
    public:
        UuidValue(const framing::Uuid& v) : value(v) {}
        UuidValue(framing::Buffer& buffer);
        std::string str() const { return value.str(); }
        bool isUuid() const { return true; }
        framing::Uuid asUuid() const { return value; }
    private:
        framing::Uuid value;
    };

    class MapValue : public Value {
    public:
        MapValue(const framing::FieldTable& v) : value(v) {}
        MapValue(framing::Buffer& buffer);
        std::string str() const;
        bool isMap() const { return true; }
        framing::FieldTable asMap() const { return value; }
    private:
        framing::FieldTable value;
    };

    class ValueFactory {
    public:
        static Value::Ptr newValue(int typeCode, framing::Buffer& buffer);
        static void encodeValue(int typeCode, Value::Ptr value, framing::Buffer& buffer);
    };
}
}

#endif
