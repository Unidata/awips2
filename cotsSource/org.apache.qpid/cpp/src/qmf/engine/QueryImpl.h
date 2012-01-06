#ifndef _QmfEngineQueryImpl_
#define _QmfEngineQueryImpl_

/*
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
 */

#include "qmf/engine/Query.h"
#include "qmf/engine/Schema.h"
#include <string>
#include <boost/shared_ptr.hpp>

namespace qpid {
    namespace framing {
        class Buffer;
    }
}

namespace qmf {
namespace engine {

    struct QueryElementImpl {
        QueryElementImpl(const std::string& a, const Value* v, ValueOper o) : attrName(a), value(v), oper(o) {}
        ~QueryElementImpl() {}
        bool evaluate(const Object* object) const;

        std::string attrName;
        const Value* value;
        ValueOper oper;
    };

    struct QueryExpressionImpl {
        QueryExpressionImpl(ExprOper o, const QueryOperand* operand1, const QueryOperand* operand2) : oper(o), left(operand1), right(operand2) {}
        ~QueryExpressionImpl() {}
        bool evaluate(const Object* object) const;

        ExprOper oper;
        const QueryOperand* left;
        const QueryOperand* right;
    };

    struct QueryImpl {
        // Constructors mapped to public
        QueryImpl(const std::string& c, const std::string& p) : packageName(p), className(c), select(0), resultLimit(0) {}
        QueryImpl(const SchemaClassKey* key) : packageName(key->getPackageName()), className(key->getClassName()), select(0), resultLimit(0) {}
        QueryImpl(const ObjectId* oid) : oid(new ObjectId(*oid)), select(0), resultLimit(0) {}

        // Factory constructors
        QueryImpl(qpid::framing::Buffer& buffer);

        ~QueryImpl() {};
        static Query* factory(qpid::framing::Buffer& buffer);

        void setSelect(const QueryOperand* criterion) { select = criterion; }
        void setLimit(uint32_t maxResults) { resultLimit = maxResults; }
        void setOrderBy(const std::string& attrName, bool decreasing) {
            orderBy = attrName; orderDecreasing = decreasing;
        }

        const std::string& getPackage() const { return packageName; }
        const std::string&  getClass() const { return className; }
        const ObjectId* getObjectId() const { return oid.get(); }

        bool haveSelect() const { return select != 0; }
        bool haveLimit() const { return resultLimit > 0; }
        bool haveOrderBy() const { return !orderBy.empty(); }
        const QueryOperand* getSelect() const { return select; }
        uint32_t getLimit() const { return resultLimit; }
        const std::string& getOrderBy() const { return orderBy; }
        bool getDecreasing() const { return orderDecreasing; }

        void encode(qpid::framing::Buffer& buffer) const;
        bool singleAgent() const { return oid.get() != 0; }
        uint32_t agentBank() const { return singleAgent() ? oid->getAgentBank() : 0; }

        std::string packageName;
        std::string className;
        boost::shared_ptr<ObjectId> oid;
        const QueryOperand* select;
        uint32_t resultLimit;
        std::string orderBy;
        bool orderDecreasing;
    };
}
}

#endif
