#ifndef _QmfEngineQuery_
#define _QmfEngineQuery_

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

#include <qmf/engine/ObjectId.h>
#include <qmf/engine/Value.h>

namespace qmf {
namespace engine {

    class Object;
    struct QueryElementImpl;
    struct QueryImpl;
    struct QueryExpressionImpl;
    class  SchemaClassKey;

    enum ValueOper {
        O_EQ = 1,
        O_NE = 2,
        O_LT = 3,
        O_LE = 4,
        O_GT = 5,
        O_GE = 6,
        O_RE_MATCH = 7,
        O_RE_NOMATCH = 8,
        O_PRESENT = 9,
        O_NOT_PRESENT = 10
    };

    struct QueryOperand {
        virtual ~QueryOperand() {}
        virtual bool evaluate(const Object* object) const = 0;
    };

    struct QueryElement : public QueryOperand {
        QueryElement(const char* attrName, const Value* value, ValueOper oper);
        QueryElement(QueryElementImpl* impl);
        virtual ~QueryElement();
        bool evaluate(const Object* object) const;

        QueryElementImpl* impl;
    };

    enum ExprOper {
        E_NOT = 1,
        E_AND = 2,
        E_OR  = 3,
        E_XOR = 4
    };

    struct QueryExpression : public QueryOperand {
        QueryExpression(ExprOper oper, const QueryOperand* operand1, const QueryOperand* operand2);
        QueryExpression(QueryExpressionImpl* impl);
        virtual ~QueryExpression();
        bool evaluate(const Object* object) const;
        
        QueryExpressionImpl* impl;
    };

    class Query {
    public:
        Query(const char* className, const char* packageName);
        Query(const SchemaClassKey* key);
        Query(const ObjectId* oid);
        Query(const Query& from);
        ~Query();

        void setSelect(const QueryOperand* criterion);
        void setLimit(uint32_t maxResults);
        void setOrderBy(const char* attrName, bool decreasing);

        const char* getPackage() const;
        const char* getClass() const;
        const ObjectId* getObjectId() const;

        bool haveSelect() const;
        bool haveLimit() const;
        bool haveOrderBy() const;
        const QueryOperand* getSelect() const;
        uint32_t getLimit() const;
        const char* getOrderBy() const;
        bool getDecreasing() const;

    private:
        friend struct QueryImpl;
        friend class BrokerProxyImpl;
        Query(QueryImpl* impl);
        QueryImpl* impl;
    };
}
}

#endif

