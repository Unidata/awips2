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

#include "qmf/engine/QueryImpl.h"
#include "qmf/engine/ObjectIdImpl.h"
#include "qpid/framing/Buffer.h"
#include "qpid/framing/FieldTable.h"

using namespace std;
using namespace qmf::engine;
using namespace qpid::framing;

bool QueryElementImpl::evaluate(const Object* /*object*/) const
{
    // TODO: Implement this
    return false;
}

bool QueryExpressionImpl::evaluate(const Object* /*object*/) const
{
    // TODO: Implement this
    return false;
}

QueryImpl::QueryImpl(Buffer& buffer)
{
    FieldTable ft;
    ft.decode(buffer);
    // TODO
}

Query* QueryImpl::factory(Buffer& buffer)
{
    QueryImpl* impl(new QueryImpl(buffer));
    return new Query(impl);
}

void QueryImpl::encode(Buffer& buffer) const
{
    FieldTable ft;

    if (oid.get() != 0) {
        ft.setString("_objectid", oid->impl->asString());
    } else {
        if (!packageName.empty())
            ft.setString("_package", packageName);
        ft.setString("_class", className);
    }

    ft.encode(buffer);
}


//==================================================================
// Wrappers
//==================================================================

QueryElement::QueryElement(const char* attrName, const Value* value, ValueOper oper) : impl(new QueryElementImpl(attrName, value, oper)) {}
QueryElement::QueryElement(QueryElementImpl* i) : impl(i) {}
QueryElement::~QueryElement() { delete impl; }
bool QueryElement::evaluate(const Object* object) const { return impl->evaluate(object); }

QueryExpression::QueryExpression(ExprOper oper, const QueryOperand* operand1, const QueryOperand* operand2) : impl(new QueryExpressionImpl(oper, operand1, operand2)) {}
QueryExpression::QueryExpression(QueryExpressionImpl* i) : impl(i) {}
QueryExpression::~QueryExpression() { delete impl; }
bool QueryExpression::evaluate(const Object* object) const { return impl->evaluate(object); }

Query::Query(const char* className, const char* packageName) : impl(new QueryImpl(className, packageName)) {}
Query::Query(const SchemaClassKey* key) : impl(new QueryImpl(key)) {}
Query::Query(const ObjectId* oid) : impl(new QueryImpl(oid)) {}
Query::Query(QueryImpl* i) : impl(i) {}
Query::Query(const Query& from) : impl(new QueryImpl(*(from.impl))) {}
Query::~Query() { delete impl; }
void Query::setSelect(const QueryOperand* criterion) { impl->setSelect(criterion); }
void Query::setLimit(uint32_t maxResults) { impl->setLimit(maxResults); }
void Query::setOrderBy(const char* attrName, bool decreasing) { impl->setOrderBy(attrName, decreasing); }
const char* Query::getPackage() const { return impl->getPackage().c_str(); }
const char* Query::getClass() const { return impl->getClass().c_str(); }
const ObjectId* Query::getObjectId() const { return impl->getObjectId(); }
bool Query::haveSelect() const { return impl->haveSelect(); }
bool Query::haveLimit() const { return impl->haveLimit(); }
bool Query::haveOrderBy() const { return impl->haveOrderBy(); }
const QueryOperand* Query::getSelect() const { return impl->getSelect(); }
uint32_t Query::getLimit() const { return impl->getLimit(); }
const char* Query::getOrderBy() const { return impl->getOrderBy().c_str(); }
bool Query::getDecreasing() const { return impl->getDecreasing(); }

