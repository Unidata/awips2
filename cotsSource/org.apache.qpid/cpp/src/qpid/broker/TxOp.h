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
#ifndef _TxOp_
#define _TxOp_

#include "qpid/broker/TxOpVisitor.h"
#include "qpid/broker/TransactionalStore.h"
#include <boost/shared_ptr.hpp>

namespace qpid {
    namespace broker {

        class TxOp{
        public:
            typedef boost::shared_ptr<TxOp> shared_ptr;

            virtual bool prepare(TransactionContext*) throw() = 0;
            virtual void commit()  throw() = 0;
            virtual void rollback()  throw() = 0;
            virtual ~TxOp(){}

            virtual void accept(TxOpConstVisitor&) const = 0;
        };

}} // namespace qpid::broker


#endif
