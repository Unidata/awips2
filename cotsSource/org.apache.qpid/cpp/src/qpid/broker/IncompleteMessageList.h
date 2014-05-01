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
#ifndef _IncompleteMessageList_
#define _IncompleteMessageList_

#include "qpid/broker/BrokerImportExport.h"
#include "qpid/sys/Monitor.h"
#include "qpid/broker/Message.h"
#include <boost/intrusive_ptr.hpp>
#include <boost/function.hpp>
#include <list>

namespace qpid {
namespace broker {

class IncompleteMessageList
{
    typedef std::list< boost::intrusive_ptr<Message> > Messages;

    void enqueueComplete(const boost::intrusive_ptr<Message>&);

    sys::Monitor lock;
    Messages incomplete;
    Message::MessageCallback callback;

public:
    typedef Message::MessageCallback CompletionListener;    

    QPID_BROKER_EXTERN IncompleteMessageList();
    QPID_BROKER_EXTERN ~IncompleteMessageList();
    
    QPID_BROKER_EXTERN void add(boost::intrusive_ptr<Message> msg);
    QPID_BROKER_EXTERN void process(const CompletionListener& l, bool sync);
    void each(const CompletionListener& l);
};


}}

#endif
