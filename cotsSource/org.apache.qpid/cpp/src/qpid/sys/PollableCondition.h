#ifndef QPID_SYS_POLLABLECONDITION_H
#define QPID_SYS_POLLABLECONDITION_H

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

#include "qpid/sys/Poller.h"
#include "qpid/CommonImportExport.h"
#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>


namespace qpid {
namespace sys {

class PollableConditionPrivate;

class PollableCondition {
public:
    typedef boost::function1<void, PollableCondition&> Callback;

    QPID_COMMON_EXTERN PollableCondition(const Callback& cb,
                      const boost::shared_ptr<sys::Poller>& poller);

    QPID_COMMON_EXTERN ~PollableCondition();

    /**
     * Set the condition. Triggers callback to Callback from Poller.
     */
    QPID_COMMON_EXTERN void set();

    /**
     * Clear the condition. Stops callbacks from Poller.
     */
    QPID_COMMON_EXTERN void clear();

 private:
    PollableConditionPrivate *impl;

    Callback callback;
    boost::shared_ptr<sys::Poller> poller;
};

}} // namespace qpid::sys

#endif  /*!QPID_SYS_POLLABLECONDITION_H*/
