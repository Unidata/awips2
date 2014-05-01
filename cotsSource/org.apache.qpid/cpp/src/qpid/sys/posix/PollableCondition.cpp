#ifndef QPID_SYS_LINUX_POLLABLECONDITION_CPP
#define QPID_SYS_LINUX_POLLABLECONDITION_CPP

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

#include "qpid/sys/PollableCondition.h"
#include "qpid/sys/DispatchHandle.h"
#include "qpid/sys/IOHandle.h"
#include "qpid/sys/posix/PrivatePosix.h"
#include "qpid/Exception.h"

#include <boost/bind.hpp>

#include <unistd.h>
#include <fcntl.h>

namespace qpid {
namespace sys {

class PollableConditionPrivate : public sys::IOHandle {
    friend class PollableCondition;

private:
    PollableConditionPrivate(const sys::PollableCondition::Callback& cb,
                             sys::PollableCondition& parent,
                             const boost::shared_ptr<sys::Poller>& poller);
    ~PollableConditionPrivate();

    void dispatch(sys::DispatchHandle& h);
    void set();
    void clear();

private:
    PollableCondition::Callback cb;
    PollableCondition& parent;
    boost::shared_ptr<sys::Poller> poller;
    int writeFd;
    std::auto_ptr<DispatchHandleRef> handle;
};

PollableConditionPrivate::PollableConditionPrivate(
    const sys::PollableCondition::Callback& cb,
    sys::PollableCondition& parent,
    const boost::shared_ptr<sys::Poller>& poller
) : IOHandle(new sys::IOHandlePrivate), cb(cb), parent(parent)
{
    int fds[2];
    if (::pipe(fds) == -1)
        throw ErrnoException(QPID_MSG("Can't create PollableCondition"));
    impl->fd = fds[0];
    writeFd = fds[1];
    if (::fcntl(impl->fd, F_SETFL, O_NONBLOCK) == -1)
        throw ErrnoException(QPID_MSG("Can't create PollableCondition"));
    if (::fcntl(writeFd, F_SETFL, O_NONBLOCK) == -1)
        throw ErrnoException(QPID_MSG("Can't create PollableCondition"));
    handle.reset (new DispatchHandleRef(
                      *this,
                      boost::bind(&sys::PollableConditionPrivate::dispatch, this, _1),
                      0, 0));
    handle->startWatch(poller);
    handle->unwatch();

    // Make the read FD readable
    static const char dummy=0;
    ssize_t n = ::write(writeFd, &dummy, 1);
    if (n == -1 && errno != EAGAIN)
        throw ErrnoException("Error setting PollableCondition");
}

PollableConditionPrivate::~PollableConditionPrivate() {
    handle->stopWatch();
    close(writeFd);
}

void PollableConditionPrivate::dispatch(sys::DispatchHandle&) {
    cb(parent);
}

void PollableConditionPrivate::set() {
    handle->rewatch();
}

void PollableConditionPrivate::clear() {
    handle->unwatch();
}


PollableCondition::PollableCondition(const Callback& cb,
                                     const boost::shared_ptr<sys::Poller>& poller
) : impl(new PollableConditionPrivate(cb, *this, poller))
{
}

PollableCondition::~PollableCondition()
{
    delete impl;
}

void PollableCondition::set() { impl->set(); }

void PollableCondition::clear() { impl->clear(); }

}} // namespace qpid::sys

#endif  /*!QPID_SYS_LINUX_POLLABLECONDITION_CPP*/
