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
#include <boost/function.hpp>
#include <boost/shared_ptr.hpp>
#include "qpid/framing/FrameSet.h"
#include "qpid/sys/Mutex.h"
#include "qpid/sys/BlockingQueue.h"
#include "qpid/client/ClientImportExport.h"

#ifndef _Demux_
#define _Demux_

namespace qpid {
namespace client {

///@internal
class ByTransferDest
{
    const std::string dest;
public:
    ByTransferDest(const std::string& dest);
    bool operator()(const framing::FrameSet& frameset) const;
};

///@internal
class Demux
{
public:
    typedef boost::function<bool(const framing::FrameSet&)> Condition;
    typedef sys::BlockingQueue<framing::FrameSet::shared_ptr> Queue;
    typedef boost::shared_ptr<Queue> QueuePtr;

    QPID_CLIENT_EXTERN Demux();
    QPID_CLIENT_EXTERN ~Demux();
    
    QPID_CLIENT_EXTERN void handle(framing::FrameSet::shared_ptr);
    QPID_CLIENT_EXTERN void close(const sys::ExceptionHolder& ex);
    QPID_CLIENT_EXTERN void open();

    QPID_CLIENT_EXTERN QueuePtr add(const std::string& name, Condition);
    QPID_CLIENT_EXTERN void remove(const std::string& name);
    QPID_CLIENT_EXTERN QueuePtr get(const std::string& name);
    QPID_CLIENT_EXTERN QueuePtr getDefault();

private:
    struct Record
    {
        const std::string name;
        Condition condition;
        QueuePtr queue;

        Record(const std::string& n, Condition c) : name(n), condition(c), queue(new Queue()) {}
    };

    sys::Mutex lock;
    std::list<Record> records;
    QueuePtr defaultQueue;

    typedef std::list<Record>::iterator iterator;

    struct Find
    {
        const std::string name;
        Find(const std::string& name);
        bool operator()(const Record& record) const;
    };
};

class ScopedDivert
{
    const std::string dest;
    Demux& demuxer;
    Demux::QueuePtr queue;
public:
    ScopedDivert(const std::string& dest, Demux& demuxer);
    ~ScopedDivert();
    Demux::QueuePtr getQueue();
};

}} // namespace qpid::client


#endif
