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

#include "qpid/client/QueueOptions.h"

namespace qpid {
namespace client {

enum QueueEventGeneration {ENQUEUE_ONLY=1, ENQUEUE_AND_DEQUEUE=2};


QueueOptions::QueueOptions()
{}

const std::string QueueOptions::strMaxCountKey("qpid.max_count");
const std::string QueueOptions::strMaxSizeKey("qpid.max_size");
const std::string QueueOptions::strTypeKey("qpid.policy_type");
const std::string QueueOptions::strREJECT("reject");
const std::string QueueOptions::strFLOW_TO_DISK("flow_to_disk");
const std::string QueueOptions::strRING("ring");
const std::string QueueOptions::strRING_STRICT("ring_strict");
const std::string QueueOptions::strLastValueQueue("qpid.last_value_queue");
const std::string QueueOptions::strPersistLastNode("qpid.persist_last_node");
const std::string QueueOptions::strLVQMatchProperty("qpid.LVQ_key");
const std::string QueueOptions::strLastValueQueueNoBrowse("qpid.last_value_queue_no_browse");
const std::string QueueOptions::strQueueEventMode("qpid.queue_event_generation");


QueueOptions::~QueueOptions()
{}
	
void QueueOptions::setSizePolicy(QueueSizePolicy sp, uint64_t maxSize, uint32_t maxCount)
{
    if (maxCount) setInt(strMaxCountKey, maxCount);
    if (maxSize) setInt(strMaxSizeKey, maxSize);
    if (maxSize || maxCount){
        switch (sp)
        {
          case REJECT:
            setString(strTypeKey, strREJECT);
            break;
          case FLOW_TO_DISK:
            setString(strTypeKey, strFLOW_TO_DISK);
            break;
          case RING:
            setString(strTypeKey, strRING);
            break;
          case RING_STRICT:
            setString(strTypeKey, strRING_STRICT);
            break;
          case NONE:
            clearSizePolicy();
            break;
        }
    }
}


void QueueOptions::setPersistLastNode()
{
    setInt(strPersistLastNode, 1);
}

void QueueOptions::setOrdering(QueueOrderingPolicy op)
{
    if (op == LVQ){
        setInt(strLastValueQueue, 1); 
    }else if (op == LVQ_NO_BROWSE){
        setInt(strLastValueQueueNoBrowse, 1); 
    }else {
        clearOrdering();
    }
}

void QueueOptions::getLVQKey(std::string& key)
{
    key.assign(strLVQMatchProperty);
}

void QueueOptions::clearSizePolicy()
{
    erase(strMaxCountKey);
    erase(strMaxSizeKey);
    erase(strTypeKey);
}

void QueueOptions::clearPersistLastNode()
{
    erase(strPersistLastNode);
}

void QueueOptions::clearOrdering()
{
    erase(strLastValueQueue);
}

void QueueOptions::enableQueueEvents(bool enqueueOnly)
{
    setInt(strQueueEventMode, enqueueOnly ? ENQUEUE_ONLY : ENQUEUE_AND_DEQUEUE);
}

}
}


