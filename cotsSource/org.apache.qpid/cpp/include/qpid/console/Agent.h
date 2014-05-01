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
#ifndef _QPID_CONSOLE_AGENT_H_
#define _QPID_CONSOLE_AGENT_H_

#include "qpid/console/Broker.h"
#include "qpid/console/ConsoleImportExport.h"

namespace qpid {
namespace console {

    /**
     *
     * \ingroup qmfconsoleapi
     */
    class QPID_CONSOLE_EXTERN Agent {
    public:
        typedef std::vector<Agent*> Vector;

        Agent(Broker* _broker, uint32_t _bank, const std::string& _label) :
            broker(_broker), brokerBank(broker->getBrokerBank()),
            agentBank(_bank), label(_label) {}
        Broker* getBroker() const { return broker; }
        uint32_t getBrokerBank() const { return brokerBank; }
        uint32_t getAgentBank() const { return agentBank; }
        const std::string& getLabel() const { return label; }

    private:
        Broker* broker;
        const uint32_t brokerBank;
        const uint32_t agentBank;
        const std::string label;
    };

    QPID_CONSOLE_EXTERN std::ostream& operator<<(std::ostream& o, const Agent& agent);
}
}


#endif
