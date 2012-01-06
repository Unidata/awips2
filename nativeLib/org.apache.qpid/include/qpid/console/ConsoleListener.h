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
#ifndef _QPID_CONSOLE_CONSOLE_LISTENER_H_
#define _QPID_CONSOLE_CONSOLE_LISTENER_H_

#include <string>
#include "qpid/console/ConsoleImportExport.h"
#include "qpid/console/Broker.h"
#include "qpid/console/ClassKey.h"
#include "qpid/console/Object.h"
#include "qpid/console/Event.h"

namespace qpid {
namespace console {

    /**
     * Implement a subclass of ConsoleListener and subscribe it using
     * the SessionManager to receive indications.
     *
     * \ingroup qmfconsoleapi
     */
    class QPID_CONSOLE_EXTERN ConsoleListener{
    public:
        virtual ~ConsoleListener() {};

        /** Invoked when a connection is established to a broker
         */
        virtual void brokerConnected(const Broker&) {}

        /** Invoked when the connection to a broker is lost
         */
        virtual void brokerDisconnected(const Broker&) {}

        /** Invoked when a QMF package is discovered.
         */
        virtual void newPackage(const std::string&) {}

        /** Invoked when a new class is discovered.  Session.getSchema can be
         *  used to obtain details about the class.
         */
        virtual void newClass(const ClassKey&) {}

        /** Invoked when a QMF agent is discovered.
         */
        virtual void newAgent(const Agent&) {}

        /** Invoked when a QMF agent disconects.
         */
        virtual void delAgent(const Agent&) {}

        /** Invoked when an object is updated.
         */
        virtual void objectProps(Broker&, Object&) {}

        /** Invoked when an object is updated.
         */
        virtual void objectStats(Broker&, Object&) {}

        /** Invoked when an event is raised.
         */
        virtual void event(Event&) {}

        /**
         */
        //virtual void heartbeat(Agent&, uint64_t) {}

        /**
         */
        virtual void brokerInfo(Broker&) {}

        /**
         */
        //virtual void methodResponse(Broker&, uint32_t seq, MethodResponse&) {}
    };
}
}


#endif
