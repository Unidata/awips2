#ifndef QPID_ADDRESS_H
#define QPID_ADDRESS_H

/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "qpid/sys/IntegerTypes.h"
#include "qpid/CommonImportExport.h"
#include <iosfwd>
#include <string>

namespace qpid {
namespace client { struct ConnectionSettings; }


/**
 * Contains the protocol address of an AMQP broker. 
 */
struct Address  {
public:
    static const std::string TCP; // Default TCP protocol tag.
    static const uint16_t AMQP_PORT=5672; // Default AMQP port.
    
    QPID_COMMON_EXTERN explicit Address(
        const std::string& protocol_=std::string(),
        const std::string& host_=std::string(),
        uint16_t port_=0
    ) : protocol(protocol_), host(host_), port(port_) {}

    std::string protocol;
    std::string host;
    uint16_t port;
};

QPID_COMMON_EXTERN std::ostream& operator<<(std::ostream& os, const Address& addr);
QPID_COMMON_EXTERN bool operator==(const Address& x, const Address& y);

} // namespace qpid

#endif  /*!QPID_ADDRESS_H*/
