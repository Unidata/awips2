#ifndef QPID_MESSAGING_CONNECTION_H
#define QPID_MESSAGING_CONNECTION_H

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
#include <string>
#include "qpid/client/ClientImportExport.h"
#include "qpid/client/Handle.h"
#include "qpid/messaging/Variant.h"

namespace qpid {
namespace client {

template <class> class PrivateImplRef;

}

namespace messaging {

class ConnectionImpl;
class Session;

class Connection : public qpid::client::Handle<ConnectionImpl>
{
  public:
    /**
     * Current implementation supports the following options:
     * 
     *     username
     *     password
     *     heartbeat
     *     tcp-nodelay
     *     sasl-mechanism
     *     sasl-min-ssf
     *     sasl-max-ssf
     * 
     * (note also bounds, locale, max-channels and max-framesize, but not sure whether those should be docuemented here)
     * 
     * Retry behaviour can be controlled through the following options:
     *
     *     reconnection-timeout - determines how long it will try to
     *                            reconnect for -1 means forever, 0
     *                            means don't try to reconnect
     *     min-retry-interval
     *     max-retry-interval
     * 
     *     The retry-interval is the time that the client waits for
     *     after a failed attempt to reconnect before retrying. It
     *     starts at the value of the min-retry-interval and is
     *     doubled every failure until the value of max-retry-interval
     *     is reached.
     * 
     *
     */
    static QPID_CLIENT_EXTERN Connection open(const std::string& url, const Variant::Map& options = Variant::Map());

    QPID_CLIENT_EXTERN Connection(ConnectionImpl* impl = 0);
    QPID_CLIENT_EXTERN Connection(const Connection&);
    QPID_CLIENT_EXTERN ~Connection();
    QPID_CLIENT_EXTERN Connection& operator=(const Connection&);
    QPID_CLIENT_EXTERN void close();
    QPID_CLIENT_EXTERN Session newSession(bool transactional, const std::string& name = std::string());
    QPID_CLIENT_EXTERN Session newSession(const std::string& name = std::string());
    QPID_CLIENT_EXTERN Session newSession(const char* name);

    QPID_CLIENT_EXTERN Session getSession(const std::string& name) const;
  private:
  friend class qpid::client::PrivateImplRef<Connection>;

};

struct InvalidOptionString : public qpid::Exception 
{
    InvalidOptionString(const std::string& msg);
};

/**
 * TODO: need to change format of connection option string (currently
 * name1=value1&name2=value2 etc, should probably use map syntax as
 * per address options.
 */
QPID_CLIENT_EXTERN void parseOptionString(const std::string&, Variant::Map&);
QPID_CLIENT_EXTERN Variant::Map parseOptionString(const std::string&);

}} // namespace qpid::messaging

#endif  /*!QPID_MESSAGING_CONNECTION_H*/
