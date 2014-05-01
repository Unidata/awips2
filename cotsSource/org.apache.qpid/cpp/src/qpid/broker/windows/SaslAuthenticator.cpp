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

// This source is only used on Windows; SSPI is the Windows mechanism for
// accessing authentication mechanisms, analogous to Cyrus SASL.

#include "qpid/broker/Connection.h"
#include "qpid/log/Statement.h"
#include "qpid/framing/reply_exceptions.h"

#include <windows.h>

using namespace qpid::framing;
using qpid::sys::SecurityLayer;

namespace qpid {
namespace broker {

class NullAuthenticator : public SaslAuthenticator
{
    Connection& connection;
    framing::AMQP_ClientProxy::Connection client;
public:
    NullAuthenticator(Connection& connection);
    ~NullAuthenticator();
    void getMechanisms(framing::Array& mechanisms);
    void start(const std::string& mechanism, const std::string& response);
    void step(const std::string&) {}
    std::auto_ptr<SecurityLayer> getSecurityLayer(uint16_t maxFrameSize);
};

class SspiAuthenticator : public SaslAuthenticator
{
    HANDLE userToken;
    Connection& connection;
    framing::AMQP_ClientProxy::Connection client;

public:
    SspiAuthenticator(Connection& connection);
    ~SspiAuthenticator();
    void getMechanisms(framing::Array& mechanisms);
    void start(const std::string& mechanism, const std::string& response);
    void step(const std::string& response);
    std::auto_ptr<SecurityLayer> getSecurityLayer(uint16_t maxFrameSize);
};

bool SaslAuthenticator::available(void)
{
    return true;
}

// Initialize the SASL mechanism; throw if it fails.
void SaslAuthenticator::init(const std::string& /*saslName*/)
{
    return;
}

void SaslAuthenticator::fini(void)
{
    return;
}

std::auto_ptr<SaslAuthenticator> SaslAuthenticator::createAuthenticator(Connection& c)
{
    if (c.getBroker().getOptions().auth) {
        return std::auto_ptr<SaslAuthenticator>(new SspiAuthenticator(c));
    } else {
        return std::auto_ptr<SaslAuthenticator>(new NullAuthenticator(c));
    }
}

NullAuthenticator::NullAuthenticator(Connection& c) : connection(c), client(c.getOutput()) {}
NullAuthenticator::~NullAuthenticator() {}

void NullAuthenticator::getMechanisms(Array& mechanisms)
{
    mechanisms.add(boost::shared_ptr<FieldValue>(new Str16Value("ANONYMOUS")));
}

void NullAuthenticator::start(const string& mechanism, const string& response)
{
    QPID_LOG(warning, "SASL: No Authentication Performed");
    if (mechanism == "PLAIN") { // Old behavior
        if (response.size() > 0 && response[0] == (char) 0) {
            string temp = response.substr(1);
            string::size_type i = temp.find((char)0);
            string uid = temp.substr(0, i);
            string pwd = temp.substr(i + 1);
            connection.setUserId(uid);
        }
    } else {
        connection.setUserId("anonymous");
    }   
    client.tune(framing::CHANNEL_MAX, connection.getFrameMax(), 0, 0);
}

std::auto_ptr<SecurityLayer> NullAuthenticator::getSecurityLayer(uint16_t)
{
    std::auto_ptr<SecurityLayer> securityLayer;
    return securityLayer;
}


SspiAuthenticator::SspiAuthenticator(Connection& c) : userToken(INVALID_HANDLE_VALUE), connection(c), client(c.getOutput()) 
{
}

SspiAuthenticator::~SspiAuthenticator()
{
    if (INVALID_HANDLE_VALUE != userToken) {
        CloseHandle(userToken);
        userToken = INVALID_HANDLE_VALUE;
    }
}

void SspiAuthenticator::getMechanisms(Array& mechanisms)
{
    mechanisms.add(boost::shared_ptr<FieldValue>(new Str16Value(string("ANONYMOUS"))));
    mechanisms.add(boost::shared_ptr<FieldValue>(new Str16Value(string("PLAIN"))));
    QPID_LOG(info, "SASL: Mechanism list: ANONYMOUS PLAIN");
}

void SspiAuthenticator::start(const string& mechanism, const string& response)
{
    QPID_LOG(info, "SASL: Starting authentication with mechanism: " << mechanism);
    if (mechanism == "ANONYMOUS") {
        connection.setUserId("anonymous");
        client.tune(framing::CHANNEL_MAX, connection.getFrameMax(), 0, 0);
        return;
    }
    if (mechanism != "PLAIN")
        throw ConnectionForcedException("Unsupported mechanism");

    // PLAIN's response is composed of 3 strings separated by 0 bytes:
    // authorization id, authentication id (user), clear-text password.
    if (response.size() == 0)
        throw ConnectionForcedException("Authentication failed");

    string::size_type i = response.find((char)0);
    string auth = response.substr(0, i);
    string::size_type j = response.find((char)0, i+1);
    string uid = response.substr(i+1, j-1);
    string pwd = response.substr(j+1);
    int error = 0;
    if (!LogonUser(uid.c_str(), ".", pwd.c_str(),
                   LOGON32_LOGON_NETWORK,
                   LOGON32_PROVIDER_DEFAULT,
                   &userToken))
        error = GetLastError();
    pwd.replace(0, string::npos, 1, (char)0);
    if (error != 0) {
      QPID_LOG(info,
               "SASL: Auth failed [" << error << "]: " << qpid::sys::strError(error));
        throw ConnectionForcedException("Authentication failed");
    }

    connection.setUserId(uid);
    client.tune(framing::CHANNEL_MAX, connection.getFrameMax(), 0, 0);
}
        
void SspiAuthenticator::step(const string& response)
{
  QPID_LOG(info, "SASL: Need another step!!!");
}

std::auto_ptr<SecurityLayer> SspiAuthenticator::getSecurityLayer(uint16_t)
{
    std::auto_ptr<SecurityLayer> securityLayer;
    return securityLayer;
}

}}
