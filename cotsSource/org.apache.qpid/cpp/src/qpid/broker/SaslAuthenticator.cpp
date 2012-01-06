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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "qpid/broker/Connection.h"
#include "qpid/log/Statement.h"
#include "qpid/framing/reply_exceptions.h"
#include <boost/format.hpp>

#if HAVE_SASL
#include <sasl/sasl.h>
#include "qpid/sys/cyrus/CyrusSecurityLayer.h"
using qpid::sys::cyrus::CyrusSecurityLayer;
#endif

using namespace qpid::framing;
using qpid::sys::SecurityLayer;
using boost::format;
using boost::str;

namespace qpid {
namespace broker {


class NullAuthenticator : public SaslAuthenticator
{
    Connection& connection;
    framing::AMQP_ClientProxy::Connection client;
    std::string realm;
    const bool encrypt;
public:
    NullAuthenticator(Connection& connection, bool encrypt);
    ~NullAuthenticator();
    void getMechanisms(framing::Array& mechanisms);
    void start(const std::string& mechanism, const std::string& response);
    void step(const std::string&) {}
    std::auto_ptr<SecurityLayer> getSecurityLayer(uint16_t maxFrameSize);
};

#if HAVE_SASL

class CyrusAuthenticator : public SaslAuthenticator
{
    sasl_conn_t *sasl_conn;
    Connection& connection;
    framing::AMQP_ClientProxy::Connection client;
    const bool encrypt;

    void processAuthenticationStep(int code, const char *challenge, unsigned int challenge_len);

public:
    CyrusAuthenticator(Connection& connection, bool encrypt);
    ~CyrusAuthenticator();
    void init();
    void getMechanisms(framing::Array& mechanisms);
    void start(const std::string& mechanism, const std::string& response);
    void step(const std::string& response);
    void getUid(std::string& uid);
    void getError(std::string& error);
    std::auto_ptr<SecurityLayer> getSecurityLayer(uint16_t maxFrameSize);
};

bool SaslAuthenticator::available(void)
{
    return true;
}

// Initialize the SASL mechanism; throw if it fails.
void SaslAuthenticator::init(const std::string& saslName)
{
    int code = sasl_server_init(NULL, saslName.c_str());
    if (code != SASL_OK) {
        // TODO: Figure out who owns the char* returned by
        // sasl_errstring, though it probably does not matter much
        throw Exception(sasl_errstring(code, NULL, NULL));
    }
}

void SaslAuthenticator::fini(void)
{
    sasl_done();
}

#else

typedef NullAuthenticator CyrusAuthenticator;

bool SaslAuthenticator::available(void)
{
    return false;
}

void SaslAuthenticator::init(const std::string& /*saslName*/)
{
    throw Exception("Requested authentication but SASL unavailable");
}

void SaslAuthenticator::fini(void)
{
    return;
}

#endif

std::auto_ptr<SaslAuthenticator> SaslAuthenticator::createAuthenticator(Connection& c)
{
    static bool needWarning = true;
    if (c.getBroker().getOptions().auth) {
        return std::auto_ptr<SaslAuthenticator>(new CyrusAuthenticator(c, c.getBroker().getOptions().requireEncrypted));
    } else {
        QPID_LOG(debug, "SASL: No Authentication Performed");
        needWarning = false;
        return std::auto_ptr<SaslAuthenticator>(new NullAuthenticator(c, c.getBroker().getOptions().requireEncrypted));
    }
}

NullAuthenticator::NullAuthenticator(Connection& c, bool e) : connection(c), client(c.getOutput()), 
                                                              realm(c.getBroker().getOptions().realm), encrypt(e) {}
NullAuthenticator::~NullAuthenticator() {}

void NullAuthenticator::getMechanisms(Array& mechanisms)
{
    mechanisms.add(boost::shared_ptr<FieldValue>(new Str16Value("ANONYMOUS")));
    mechanisms.add(boost::shared_ptr<FieldValue>(new Str16Value("PLAIN")));//useful for testing
}

void NullAuthenticator::start(const string& mechanism, const string& response)
{
    if (encrypt) {
        QPID_LOG(error, "Rejected un-encrypted connection.");
        throw ConnectionForcedException("Connection must be encrypted.");
    }
    if (mechanism == "PLAIN") { // Old behavior
        if (response.size() > 0) {
            string uid;
            string::size_type i = response.find((char)0);
            if (i == 0 && response.size() > 1) {
                //no authorization id; use authentication id
                i = response.find((char)0, 1);
                if (i != string::npos) uid = response.substr(1, i-1);
            } else if (i != string::npos) {
                //authorization id is first null delimited field
                uid = response.substr(0, i);
            }//else not a valid SASL PLAIN response, throw error?            
            if (!uid.empty()) {
                //append realm if it has not already been added
                i = uid.find(realm);
                if (i == string::npos || realm.size() + i < uid.size()) {
                    uid = str(format("%1%@%2%") % uid % realm);
                }
                connection.setUserId(uid);
            }
        }
    } else {
        connection.setUserId("anonymous");
    }   
    client.tune(framing::CHANNEL_MAX, connection.getFrameMax(), 0, connection.getHeartbeatMax());
}


std::auto_ptr<SecurityLayer> NullAuthenticator::getSecurityLayer(uint16_t)
{
    std::auto_ptr<SecurityLayer> securityLayer;
    return securityLayer;
}


#if HAVE_SASL


CyrusAuthenticator::CyrusAuthenticator(Connection& c, bool _encrypt) : 
    sasl_conn(0), connection(c), client(c.getOutput()), encrypt(_encrypt)
{
    init();
}

void CyrusAuthenticator::init()
{
        /* Next to the service name, which specifies the
          * /etc/sasl2/<service name>.conf file to read, the realm is
          * currently the most important argument below. When
          * performing authentication the user that is authenticating
          * will be looked up in a specific realm. If none is given
          * then the realm defaults to the hostname, which can cause
          * confusion when the daemon is run on different hosts that
          * may be logically sharing a realm (aka a user domain). This
          * is especially important for SASL PLAIN authentication,
          * which cannot specify a realm for the user that is
          * authenticating.
          */
    const char *realm = connection.getBroker().getOptions().realm.c_str();
    int code = sasl_server_new(BROKER_SASL_NAME, /* Service name */
                               NULL, /* Server FQDN, gethostname() */
                               realm, /* Authentication realm */
                               NULL, /* Local IP, needed for some mechanism */
                               NULL, /* Remote IP, needed for some mechanism */
                               NULL, /* Callbacks */
                               0, /* Connection flags */
                               &sasl_conn);
    
    if (SASL_OK != code) {
        QPID_LOG(info, "SASL: Connection creation failed: [" << code << "] " << sasl_errdetail(sasl_conn));
        
        // TODO: Change this to an exception signaling
        // server error, when one is available
        throw ConnectionForcedException("Unable to perform authentication");
    }

    sasl_security_properties_t secprops;
    
    //TODO: should the actual SSF values be configurable here?
    secprops.min_ssf = encrypt ? 10: 0;
    secprops.max_ssf = 256;

    // If the transport provides encryption, notify the SASL library of
    // the key length and set the ssf range to prevent double encryption.
    sasl_ssf_t external_ssf = (sasl_ssf_t) connection.getSSF();
    if (external_ssf) {
        int result = sasl_setprop(sasl_conn, SASL_SSF_EXTERNAL, &external_ssf);
        if (result != SASL_OK) {
            throw framing::InternalErrorException(QPID_MSG("SASL error: unable to set external SSF: " << result));
        }

        secprops.max_ssf = secprops.min_ssf = 0;
    }

    QPID_LOG(debug, "min_ssf: " << secprops.min_ssf <<
             ", max_ssf: " << secprops.max_ssf <<
             ", external_ssf: " << external_ssf );

    secprops.maxbufsize = 65535;
    secprops.property_names = 0;
    secprops.property_values = 0;
    secprops.security_flags = 0; /* or SASL_SEC_NOANONYMOUS etc as appropriate */
    
    int result = sasl_setprop(sasl_conn, SASL_SEC_PROPS, &secprops);
    if (result != SASL_OK) {
        throw framing::InternalErrorException(QPID_MSG("SASL error: " << result));
    }

}

CyrusAuthenticator::~CyrusAuthenticator()
{
    if (sasl_conn) {
        sasl_dispose(&sasl_conn);
        sasl_conn = 0;
    }
}

void CyrusAuthenticator::getError(string& error)
{
    error = string(sasl_errdetail(sasl_conn));
}

void CyrusAuthenticator::getUid(string& uid)
{
    int code;
    const void* ptr;

    code = sasl_getprop(sasl_conn, SASL_USERNAME, &ptr);
    if (SASL_OK != code)
        return;

    uid = string(const_cast<char*>(static_cast<const char*>(ptr)));
}

void CyrusAuthenticator::getMechanisms(Array& mechanisms)
{
    const char *separator = " ";
    const char *list;
    unsigned int list_len;
    int count;

    int code = sasl_listmech(sasl_conn, NULL,
                             "", separator, "",
                             &list, &list_len,
                             &count);
    
    if (SASL_OK != code) {
        QPID_LOG(info, "SASL: Mechanism listing failed: " << sasl_errdetail(sasl_conn));
        
        // TODO: Change this to an exception signaling
        // server error, when one is available
        throw ConnectionForcedException("Mechanism listing failed");
    } else {
        string mechanism;
        unsigned int start;
        unsigned int end;
        
        QPID_LOG(info, "SASL: Mechanism list: " << list);
        
        end = 0;
        do {
            start = end;
            
            // Seek to end of next mechanism
            while (end < list_len && separator[0] != list[end])
                end++;
            
            // Record the mechanism
            mechanisms.add(boost::shared_ptr<FieldValue>(new Str16Value(string(list, start, end - start))));
            end++;
        } while (end < list_len);
    }
}

void CyrusAuthenticator::start(const string& mechanism, const string& response)
{
    const char *challenge;
    unsigned int challenge_len;
    
    QPID_LOG(info, "SASL: Starting authentication with mechanism: " << mechanism);
    int code = sasl_server_start(sasl_conn,
                                 mechanism.c_str(),
                                 response.c_str(), response.length(),
                                 &challenge, &challenge_len);
    
    processAuthenticationStep(code, challenge, challenge_len);
}
        
void CyrusAuthenticator::step(const string& response)
{
    const char *challenge;
    unsigned int challenge_len;

    int code = sasl_server_step(sasl_conn,
                            response.c_str(), response.length(),
                            &challenge, &challenge_len);

    processAuthenticationStep(code, challenge, challenge_len);
}

void CyrusAuthenticator::processAuthenticationStep(int code, const char *challenge, unsigned int challenge_len)
{
    if (SASL_OK == code) {
        const void *uid;

        code = sasl_getprop(sasl_conn, SASL_USERNAME, &uid);
        if (SASL_OK != code) {
            QPID_LOG(info, "SASL: Authentication succeeded, username unavailable");
            // TODO: Change this to an exception signaling
            // authentication failure, when one is available
            throw ConnectionForcedException("Authenticated username unavailable");
        }

        QPID_LOG(info, "SASL: Authentication succeeded for: "
                 << const_cast<char*>(static_cast<const char*>(uid)));

        connection.setUserId(const_cast<char*>(static_cast<const char*>(uid)));

        client.tune(framing::CHANNEL_MAX, connection.getFrameMax(), 0, connection.getHeartbeatMax());
    } else if (SASL_CONTINUE == code) {
        string challenge_str(challenge, challenge_len);

        QPID_LOG(debug, "SASL: sending challenge to client");

        client.secure(challenge_str);
    } else {
        QPID_LOG(info, "SASL: Authentication failed: " << sasl_errdetail(sasl_conn));

        // TODO: Change to more specific exceptions, when they are
        // available
        switch (code) {
        case SASL_NOMECH:
            throw ConnectionForcedException("Unsupported mechanism");
            break;
        case SASL_TRYAGAIN:
            throw ConnectionForcedException("Transient failure, try again");
            break;
        default:
            throw ConnectionForcedException("Authentication failed");
            break;
        }
    }
}

std::auto_ptr<SecurityLayer> CyrusAuthenticator::getSecurityLayer(uint16_t maxFrameSize)
{

    const void* value(0);
    int result = sasl_getprop(sasl_conn, SASL_SSF, &value);
    if (result != SASL_OK) {
        throw framing::InternalErrorException(QPID_MSG("SASL error: " << sasl_errdetail(sasl_conn)));
    }
    uint ssf = *(reinterpret_cast<const unsigned*>(value));
    std::auto_ptr<SecurityLayer> securityLayer;
    if (ssf) {
        QPID_LOG(info, "Installing security layer,  SSF: "<< ssf);
        securityLayer = std::auto_ptr<SecurityLayer>(new CyrusSecurityLayer(sasl_conn, maxFrameSize));
    }
    return securityLayer;
}

#endif

}}
