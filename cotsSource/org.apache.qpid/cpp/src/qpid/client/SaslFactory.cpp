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
#include "qpid/client/SaslFactory.h"
#include "qpid/client/ConnectionSettings.h"
#include <map>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifndef HAVE_SASL

namespace qpid {
namespace client {

//Null implementation

SaslFactory::SaslFactory() {}

SaslFactory::~SaslFactory() {}

SaslFactory& SaslFactory::getInstance()
{
    qpid::sys::Mutex::ScopedLock l(lock);
    if (!instance.get()) {
        instance = std::auto_ptr<SaslFactory>(new SaslFactory());
    }
    return *instance;
}

std::auto_ptr<Sasl> SaslFactory::create(const ConnectionSettings&)
{
    return std::auto_ptr<Sasl>();
}

qpid::sys::Mutex SaslFactory::lock;
std::auto_ptr<SaslFactory> SaslFactory::instance;

}} // namespace qpid::client

#else

#include "qpid/Exception.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/sys/SecurityLayer.h"
#include "qpid/sys/cyrus/CyrusSecurityLayer.h"
#include "qpid/log/Statement.h"
#include <sasl/sasl.h>
#include <strings.h>

namespace qpid {
namespace client {

using qpid::sys::SecurityLayer;
using qpid::sys::cyrus::CyrusSecurityLayer;
using qpid::framing::InternalErrorException;

const size_t MAX_LOGIN_LENGTH = 50;

class CyrusSasl : public Sasl
{
  public:
    CyrusSasl(const ConnectionSettings&);
    ~CyrusSasl();
    std::string start(const std::string& mechanisms, unsigned int ssf);
    std::string step(const std::string& challenge);
    std::string getMechanism();
    std::string getUserId();
    std::auto_ptr<SecurityLayer> getSecurityLayer(uint16_t maxFrameSize);
  private:
    sasl_conn_t* conn;    
    sasl_callback_t callbacks[5];//realm, user, authname, password, end-of-list
    ConnectionSettings settings;
    std::string input;
    std::string mechanism;
    char login[MAX_LOGIN_LENGTH];

    void interact(sasl_interact_t* client_interact);
};

//sasl callback functions
int getUserFromSettings(void *context, int id, const char **result, unsigned *len);
int getPasswordFromSettings(sasl_conn_t *conn, void *context, int id, sasl_secret_t **psecret);
typedef int CallbackProc();

qpid::sys::Mutex SaslFactory::lock;
std::auto_ptr<SaslFactory> SaslFactory::instance;

SaslFactory::SaslFactory()
{
    sasl_callback_t* callbacks = 0;
    int result = sasl_client_init(callbacks);
    if (result != SASL_OK) {
        throw InternalErrorException(QPID_MSG("Sasl error: " << sasl_errstring(result, 0, 0)));
    }
}

SaslFactory::~SaslFactory()
{
    sasl_done();
}

SaslFactory& SaslFactory::getInstance()
{
    qpid::sys::Mutex::ScopedLock l(lock);
    if (!instance.get()) {
        instance = std::auto_ptr<SaslFactory>(new SaslFactory());
    }
    return *instance;
}

std::auto_ptr<Sasl> SaslFactory::create(const ConnectionSettings& settings)
{
    std::auto_ptr<Sasl> sasl(new CyrusSasl(settings));
    return sasl;
}

CyrusSasl::CyrusSasl(const ConnectionSettings& s) : conn(0), settings(s) 
{
    size_t i = 0;

    callbacks[i].id = SASL_CB_GETREALM;
    callbacks[i].proc = 0;
    callbacks[i++].context = 0;

    if (!settings.username.empty()) {
        callbacks[i].id = SASL_CB_USER;
        callbacks[i].proc = (CallbackProc*) &getUserFromSettings;
        callbacks[i++].context = &settings;

        callbacks[i].id = SASL_CB_AUTHNAME;
        callbacks[i].proc = (CallbackProc*) &getUserFromSettings;
        callbacks[i++].context = &settings;
    }

    callbacks[i].id = SASL_CB_PASS;
    if (settings.password.empty()) {
        callbacks[i].proc = 0;
        callbacks[i++].context = 0;        
    } else {
        callbacks[i].proc = (CallbackProc*) &getPasswordFromSettings;
        callbacks[i++].context = &settings;
    }

    callbacks[i].id = SASL_CB_LIST_END;
    callbacks[i].proc = 0;
    callbacks[i++].context = 0;
}

CyrusSasl::~CyrusSasl() 
{
    if (conn) {
        sasl_dispose(&conn);
    }
}

namespace {
    const std::string SSL("ssl");
}

std::string CyrusSasl::start(const std::string& mechanisms, unsigned int ssf)
{
    QPID_LOG(debug, "CyrusSasl::start(" << mechanisms << ")");
    int result = sasl_client_new(settings.service.c_str(),
                                 settings.host.c_str(),
                                 0, 0, /* Local and remote IP address strings */
                                 callbacks,
                                 0,          /* security flags */
                                 &conn);
    
    if (result != SASL_OK) throw InternalErrorException(QPID_MSG("Sasl error: " << sasl_errdetail(conn)));

    sasl_security_properties_t secprops;

    if (ssf) {
        sasl_ssf_t external_ssf = (sasl_ssf_t) ssf;
        if (external_ssf) {
            int result = sasl_setprop(conn, SASL_SSF_EXTERNAL, &external_ssf);
            if (result != SASL_OK) {
                throw framing::InternalErrorException(QPID_MSG("SASL error: unable to set external SSF: " << result));
            }
            QPID_LOG(debug, "external SSF detected and set to " << ssf);
        }
    }

    secprops.min_ssf = settings.minSsf;
    secprops.max_ssf = settings.maxSsf;
    secprops.maxbufsize = 65535;

    QPID_LOG(debug, "min_ssf: " << secprops.min_ssf << ", max_ssf: " << secprops.max_ssf);
    
    secprops.property_names = 0;
    secprops.property_values = 0;
    secprops.security_flags = 0;//TODO: provide means for application to configure these
    
    result = sasl_setprop(conn, SASL_SEC_PROPS, &secprops);
    if (result != SASL_OK) {
        throw framing::InternalErrorException(QPID_MSG("SASL error: " << sasl_errdetail(conn)));
    }


    sasl_interact_t* client_interact = 0;
    const char *out = 0;
    unsigned outlen = 0;
    const char *chosenMechanism = 0;

    do {        
        result = sasl_client_start(conn,
                                   mechanisms.c_str(),
                                   &client_interact,
                                   &out,
                                   &outlen,
                                   &chosenMechanism);
        
        if (result == SASL_INTERACT) {
            interact(client_interact);
        }        
    } while (result == SASL_INTERACT);

    if (result != SASL_CONTINUE && result != SASL_OK) {
        throw InternalErrorException(QPID_MSG("Sasl error: " << sasl_errdetail(conn)));
    }

    mechanism = std::string(chosenMechanism);
    QPID_LOG(debug, "CyrusSasl::start(" << mechanisms << "): selected "
             << mechanism << " response: '" << std::string(out, outlen) << "'");
    return std::string(out, outlen);
}

std::string CyrusSasl::step(const std::string& challenge)
{
    sasl_interact_t* client_interact = 0;
    const char *out = 0;
    unsigned outlen = 0;
    int result = 0;
    do {
        result = sasl_client_step(conn,  /* our context */
                                  challenge.data(), /* the data from the server */
                                  challenge.size(), /* it's length */
                                  &client_interact,  /* this should be
                                                        unallocated and NULL */
                                  &out,     /* filled in on success */
                                  &outlen); /* filled in on success */
        
        if (result == SASL_INTERACT) {
            interact(client_interact);
        }        
    } while (result == SASL_INTERACT);

    std::string response;
    if (result == SASL_CONTINUE || result == SASL_OK) response = std::string(out, outlen);
    else if (result != SASL_OK) {
        throw InternalErrorException(QPID_MSG("Sasl error: " << sasl_errdetail(conn)));
    }
    QPID_LOG(debug, "CyrusSasl::step(" << challenge << "): " << response);
    return response;
}

std::string CyrusSasl::getMechanism()
{
    return mechanism;
}

std::string CyrusSasl::getUserId()
{
    int propResult;
    const void* operName;

    propResult = sasl_getprop(conn, SASL_USERNAME, &operName);
    if (propResult == SASL_OK)
        return std::string((const char*) operName);

    return std::string();
}

void CyrusSasl::interact(sasl_interact_t* client_interact)
{

    if (client_interact->id == SASL_CB_PASS) {
        char* password = getpass(client_interact->prompt);
        input = std::string(password);
        client_interact->result = input.data();
        client_interact->len = input.size();
    } else {
        std::cout << client_interact->prompt;
        if (client_interact->defresult) std::cout << " (" << client_interact->defresult << ")";
        std::cout << ": ";
        if (std::cin >> input) {
            client_interact->result = input.data();
            client_interact->len = input.size();
        }
    }

}

std::auto_ptr<SecurityLayer> CyrusSasl::getSecurityLayer(uint16_t maxFrameSize)
{
    const void* value(0);
    int result = sasl_getprop(conn, SASL_SSF, &value);
    if (result != SASL_OK) {
        throw framing::InternalErrorException(QPID_MSG("SASL error: " << sasl_errdetail(conn)));
    }
    uint ssf = *(reinterpret_cast<const unsigned*>(value));
    std::auto_ptr<SecurityLayer> securityLayer;
    if (ssf) {
        QPID_LOG(info, "Installing security layer,  SSF: "<< ssf);
        securityLayer = std::auto_ptr<SecurityLayer>(new CyrusSecurityLayer(conn, maxFrameSize));
    }
    return securityLayer;
}

int getUserFromSettings(void* context, int /*id*/, const char** result, unsigned* /*len*/)
{
    if (context) {
        *result = ((ConnectionSettings*) context)->username.c_str();
        QPID_LOG(debug, "getUserFromSettings(): " << (*result));
        return SASL_OK;
    } else {
        return SASL_FAIL;
    }
}

namespace {
// Global map of secrest allocated for SASL connections via callback
// to getPasswordFromSettings. Ensures secrets are freed.
class SecretsMap {
    typedef std::map<sasl_conn_t*, void*> Map;
    Map map;
  public:
    void keep(sasl_conn_t* conn, void* secret) {
        Map::iterator i = map.find(conn);
        if (i != map.end()) free(i->second);
        map[conn] = secret;
    }

    ~SecretsMap() {
        for (Map::iterator i = map.begin(); i != map.end(); ++i)
            free(i->second);
    }
};
SecretsMap getPasswordFromSettingsSecrets;
}

int getPasswordFromSettings(sasl_conn_t* conn, void* context, int /*id*/, sasl_secret_t** psecret)
{
    if (context) {
        size_t length = ((ConnectionSettings*) context)->password.size();
        sasl_secret_t* secret = (sasl_secret_t*) malloc(sizeof(sasl_secret_t) + length);
        getPasswordFromSettingsSecrets.keep(conn, secret);
        secret->len = length;
        memcpy(secret->data, ((ConnectionSettings*) context)->password.data(), length);
        *psecret = secret;
        return SASL_OK;
    } else {
        return SASL_FAIL;
    }
}

}} // namespace qpid::client

#endif
