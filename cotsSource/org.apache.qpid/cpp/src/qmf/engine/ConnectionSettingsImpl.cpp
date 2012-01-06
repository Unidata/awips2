/*
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
 */

#include "qmf/engine/ConnectionSettingsImpl.h"
#include "qmf/engine/Typecode.h"

using namespace std;
using namespace qmf::engine;
using namespace qpid;

const string attrProtocol("protocol");
const string attrHost("host");
const string attrPort("port");
const string attrVirtualhost("virtualhost");
const string attrUsername("username");
const string attrPassword("password");
const string attrMechanism("mechanism");
const string attrLocale("locale");
const string attrHeartbeat("heartbeat");
const string attrMaxChannels("maxChannels");
const string attrMaxFrameSize("maxFrameSize");
const string attrBounds("bounds");
const string attrTcpNoDelay("tcpNoDelay");
const string attrService("service");
const string attrMinSsf("minSsf");
const string attrMaxSsf("maxSsf");
const string attrRetryDelayMin("retryDelayMin");
const string attrRetryDelayMax("retryDelayMax");
const string attrRetryDelayFactor("retryDelayFactor");
const string attrSendUserId("sendUserId");

ConnectionSettingsImpl::ConnectionSettingsImpl() :
    retryDelayMin(1), retryDelayMax(64), retryDelayFactor(2), sendUserId(true)
{
}

ConnectionSettingsImpl::ConnectionSettingsImpl(const string& /*url*/) :
    retryDelayMin(1), retryDelayMax(64), retryDelayFactor(2), sendUserId(true)
{
    // TODO: Parse the URL
}

bool ConnectionSettingsImpl::setAttr(const string& key, const Value& value)
{
    if      (key == attrProtocol)     clientSettings.protocol     = value.asString();
    else if (key == attrHost)         clientSettings.host         = value.asString();
    else if (key == attrPort)         clientSettings.port         = value.asUint();
    else if (key == attrVirtualhost)  clientSettings.virtualhost  = value.asString();
    else if (key == attrUsername)     clientSettings.username     = value.asString();
    else if (key == attrPassword)     clientSettings.password     = value.asString();
    else if (key == attrMechanism)    clientSettings.mechanism    = value.asString();
    else if (key == attrLocale)       clientSettings.locale       = value.asString();
    else if (key == attrHeartbeat)    clientSettings.heartbeat    = value.asUint();
    else if (key == attrMaxChannels)  clientSettings.maxChannels  = value.asUint();
    else if (key == attrMaxFrameSize) clientSettings.maxFrameSize = value.asUint();
    else if (key == attrBounds)       clientSettings.bounds       = value.asUint();
    else if (key == attrTcpNoDelay)   clientSettings.tcpNoDelay   = value.asBool();
    else if (key == attrService)      clientSettings.service      = value.asString();
    else if (key == attrMinSsf)       clientSettings.minSsf       = value.asUint();
    else if (key == attrMaxSsf)       clientSettings.maxSsf       = value.asUint();

    else if (key == attrRetryDelayMin)    retryDelayMin    = value.asUint();
    else if (key == attrRetryDelayMax)    retryDelayMax    = value.asUint();
    else if (key == attrRetryDelayFactor) retryDelayFactor = value.asUint();
    else if (key == attrSendUserId)       sendUserId       = value.asBool();
    else
        return false;
    return true;
}

Value ConnectionSettingsImpl::getAttr(const string& key) const
{
    Value strval(TYPE_LSTR);
    Value intval(TYPE_UINT32);
    Value boolval(TYPE_BOOL);

    if (key == attrProtocol) {
        strval.setString(clientSettings.protocol.c_str());
        return strval;
    }

    if (key == attrHost) {
        strval.setString(clientSettings.host.c_str());
        return strval;
    }

    if (key == attrPort) {
        intval.setUint(clientSettings.port);
        return intval;
    }

    if (key == attrVirtualhost) {
        strval.setString(clientSettings.virtualhost.c_str());
        return strval;
    }

    if (key == attrUsername) {
        strval.setString(clientSettings.username.c_str());
        return strval;
    }

    if (key == attrPassword) {
        strval.setString(clientSettings.password.c_str());
        return strval;
    }

    if (key == attrMechanism) {
        strval.setString(clientSettings.mechanism.c_str());
        return strval;
    }

    if (key == attrLocale) {
        strval.setString(clientSettings.locale.c_str());
        return strval;
    }

    if (key == attrHeartbeat) {
        intval.setUint(clientSettings.heartbeat);
        return intval;
    }

    if (key == attrMaxChannels) {
        intval.setUint(clientSettings.maxChannels);
        return intval;
    }

    if (key == attrMaxFrameSize) {
        intval.setUint(clientSettings.maxFrameSize);
        return intval;
    }

    if (key == attrBounds) {
        intval.setUint(clientSettings.bounds);
        return intval;
    }

    if (key == attrTcpNoDelay) {
        boolval.setBool(clientSettings.tcpNoDelay);
        return boolval;
    }

    if (key == attrService) {
        strval.setString(clientSettings.service.c_str());
        return strval;
    }

    if (key == attrMinSsf) {
        intval.setUint(clientSettings.minSsf);
        return intval;
    }

    if (key == attrMaxSsf) {
        intval.setUint(clientSettings.maxSsf);
        return intval;
    }

    if (key == attrRetryDelayMin) {
        intval.setUint(retryDelayMin);
        return intval;
    }

    if (key == attrRetryDelayMax) {
        intval.setUint(retryDelayMax);
        return intval;
    }

    if (key == attrRetryDelayFactor) {
        intval.setUint(retryDelayFactor);
        return intval;
    }

    if (key == attrSendUserId) {
        boolval.setBool(sendUserId);
        return boolval;
    }

    return strval;
}

const string& ConnectionSettingsImpl::getAttrString() const
{
    // TODO: build and return attribute string
    return attrString;
}

void ConnectionSettingsImpl::transportTcp(uint16_t port)
{
    clientSettings.protocol = "tcp";
    clientSettings.port = port;
}

void ConnectionSettingsImpl::transportSsl(uint16_t port)
{
    clientSettings.protocol = "ssl";
    clientSettings.port = port;
}

void ConnectionSettingsImpl::transportRdma(uint16_t port)
{
    clientSettings.protocol = "rdma";
    clientSettings.port = port;
}

void ConnectionSettingsImpl::authAnonymous(const string& username)
{
    clientSettings.mechanism = "ANONYMOUS";
    clientSettings.username = username;
}

void ConnectionSettingsImpl::authPlain(const string& username, const string& password)
{
    clientSettings.mechanism = "PLAIN";
    clientSettings.username = username;
    clientSettings.password = password;
}

void ConnectionSettingsImpl::authGssapi(const string& serviceName, uint32_t minSsf, uint32_t maxSsf)
{
    clientSettings.mechanism = "GSSAPI";
    clientSettings.service = serviceName;
    clientSettings.minSsf = minSsf;
    clientSettings.maxSsf = maxSsf;
}

void ConnectionSettingsImpl::setRetry(int delayMin, int delayMax, int delayFactor)
{
    retryDelayMin = delayMin;
    retryDelayMax = delayMax;
    retryDelayFactor = delayFactor;
}

const client::ConnectionSettings& ConnectionSettingsImpl::getClientSettings() const
{
    return clientSettings;
}

void ConnectionSettingsImpl::getRetrySettings(int* min, int* max, int* factor) const
{
    *min = retryDelayMin;
    *max = retryDelayMax;
    *factor = retryDelayFactor;
}

//==================================================================
// Wrappers
//==================================================================

ConnectionSettings::ConnectionSettings(const ConnectionSettings& from) { impl = new ConnectionSettingsImpl(*from.impl); }
ConnectionSettings::ConnectionSettings() { impl = new ConnectionSettingsImpl(); }
ConnectionSettings::ConnectionSettings(const char* url) { impl = new ConnectionSettingsImpl(url); }
ConnectionSettings::~ConnectionSettings() { delete impl; }
bool ConnectionSettings::setAttr(const char* key, const Value& value) { return impl->setAttr(key, value); }
Value ConnectionSettings::getAttr(const char* key) const { return impl->getAttr(key); }
const char* ConnectionSettings::getAttrString() const { return impl->getAttrString().c_str(); }
void ConnectionSettings::transportTcp(uint16_t port) { impl->transportTcp(port); }
void ConnectionSettings::transportSsl(uint16_t port) { impl->transportSsl(port); }
void ConnectionSettings::transportRdma(uint16_t port) { impl->transportRdma(port); }
void ConnectionSettings::authAnonymous(const char* username) { impl->authAnonymous(username); }
void ConnectionSettings::authPlain(const char* username, const char* password) { impl->authPlain(username, password); }
void ConnectionSettings::authGssapi(const char* serviceName, uint32_t minSsf, uint32_t maxSsf) { impl->authGssapi(serviceName, minSsf, maxSsf); }
void ConnectionSettings::setRetry(int delayMin, int delayMax, int delayFactor) { impl->setRetry(delayMin, delayMax, delayFactor); }

