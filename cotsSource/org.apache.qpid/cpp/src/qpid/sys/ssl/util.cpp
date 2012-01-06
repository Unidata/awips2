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
#include "qpid/sys/ssl/util.h"
#include "qpid/sys/ssl/check.h"
#include "qpid/Exception.h"
#include "qpid/sys/SystemInfo.h"

#include <unistd.h>
#include <nspr.h>
#include <nss.h>
#include <pk11pub.h>
#include <ssl.h>

#include <iostream>
#include <fstream>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>

namespace qpid {
namespace sys {
namespace ssl {

std::string defaultCertName() 
{
    TcpAddress address;
    if (SystemInfo::getLocalHostname(address)) {
        return address.host;
    } else {
        return "localhost";
    }
}

SslOptions::SslOptions() : qpid::Options("SSL Settings"), 
                           certName(defaultCertName()),
                           exportPolicy(false)
{
    addOptions()
        ("ssl-use-export-policy", optValue(exportPolicy), "Use NSS export policy")
        ("ssl-cert-password-file", optValue(certPasswordFile, "PATH"), "File containing password to use for accessing certificate database")
        ("ssl-cert-db", optValue(certDbPath, "PATH"), "Path to directory containing certificate database")
        ("ssl-cert-name", optValue(certName, "NAME"), "Name of the certificate to use");
}

SslOptions& SslOptions::operator=(const SslOptions& o) 
{
    certDbPath = o.certDbPath;
    certName = o.certName;
    certPasswordFile = o.certPasswordFile;
    exportPolicy = o.exportPolicy;
    return *this;
}

char* promptForPassword(PK11SlotInfo*, PRBool retry, void*) 
{
    if (retry) return 0;
    //TODO: something else?
    return PL_strdup(getpass("Please enter the password for accessing the certificate database:"));
}

SslOptions SslOptions::global;

char* readPasswordFromFile(PK11SlotInfo*, PRBool retry, void*)
{
    const std::string& passwordFile = SslOptions::global.certPasswordFile;
    if (retry || passwordFile.empty() || !boost::filesystem::exists(passwordFile)) {
        return 0;
    } else {
        std::ifstream file(passwordFile.c_str());
        std::string password;
        file >> password;
        return PL_strdup(password.c_str());
    }
}    

void initNSS(const SslOptions& options, bool server)
{
    SslOptions::global = options;
    if (options.certPasswordFile.empty()) {
        PK11_SetPasswordFunc(promptForPassword);
    } else {
        PK11_SetPasswordFunc(readPasswordFromFile);
    }
    NSS_CHECK(NSS_Init(options.certDbPath.c_str()));
    if (options.exportPolicy) {
        NSS_CHECK(NSS_SetExportPolicy());
    } else {
        NSS_CHECK(NSS_SetDomesticPolicy());
    }
    if (server) {
        //use defaults for all args, TODO: may want to make this configurable
        SSL_ConfigServerSessionIDCache(0, 0, 0, 0);
    }
}

void shutdownNSS()
{
    NSS_Shutdown();
}

}}} // namespace qpid::sys::ssl
