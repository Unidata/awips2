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
 *
 */

#include "qpid/sys/SystemInfo.h"

#define BSD_COMP
#include <sys/ioctl.h>
#include <netdb.h>
#undef BDS_COMP


#include <unistd.h>
#include <net/if.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <errno.h>
#include <limits.h>
#include <procfs.h>
#include <fcntl.h>
#include <sys/types.h>

using namespace std;

namespace qpid {
namespace sys {

long  SystemInfo::concurrency() {
    return sysconf(_SC_NPROCESSORS_ONLN);
}

bool SystemInfo::getLocalHostname(TcpAddress &address) {
    char name[MAXHOSTNAMELEN];
    if (::gethostname(name, sizeof(name)) != 0)
        return false;
    address.host = name;
    return true;
}
 
static const string LOCALHOST("127.0.0.1");

void SystemInfo::getLocalIpAddresses(uint16_t port,
                                     std::vector<Address> &addrList) {
    int s = socket(PF_INET, SOCK_STREAM, 0);
    for (int i=1;;i++) {
        struct lifreq ifr;
        ifr.lifr_index = i;
        if (::ioctl(s, SIOCGIFADDR, &ifr) < 0) {
            break;
        }
        struct sockaddr_in *sin = (struct sockaddr_in *) &ifr.lifr_addr;
        std::string addr(inet_ntoa(sin->sin_addr));
        if (addr != LOCALHOST)
            addrList.push_back(TcpAddress(addr, port));
    }
    if (addrList.empty()) {
        addrList.push_back(TcpAddress(LOCALHOST, port));
    }
    close (s);
}

void SystemInfo::getSystemId(std::string &osName,
                             std::string &nodeName,
                             std::string &release,
                             std::string &version,
                             std::string &machine) {
    struct utsname _uname;
    if (uname (&_uname) == 0) {
        osName = _uname.sysname;
        nodeName = _uname.nodename;
        release = _uname.release;
        version = _uname.version;
        machine = _uname.machine;
    }
}

uint32_t SystemInfo::getProcessId()
{
    return (uint32_t) ::getpid();
}

uint32_t SystemInfo::getParentProcessId()
{
    return (uint32_t) ::getppid();
}

string SystemInfo::getProcessName()
{
    psinfo processInfo;
    char procfile[PATH_MAX];
    int fd;
    string value;

    snprintf(procfile, PATH_MAX, "/proc/%d/psinfo", getProcessId());
    if ((fd = open(procfile, O_RDONLY)) >= 0) {
        if (read(fd, (void *) &processInfo, sizeof(processInfo)) == sizeof(processInfo)) {
            value = processInfo.pr_fname;
        }
    }
    return value;
}

}} // namespace qpid::sys
