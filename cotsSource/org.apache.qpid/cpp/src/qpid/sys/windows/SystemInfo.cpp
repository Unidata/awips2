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

/* GetNativeSystemInfo call requires _WIN32_WINNT 0x0501 or higher */
#ifndef _WIN32_WINNT
#  define _WIN32_WINNT 0x0501
#endif

#include "qpid/sys/IntegerTypes.h"
#include "qpid/sys/SystemInfo.h"

#include <winsock2.h>
#include <ws2tcpip.h>
#include <windows.h>
#include <tlhelp32.h>

#ifndef HOST_NAME_MAX
#  define HOST_NAME_MAX 256
#endif

namespace qpid {
namespace sys {

long  SystemInfo::concurrency() {
    SYSTEM_INFO sys_info;
    ::GetSystemInfo (&sys_info);
    long activeProcessors = 0;
    DWORD_PTR mask = sys_info.dwActiveProcessorMask;
    while (mask != 0) {
        if (mask & 1)
            ++activeProcessors;
        mask >>= 1;
    }
    return activeProcessors;
}

bool SystemInfo::getLocalHostname (TcpAddress &address) {
    char name[HOST_NAME_MAX];
    if (::gethostname(name, sizeof(name)) != 0) {
        errno = WSAGetLastError();
        return false;
    }
    address.host = name;
    return true;
}

void SystemInfo::getLocalIpAddresses (uint16_t port,
                                      std::vector<Address> &addrList) {
    enum { MAX_URL_INTERFACES = 100 };
    static const std::string LOCALHOST("127.0.0.1");

    SOCKET s = socket (PF_INET, SOCK_STREAM, 0);
    if (s != INVALID_SOCKET) {
        INTERFACE_INFO interfaces[MAX_URL_INTERFACES];
        DWORD filledBytes = 0;
        WSAIoctl (s,
                  SIO_GET_INTERFACE_LIST,
                  0,
                  0,
                  interfaces,
                  sizeof (interfaces),
                  &filledBytes,
                  0,
                  0);
        unsigned int interfaceCount = filledBytes / sizeof (INTERFACE_INFO);
        for (unsigned int i = 0; i < interfaceCount; ++i) {
            if (interfaces[i].iiFlags & IFF_UP) {
                std::string addr(inet_ntoa(interfaces[i].iiAddress.AddressIn.sin_addr));
                if (addr != LOCALHOST)
                    addrList.push_back(TcpAddress(addr, port));
            }
        }
        closesocket (s);
    }
}

void SystemInfo::getSystemId (std::string &osName,
                              std::string &nodeName,
                              std::string &release,
                              std::string &version,
                              std::string &machine)
{
    osName = "Microsoft Windows";

    char node[MAX_COMPUTERNAME_LENGTH + 1];
    DWORD nodelen = MAX_COMPUTERNAME_LENGTH + 1;
    GetComputerName (node, &nodelen);
    nodeName = node;

    OSVERSIONINFOEX vinfo;
    vinfo.dwOSVersionInfoSize = sizeof(vinfo);
    GetVersionEx ((OSVERSIONINFO *)&vinfo);

    SYSTEM_INFO sinfo;
    GetNativeSystemInfo(&sinfo);

    switch(vinfo.dwMajorVersion) {
    case 5:
        switch(vinfo.dwMinorVersion) {
        case 0:
            release  ="2000";
            break;
        case 1:
            release = "XP";
            break;
        case 2:
            if (sinfo.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_AMD64 ||
                sinfo.wProcessorArchitecture == PROCESSOR_ARCHITECTURE_IA64)
                release = "XP-64";
            else
                release = "Server 2003";
            break;
        default:
            release = "Windows";
        }
        break;
    case 6:
        if (vinfo.wProductType == VER_NT_SERVER)
            release = "Server 2008";
        else
            release = "Vista";
        break;
    default:
        release = "Microsoft Windows";
    }
    version = vinfo.szCSDVersion;

    switch(sinfo.wProcessorArchitecture) {
    case PROCESSOR_ARCHITECTURE_AMD64:
        machine = "x86-64";
        break;
    case PROCESSOR_ARCHITECTURE_IA64:
        machine = "IA64";
        break;
    case PROCESSOR_ARCHITECTURE_INTEL:
        machine = "x86";
        break;
    default:
        machine = "unknown";
        break;
    }
}

uint32_t SystemInfo::getProcessId()
{
    return static_cast<uint32_t>(::GetCurrentProcessId());
}

uint32_t SystemInfo::getParentProcessId()
{
    // Only want info for the current process, so ask for something specific.
    // The module info won't be used here but it keeps the snapshot limited to
    // the current process so a search through all processes is not needed.
    HANDLE snap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, 0);
    if (snap == INVALID_HANDLE_VALUE)
        return 0;
    PROCESSENTRY32 entry;
    entry.dwSize = sizeof(entry);
    if (!Process32First(snap, &entry))
        entry.th32ParentProcessID = 0;
    CloseHandle(snap);
    return static_cast<uint32_t>(entry.th32ParentProcessID);
}

std::string SystemInfo::getProcessName()
{
    std::string name;

    // Only want info for the current process, so ask for something specific.
    // The module info won't be used here but it keeps the snapshot limited to
    // the current process so a search through all processes is not needed.
    HANDLE snap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, 0);
    if (snap == INVALID_HANDLE_VALUE)
        return name;
    PROCESSENTRY32 entry;
    entry.dwSize = sizeof(entry);
    if (!Process32First(snap, &entry))
        entry.szExeFile[0] = '\0';
    CloseHandle(snap);
    name = entry.szExeFile;
    return name;
}

}} // namespace qpid::sys
