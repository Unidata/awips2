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

/** @file helper executable for WatchDogPlugin.cpp */

#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

long timeout;

void killParent(int) {
    ::kill(getppid(), SIGKILL);
    ::fprintf(stderr, "Watchdog killed unresponsive broker, pid=%d\n", ::getppid());
    ::exit(1);
}

void resetTimer(int) {
    struct ::itimerval itval = { { 0, 0 }, { timeout, 0 } };
    if (::setitimer(ITIMER_REAL, &itval, 0) !=0) {
        ::perror("Watchdog failed to set timer");
        killParent(0);
        ::exit(1);
    }
}

/** Simple watchdog program: kill parent process if timeout
 * expires without a SIGUSR1.
 * Will be killed with SIGHUP when parent shuts down.
 * Args: timeout in seconds.
 */
int main(int argc, char** argv) {
    if(argc != 2 || (timeout = atoi(argv[1])) == 0) {
        ::fprintf(stderr, "Usage: %s <timeout_seconds>\n", argv[0]);
        ::exit(1);
    }
    ::signal(SIGUSR1, resetTimer);
    ::signal(SIGALRM, killParent);
    resetTimer(0);
    while (true) { sleep(INT_MAX); }
}
