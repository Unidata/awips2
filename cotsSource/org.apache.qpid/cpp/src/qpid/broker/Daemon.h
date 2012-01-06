#ifndef _broker_Daemon_h
#define _broker_Daemon_h

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
#include <boost/scoped_ptr.hpp>
#include <boost/function.hpp>
#include <boost/noncopyable.hpp>
#include <string>


namespace qpid {
namespace broker {

/**
 * Tools for forking and managing a daemon process.
 * NB: Only one Daemon instance is allowed in a process.
 */
class Daemon : private boost::noncopyable
{
  public:
    /** Check daemon is running on port, throw exception if not */
    static pid_t getPid(std::string pidDir, uint16_t port);

    Daemon(std::string pidDir);

    virtual ~Daemon();

    /**
     * Fork a daemon process.
     * Call parent() in the parent process, child() in the child.
     */
    void fork();

  protected:

    /** Called in parent process */
    virtual void parent() = 0;

    /** Called in child process */
    virtual void child() = 0;

    /** Call from parent(): wait for child to indicate it is ready.
     * @timeout in seconds to wait for response.
     * @return port passed by child to ready().
     */
    uint16_t wait(int timeout);

    /** Call from child(): Notify the parent we are ready and write the
     * PID file.
     *@param port returned by parent call to wait().
     */
    void ready(uint16_t port);
    
  private:
    static std::string pidFile(std::string pidDir, uint16_t port);

    pid_t pid;
    int pipeFds[2];
    int lockFileFd;
    std::string lockFile;
    std::string pidDir;
};

}} // namespace qpid::broker

#endif  /*!_broker_Daemon_h*/
