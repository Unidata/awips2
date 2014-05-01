#ifndef QPID_SYS_POSIX_FORK_H
#define QPID_SYS_POSIX_FORK_H

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

#include <string>
#include <sys/types.h>

namespace qpid {
namespace sys {

/**
 * Fork the process. Call parent() in parent and child() in child.
 */
class Fork {
  public:
    Fork();
    virtual ~Fork();

    /**
     * Fork the process.
     * Calls parent() in the parent process, child() in the child.
     */
    virtual void fork();

  protected:

    /** Called in parent process.
     *@child pid of child process
     */
    virtual void parent(pid_t child) = 0;

    /** Called in child process */
    virtual void child() = 0;
};

/**
 * Like Fork but also allows the child to send a string message
 * or throw an exception to the parent.
 */
class ForkWithMessage : public Fork {
  public:
    ForkWithMessage();
    void fork();

  protected:
    /** Call from parent(): wait for child to send a value or throw exception.
     * @timeout in seconds to wait for response.
     * @return value passed by child to ready(). 
     */
    std::string wait(int timeout);

    /** Call from child(): Send a value to the parent.
     *@param value returned by parent call to wait(). 
     */
    void ready(const std::string& value);

  private:
    int pipeFds[2];
};

}} // namespace qpid::sys



#endif  /*!QPID_SYS_POSIX_FORK_H*/
