#ifndef QPID_SYS_WINDOWS_TIME_H
#define QPID_SYS_WINDOWS_TIME_H

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

#include <boost/date_time/posix_time/posix_time_types.hpp>

namespace qpid {
namespace sys {

/**
 * Class to represent an instant in time. Boost has this stuff already done
 * so just reuse it. We can also grab this for quick use with the Condition
 * wait operations.
 */
typedef boost::posix_time::ptime TimePrivate;

}} // namespace qpid::sys

#endif  /*!QPID_SYS_WINDOWS_TIME_H*/
