#ifndef QPID_VERSION_H
#define QPID_VERSION_H

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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

namespace qpid {
#ifdef HAVE_CONFIG_H
  const std::string product  = PACKAGE_NAME;
  const std::string version  = PACKAGE_VERSION;
#  if HAVE_SASL
  const std::string saslName = BROKER_SASL_NAME;
#  else
  const std::string saslName = "qpidd-no-sasl";
#  endif
#else
  const std::string product  = "qpidc";
  const std::string version  = "0.6";
  const std::string saslName = "qpid-broker";
#endif
}

#endif  /*!QPID_VERSION_H*/
