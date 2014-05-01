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

#include "qpid/client/Completion.h"
#include "qpid/client/CompletionImpl.h"
#include "qpid/client/PrivateImplRef.h"

namespace qpid {
namespace client {

typedef PrivateImplRef<Completion> PI;
Completion::Completion(CompletionImpl* p) { PI::ctor(*this, p); }
Completion::Completion(const Completion& c) : Handle<CompletionImpl>() { PI::copy(*this, c); }
Completion::~Completion() { PI::dtor(*this); }
Completion& Completion::operator=(const Completion& c) { return PI::assign(*this, c); }


void Completion::wait() { impl->wait(); }
bool Completion::isComplete() { return impl->isComplete(); }
std::string Completion::getResult() { return impl->getResult(); }

}} // namespace qpid::client
