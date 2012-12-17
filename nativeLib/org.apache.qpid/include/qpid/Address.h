#ifndef QPID_ADDRESS_H
#define QPID_ADDRESS_H

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
#include "qpid/CommonImportExport.h"
#include <boost/variant.hpp>
#include <iosfwd>
#include <string>
#include <vector>

namespace qpid {

/** TCP address of a broker - host:port */
struct TcpAddress {
    static const uint16_t DEFAULT_PORT=5672;
    QPID_COMMON_EXTERN explicit TcpAddress(const std::string& host_=std::string(),uint16_t port_=DEFAULT_PORT);
    std::string host;
    uint16_t port;
};
bool operator==(const TcpAddress& x, const TcpAddress& y);
QPID_COMMON_EXTERN std::ostream& operator<<(std::ostream& os, const TcpAddress& a);

/**@internal Not a real address type, this is a placeholder to
 * demonstrate and validate multi-protocol Urls for unit tests and
 * developer education only. An example address holds just a char.
 */
struct ExampleAddress {
    explicit ExampleAddress(char data);
    char data;
};
bool operator==(const ExampleAddress& x, const ExampleAddress& y);
std::ostream& operator<<(std::ostream& os, const ExampleAddress& a);

/**
 * Contains the address of an AMQP broker. Can any supported type of
 * broker address.  Currently only TcpAddress is supported.
 */
struct Address  {
public:
    Address(const Address& a) : value(a.value) {}
    Address(const TcpAddress& tcp) : value(tcp) {}
    Address(const ExampleAddress& eg) : value(eg) {} ///<@internal

    template <class AddressType> Address& operator=(const AddressType& t) { value=t; return *this; }

    /** Get the address of type AddressType.
     *@return AddressType* pointing to the contained address or 0 if
     *contained address is not of type AddressType.
     */
    template <class AddressType> AddressType* get() { return boost::get<AddressType>(&value); }

    /** Get the address of type AddressType.
     *@return AddressType* pointing to the contained address or 0 if
     *contained address is not of type AddressType.
     */
    template <class AddressType> const AddressType* get() const { return boost::get<AddressType>(&value); }

private:
    boost::variant<TcpAddress,ExampleAddress> value;
  friend std::ostream& operator<<(std::ostream& os, const Address& addr);
};



} // namespace qpid

#endif  /*!QPID_ADDRESS_H*/
