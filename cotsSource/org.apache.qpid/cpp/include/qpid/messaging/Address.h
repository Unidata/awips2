#ifndef QPID_MESSAGING_ADDRESS_H
#define QPID_MESSAGING_ADDRESS_H

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
#include <string>
#include "qpid/Exception.h"
#include "qpid/messaging/Variant.h"
#include "qpid/client/ClientImportExport.h"
#include <ostream>

namespace qpid {
namespace messaging {

struct InvalidAddress : public qpid::Exception 
{
    InvalidAddress(const std::string& msg);
};

struct MalformedAddress : public qpid::Exception 
{
    MalformedAddress(const std::string& msg);
};

class AddressImpl;

/**
 * Represents an address to which messages can be sent and from which
 * messages can be received. Often a simple name is sufficient for
 * this, however this can be augmented with a subject pattern and
 * options.
 * 
 * All parts of an address can be specified in a string of the
 * following form:
 * 
 * &lt;address&gt; [ / &lt;subject&gt; ] ; [ { &lt;key&gt; : &lt;value&gt; , ... } ]
 * 
 * Here the &lt;address&gt; is a simple name for the addressed
 * entity and &lt;subject&gt; is a subject or subject pattern for
 * messages sent to or received from this address. The options are
 * specified as a series of key value pairs enclosed in curly brackets
 * (denoting a map). Values can be nested maps, or lists (which are
 * denoted as a comma separated list of values inside square brackets,
 * e.g. [a, b, c]).
 * 
 * The currently supported options are as follows:
 *
 * <table border=0> 
 * 
 * <tr valign=top><td>create</td><td>Indicate whether the address should be
 * automatically created or not. Can be one of <i>always</i>,
 * <i>never</i>, <i>sender</i> or <i>receiver</i>. The properties of
 * the node to be created can be specified via the node-properties
 * option (see below).</td></tr>
 * 
 * <tr valign=top><td>assert</td><td>Indicate whether or not to assert any specified
 * node-properties match the address. Can be one of <i>always</i>,
 * <i>never</i>, <i>sender</i> or <i>receiver</i>.</td></tr>
 * 
 * <tr valign=top><td>delete</td><td>Indicate whether or not to delete the addressed
 * nide when a sender or receiver is cancelled. Can be one of <i>always</i>,
 * <i>never</i>, <i>sender</i> or <i>receiver</i>.</td></tr>
 *
 * <tr valign=top><td>node-properties</td><td>A nested map of properties of the addressed
 * entity or 'node'. These can be used when automatically creating it,
 * or to assert certain properties.
 * 
 * The valid node-properties are:
 * <ul>
 * <li>type - queue or topic</li>
 * 
 * <li>durable - true or false</li>
 * 
 * <li>x-properties - a nested map that can contain implementation or
 * protocol specifiec extedned properties. For the amqp 0-10 mapping,
 * the fields in queue- or exchange- declare can be specified in here;
 * anything that is not recognised as one of those will be passed
 * through in the arguments field.,/li>
 * </ul>
 * </td></tr>
 * 
 * </table>
 * 
 * For receivers there are some further options of interest:
 * 
 * <table border=0 valign=top>
 * 
 * <tr valign=top><td>no-local</td><td>(only relevant for topics at present) specifies that the
 * receiver does not want to receiver messages published to the topic
 * that originate from a sender on the same connection</td></tr>
 *
 * <tr valign=top><td>browse</td><td>(only relevant for queues) specifies that the receiver
 * does not wish to consume the messages, but merely browse them</td></tr>
 * 
 * <tr valign=top><td>durable</td><td>(only relevant for topics at present) specifies that a
 * durable subscription is required</td></tr>
 * 
 * <tr valign=top><td>reliability</td><td>indicates the level of reliability that the receiver
 * expects. Can be one of unreliable, at-most-once, at-least-once or
 * exactly-once (the latter is not yet correctly supported).</td></tr>
 * 
 * <tr valign=top><td>filter</td><td>(only relevant for topics at present) allows bindings to
 * be created for the queue that match the given criteris (or list of
 * criteria).</td></tr>
 * 
 * <tr valign=top><td>x-properties</td><td>allows protocol or implementation specific options
 * to be specified for a receiver; this is a nested map and currently
 * the implementation only recognises two specific nested properties
 * within it (all others are passed through in the arguments of the
 * message-subscribe command):
 * 
 * <ul>
 *     <li>exclusive, which requests an exclusive subscription and
 *     is only relevant for queues</li>
 *
 *     <li>x-queue-arguments, which ais only relevant for topics and
 *     allows arguments to the queue-declare for the subscription
 *     queue to be specified</li>
 * </ul>
 * </td></tr>
 * </table>
 */
class Address
{
  public:
    QPID_CLIENT_EXTERN Address();
    QPID_CLIENT_EXTERN Address(const std::string& address);
    QPID_CLIENT_EXTERN Address(const std::string& name, const std::string& subject,
                               const Variant::Map& options, const std::string& type = "");
    QPID_CLIENT_EXTERN Address(const Address& address);
    QPID_CLIENT_EXTERN ~Address();
    QPID_CLIENT_EXTERN Address& operator=(const Address&);
    QPID_CLIENT_EXTERN const std::string& getName() const;
    QPID_CLIENT_EXTERN void setName(const std::string&);
    QPID_CLIENT_EXTERN const std::string& getSubject() const;
    QPID_CLIENT_EXTERN void setSubject(const std::string&);
    QPID_CLIENT_EXTERN bool hasSubject() const;
    QPID_CLIENT_EXTERN const Variant::Map& getOptions() const;
    QPID_CLIENT_EXTERN Variant::Map& getOptions();
    QPID_CLIENT_EXTERN void setOptions(const Variant::Map&);

    QPID_CLIENT_EXTERN std::string getType() const;
    QPID_CLIENT_EXTERN void setType(const std::string&);

    QPID_CLIENT_EXTERN const Variant& getOption(const std::string& key) const;

    QPID_CLIENT_EXTERN std::string toStr() const;
    QPID_CLIENT_EXTERN operator bool() const;
    QPID_CLIENT_EXTERN bool operator !() const;
  private:
    AddressImpl* impl;
};

QPID_CLIENT_EXTERN std::ostream& operator<<(std::ostream& out, const Address& address);

}} // namespace qpid::messaging

#endif  /*!QPID_MESSAGING_ADDRESS_H*/
