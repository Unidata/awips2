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
#include "qpid/client/amqp0_10/AddressResolution.h"
#include "qpid/client/amqp0_10/Codecs.h"
#include "qpid/client/amqp0_10/CodecsInternal.h"
#include "qpid/client/amqp0_10/MessageSource.h"
#include "qpid/client/amqp0_10/MessageSink.h"
#include "qpid/client/amqp0_10/OutgoingMessage.h"
#include "qpid/messaging/Address.h"
#include "qpid/messaging/Message.h"
#include "qpid/messaging/Variant.h"
#include "qpid/Exception.h"
#include "qpid/log/Statement.h"
#include "qpid/framing/enum.h"
#include "qpid/framing/ExchangeBoundResult.h"
#include "qpid/framing/ExchangeQueryResult.h"
#include "qpid/framing/FieldTable.h"
#include "qpid/framing/QueueQueryResult.h"
#include "qpid/framing/ReplyTo.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/framing/Uuid.h"
#include <boost/assign.hpp>
#include <boost/format.hpp>

namespace qpid {
namespace client {
namespace amqp0_10 {

using qpid::Exception;
using qpid::messaging::Address;
using qpid::messaging::InvalidAddress;
using qpid::messaging::Variant;
using qpid::framing::ExchangeBoundResult;
using qpid::framing::ExchangeQueryResult;
using qpid::framing::FieldTable;
using qpid::framing::QueueQueryResult;
using qpid::framing::ReplyTo;
using qpid::framing::Uuid;
using namespace qpid::framing::message;
using namespace boost::assign;


namespace{
const Variant EMPTY_VARIANT;
const FieldTable EMPTY_FIELD_TABLE;
const std::string EMPTY_STRING;

//option names
const std::string BROWSE("browse");
const std::string EXCLUSIVE("exclusive");
const std::string NO_LOCAL("no-local");
const std::string FILTER("filter");
const std::string RELIABILITY("reliability");
const std::string NAME("subscription-name");
const std::string NODE_PROPERTIES("node-properties");
const std::string X_PROPERTIES("x-properties");

//policy types
const std::string CREATE("create");
const std::string ASSERT("assert");
const std::string DELETE("delete");
//policy values
const std::string ALWAYS("always");
const std::string NEVER("never");
const std::string RECEIVER("receiver");
const std::string SENDER("sender");

const std::string QUEUE_ADDRESS("queue");
const std::string TOPIC_ADDRESS("topic");

const std::string UNRELIABLE("unreliable");
const std::string AT_MOST_ONCE("at-most-once");
const std::string AT_LEAST_ONCE("at-least-once");
const std::string EXACTLY_ONCE("exactly-once");
const std::string DURABLE_SUBSCRIPTION("durable");
const std::string DURABLE("durable");

const std::string TOPIC_EXCHANGE("topic");
const std::string FANOUT_EXCHANGE("fanout");
const std::string DIRECT_EXCHANGE("direct");
const std::string HEADERS_EXCHANGE("headers");
const std::string XML_EXCHANGE("xml");
const std::string WILDCARD_ANY("*");
}

//some amqp 0-10 specific options
namespace xamqp{
const std::string AUTO_DELETE("auto-delete");
const std::string EXCHANGE_TYPE("type");
const std::string EXCLUSIVE("exclusive");
const std::string ALTERNATE_EXCHANGE("alternate-exchange");
const std::string QUEUE_ARGUMENTS("x-queue-arguments");
const std::string SUBSCRIBE_ARGUMENTS("x-subscribe-arguments");
}

class Node
{
  protected:
    enum CheckMode {FOR_RECEIVER, FOR_SENDER};

    Node(const Address& address);

    const std::string name;
    Variant createPolicy;
    Variant assertPolicy;
    Variant deletePolicy;

    static bool enabled(const Variant& policy, CheckMode mode);
    static bool createEnabled(const Address& address, CheckMode mode);
    static void convert(const Variant& option, FieldTable& arguments);
    static std::vector<std::string> RECEIVER_MODES;
    static std::vector<std::string> SENDER_MODES;
};

class Queue : protected Node
{
  public:
    Queue(const Address& address);
  protected:
    void checkCreate(qpid::client::AsyncSession&, CheckMode);
    void checkAssert(qpid::client::AsyncSession&, CheckMode);
    void checkDelete(qpid::client::AsyncSession&, CheckMode);
  private:
    bool durable;
    bool autoDelete;
    bool exclusive;
    std::string alternateExchange;
    FieldTable arguments;    

    void configure(const Address&);
};

class Exchange : protected Node
{
  public:
    Exchange(const Address& address);
  protected:
    void checkCreate(qpid::client::AsyncSession&, CheckMode);
    void checkAssert(qpid::client::AsyncSession&, CheckMode);
    void checkDelete(qpid::client::AsyncSession&, CheckMode);
    const std::string& getDesiredExchangeType() { return type; }

  private:
    std::string type;
    bool typeSpecified;
    bool durable;
    bool autoDelete;
    std::string alternateExchange;
    FieldTable arguments;    

    void configure(const Address&);
};

class QueueSource : public Queue, public MessageSource
{
  public:
    QueueSource(const Address& address);
    void subscribe(qpid::client::AsyncSession& session, const std::string& destination);
    void cancel(qpid::client::AsyncSession& session, const std::string& destination);
  private:
    const AcceptMode acceptMode;
    const AcquireMode acquireMode;
    bool exclusive;
    FieldTable options;
};

class Subscription : public Exchange, public MessageSource
{
  public:
    Subscription(const Address&, const std::string& exchangeType="");
    void subscribe(qpid::client::AsyncSession& session, const std::string& destination);
    void cancel(qpid::client::AsyncSession& session, const std::string& destination);
  private:
    struct Binding
    {
        Binding(const std::string& exchange, const std::string& key, const FieldTable& options = EMPTY_FIELD_TABLE);

        std::string exchange;
        std::string key;
        FieldTable options;
    };

    typedef std::vector<Binding> Bindings;

    const std::string queue;
    const bool reliable;
    const bool durable;
    FieldTable queueOptions;
    FieldTable subscriptionOptions;
    Bindings bindings;
    
    void bindSpecial(const std::string& exchangeType);
    void bind(const std::string& subject);
    void bind(const std::string& subject, const Variant& filter);
    void bind(const std::string& subject, const Variant::Map& filter);
    void bind(const std::string& subject, const Variant::List& filter);
    void add(const std::string& exchange, const std::string& key, const FieldTable& options = EMPTY_FIELD_TABLE);
    static std::string getSubscriptionName(const std::string& base, const Variant& name);
};

class ExchangeSink : public Exchange, public MessageSink
{
  public:
    ExchangeSink(const Address& name);
    void declare(qpid::client::AsyncSession& session, const std::string& name);
    void send(qpid::client::AsyncSession& session, const std::string& name, OutgoingMessage& message);
    void cancel(qpid::client::AsyncSession& session, const std::string& name);
  private:
};

class QueueSink : public Queue, public MessageSink
{
  public:
    QueueSink(const Address& name);
    void declare(qpid::client::AsyncSession& session, const std::string& name);
    void send(qpid::client::AsyncSession& session, const std::string& name, OutgoingMessage& message);
    void cancel(qpid::client::AsyncSession& session, const std::string& name);
  private:
};


bool isQueue(qpid::client::Session session, const qpid::messaging::Address& address);
bool isTopic(qpid::client::Session session, const qpid::messaging::Address& address);

bool in(const Variant& value, const std::vector<std::string>& choices)
{
    if (!value.isVoid()) {
        for (std::vector<std::string>::const_iterator i = choices.begin(); i != choices.end(); ++i) {
            if (value.asString() == *i) return true;
        }
    }
    return false;
}

bool getReceiverPolicy(const Address& address, const std::string& key)
{
    return in(address.getOption(key), list_of<std::string>(ALWAYS)(RECEIVER));
}

bool getSenderPolicy(const Address& address, const std::string& key)
{
    return in(address.getOption(key), list_of<std::string>(ALWAYS)(SENDER));
}

bool is_unreliable(const Address& address)
{
    return in(address.getOption(RELIABILITY), list_of<std::string>(UNRELIABLE)(AT_MOST_ONCE));
}

bool is_reliable(const Address& address)
{
    return in(address.getOption(RELIABILITY), list_of<std::string>(AT_LEAST_ONCE)(EXACTLY_ONCE));
}

std::string checkAddressType(qpid::client::Session session, const Address& address)
{
    std::string type = address.getType();
    if (type.empty()) {
        ExchangeBoundResult result = session.exchangeBound(arg::exchange=address.getName(), arg::queue=address.getName());
        if (result.getQueueNotFound() && result.getExchangeNotFound()) {
            //neither a queue nor an exchange exists with that name; treat it as a queue
            type = QUEUE_ADDRESS;
        } else if (result.getExchangeNotFound()) {
            //name refers to a queue
            type = QUEUE_ADDRESS;
        } else if (result.getQueueNotFound()) {
            //name refers to an exchange
            type = TOPIC_ADDRESS;
        } else {
            //both a queue and exchange exist for that name
            throw InvalidAddress("Ambiguous address, please specify queue or topic as node type");
        }
    }
    return type;
}

std::auto_ptr<MessageSource> AddressResolution::resolveSource(qpid::client::Session session,
                                                              const Address& address)
{
    std::string type = checkAddressType(session, address);
    if (type == TOPIC_ADDRESS) {
        std::auto_ptr<MessageSource> source(new Subscription(address));
        QPID_LOG(debug, "treating source address as topic: " << address);
        return source;
    } else if (type == QUEUE_ADDRESS) {
        std::auto_ptr<MessageSource> source(new QueueSource(address));
        QPID_LOG(debug, "treating source address as queue: " << address);
        return source;
    } else {
        throw InvalidAddress("Unrecognised type: " + type);
    }
}


std::auto_ptr<MessageSink> AddressResolution::resolveSink(qpid::client::Session session,
                                                          const qpid::messaging::Address& address)
{
    std::string type = checkAddressType(session, address);
    if (type == TOPIC_ADDRESS) {
        std::auto_ptr<MessageSink> sink(new ExchangeSink(address));
        QPID_LOG(debug, "treating target address as topic: " << address);
        return sink;
    } else if (type == QUEUE_ADDRESS) {
        std::auto_ptr<MessageSink> sink(new QueueSink(address));
        QPID_LOG(debug, "treating target address as queue: " << address);
        return sink;
    } else {
        throw InvalidAddress("Unrecognised type: " + type);
    }
}

const Variant& getNestedOption(const Variant::Map& options, const std::vector<std::string>& keys, size_t index = 0)
{
    Variant::Map::const_iterator i = options.find(keys[index]);
    if (i == options.end()) {
        return EMPTY_VARIANT;
    } else if (index+1 < keys.size()) {
        return getNestedOption(i->second.asMap(), keys, index+1);
    } else {
        return i->second;
    }
}

QueueSource::QueueSource(const Address& address) :
    Queue(address),
    acceptMode(is_unreliable(address) ? ACCEPT_MODE_NONE : ACCEPT_MODE_EXPLICIT),
    acquireMode(address.getOption(BROWSE).asBool() ? ACQUIRE_MODE_NOT_ACQUIRED : ACQUIRE_MODE_PRE_ACQUIRED),
    exclusive(false)
{
    //extract subscription arguments from address options
    const Variant& x = address.getOption(X_PROPERTIES);
    if (!x.isVoid()) {
        const Variant::Map& xProps = x.asMap();
        Variant::Map passthrough;
        for (Variant::Map::const_iterator i = xProps.begin(); i != xProps.end(); ++i) {
            if (i->first == xamqp::EXCLUSIVE) exclusive = i->second;
            else passthrough[i->first] = i->second;
        }
        translate(passthrough, options);
    }
}

void QueueSource::subscribe(qpid::client::AsyncSession& session, const std::string& destination)
{
    checkCreate(session, FOR_RECEIVER);
    checkAssert(session, FOR_RECEIVER);
    session.messageSubscribe(arg::queue=name, 
                             arg::destination=destination,
                             arg::acceptMode=acceptMode,
                             arg::acquireMode=acquireMode,
                             arg::exclusive=exclusive,
                             arg::arguments=options);
}

void QueueSource::cancel(qpid::client::AsyncSession& session, const std::string& destination)
{
    session.messageCancel(destination);
    checkDelete(session, FOR_RECEIVER);
}

std::string Subscription::getSubscriptionName(const std::string& base, const Variant& name)
{
    if (name.isVoid()) {
        return (boost::format("%1%_%2%") % base % Uuid(true).str()).str();
    } else {
        return (boost::format("%1%_%2%") % base % name.asString()).str();
    }
}

Subscription::Subscription(const Address& address, const std::string& exchangeType)
    : Exchange(address),
      queue(getSubscriptionName(name, address.getOption(NAME))),
      reliable(is_reliable(address)),
      durable(address.getOption(DURABLE_SUBSCRIPTION).asBool())
{
    if (address.getOption(NO_LOCAL).asBool()) queueOptions.setInt(NO_LOCAL, 1);
    const Variant& x = address.getOption(X_PROPERTIES);
    if (!x.isVoid()) {
        const Variant::Map& xProps = x.asMap();
        Variant::Map passthrough;
        for (Variant::Map::const_iterator i = xProps.begin(); i != xProps.end(); ++i) {
            if (i->first == xamqp::QUEUE_ARGUMENTS) convert(i->second.asMap(), queueOptions);
            else passthrough[i->first] = i->second;
        }
        translate(passthrough, subscriptionOptions);
    }

    const Variant& filter = address.getOption(FILTER);
    if (!filter.isVoid()) {
        bind(address.getSubject(), filter);
    } else if (address.hasSubject()) {
        //Note: This will not work for headers- or xml- exchange;
        //fanout exchange will do no filtering.
        //TODO: for headers- or xml- exchange can construct a match
        //for the subject in the application-headers
        bind(address.getSubject());
    } else {
        //Neither a subject nor a filter has been defined, treat this
        //as wanting to match all messages (Note: direct exchange is
        //currently unable to support this case).
        if (!exchangeType.empty()) bindSpecial(exchangeType);
        else if (!getDesiredExchangeType().empty()) bindSpecial(getDesiredExchangeType());
    }
}

void Subscription::add(const std::string& exchange, const std::string& key, const FieldTable& options)
{
    bindings.push_back(Binding(exchange, key, options));
}

void Subscription::subscribe(qpid::client::AsyncSession& session, const std::string& destination)
{
    //create exchange if required and specified by policy:
    checkCreate(session, FOR_RECEIVER);
    checkAssert(session, FOR_RECEIVER);

    //create subscription queue:
    session.queueDeclare(arg::queue=queue, arg::exclusive=true, 
                         arg::autoDelete=!reliable, arg::durable=durable, arg::arguments=queueOptions);
    //bind subscription queue to exchange:
    for (Bindings::const_iterator i = bindings.begin(); i != bindings.end(); ++i) {
        session.exchangeBind(arg::queue=queue, arg::exchange=i->exchange, arg::bindingKey=i->key, arg::arguments=i->options);
    }
    //subscribe to subscription queue:
    AcceptMode accept = reliable ? ACCEPT_MODE_EXPLICIT : ACCEPT_MODE_NONE;
    session.messageSubscribe(arg::queue=queue, arg::destination=destination, 
                             arg::exclusive=true, arg::acceptMode=accept, arg::arguments=subscriptionOptions);
}

void Subscription::cancel(qpid::client::AsyncSession& session, const std::string& destination)
{
    session.messageCancel(destination);
    session.queueDelete(arg::queue=queue);
    checkDelete(session, FOR_RECEIVER);
}

Subscription::Binding::Binding(const std::string& e, const std::string& k, const FieldTable& o):
    exchange(e), key(k), options(o) {}

ExchangeSink::ExchangeSink(const Address& address) : Exchange(address) {}

void ExchangeSink::declare(qpid::client::AsyncSession& session, const std::string&)
{
    checkCreate(session, FOR_SENDER);
    checkAssert(session, FOR_SENDER);
}

void ExchangeSink::send(qpid::client::AsyncSession& session, const std::string&, OutgoingMessage& m)
{
    m.message.getDeliveryProperties().setRoutingKey(m.getSubject());
    m.status = session.messageTransfer(arg::destination=name, arg::content=m.message);
}

void ExchangeSink::cancel(qpid::client::AsyncSession& session, const std::string&)
{
    checkDelete(session, FOR_SENDER);    
}

QueueSink::QueueSink(const Address& address) : Queue(address) {}

void QueueSink::declare(qpid::client::AsyncSession& session, const std::string&)
{
    checkCreate(session, FOR_SENDER);
    checkAssert(session, FOR_SENDER);
}
void QueueSink::send(qpid::client::AsyncSession& session, const std::string&, OutgoingMessage& m)
{
    m.message.getDeliveryProperties().setRoutingKey(name);
    m.status = session.messageTransfer(arg::content=m.message);
}

void QueueSink::cancel(qpid::client::AsyncSession& session, const std::string&)
{
    checkDelete(session, FOR_SENDER);
}

Address AddressResolution::convert(const qpid::framing::ReplyTo& rt)
{
    Address address;
    if (rt.getExchange().empty()) {//if default exchange, treat as queue
        address.setName(rt.getRoutingKey());
        address.setType(QUEUE_ADDRESS);
    } else {
        address.setName(rt.getExchange());
        address.setSubject(rt.getRoutingKey());
        address.setType(TOPIC_ADDRESS);
    }
    return address;
}

qpid::framing::ReplyTo AddressResolution::convert(const Address& address)
{
    if (address.getType() == QUEUE_ADDRESS || address.getType().empty()) {
        return ReplyTo(EMPTY_STRING, address.getName());
    } else if (address.getType() == TOPIC_ADDRESS) {
        return ReplyTo(address.getName(), address.getSubject());
    } else {
        QPID_LOG(notice, "Unrecognised type for reply-to: " << address.getType());
        return ReplyTo(EMPTY_STRING, address.getName());//treat as queue
    }
}

bool isQueue(qpid::client::Session session, const qpid::messaging::Address& address) 
{
    return address.getType() == QUEUE_ADDRESS || 
        (address.getType().empty() && session.queueQuery(address.getName()).getQueue() == address.getName());
}

bool isTopic(qpid::client::Session session, const qpid::messaging::Address& address)
{
    if (address.getType().empty()) {
        return !session.exchangeQuery(address.getName()).getNotFound();
    } else if (address.getType() == TOPIC_ADDRESS) {
        return true;
    } else {
        return false;
    }
}

void Subscription::bind(const std::string& subject)
{
    add(name, subject);
}

void Subscription::bind(const std::string& subject, const Variant& filter)
{
    switch (filter.getType()) {
      case qpid::messaging::VAR_MAP:
        bind(subject, filter.asMap());
        break;
      case qpid::messaging::VAR_LIST:
        bind(subject, filter.asList());
        break;
      default:
        //TODO: if both subject _and_ filter are specified, combine in
        //some way; for now we just ignore the subject in that case.
        add(name, filter.asString());
        break;
    }
}

void Subscription::bind(const std::string& subject, const Variant::Map& filter)
{
    qpid::framing::FieldTable arguments;
    translate(filter, arguments);
    add(name, subject.empty() ? queue : subject, arguments);
}

void Subscription::bind(const std::string& subject, const Variant::List& filter)
{
    for (Variant::List::const_iterator i = filter.begin(); i != filter.end(); ++i) {
        bind(subject, *i);
    }
}

void Subscription::bindSpecial(const std::string& exchangeType)
{
    if (exchangeType == TOPIC_EXCHANGE) {
        add(name, WILDCARD_ANY);        
    } else if (exchangeType == FANOUT_EXCHANGE) {
        add(name, queue);        
    } else if (exchangeType == HEADERS_EXCHANGE) {
        //TODO: add special binding for headers exchange to match all messages
    } else if (exchangeType == XML_EXCHANGE) {
        //TODO: add special binding for xml exchange to match all messages
    } else { //E.g. direct
        throw qpid::Exception(QPID_MSG("Cannot create binding to match all messages for exchange of type " << exchangeType));
    }
}

Node::Node(const Address& address) : name(address.getName()),
                                     createPolicy(address.getOption(CREATE)),
                                     assertPolicy(address.getOption(ASSERT)),
                                     deletePolicy(address.getOption(DELETE)) {}

Queue::Queue(const Address& a) : Node(a),
                                 durable(false),
                                 autoDelete(false),
                                 exclusive(false)
{
    configure(a);
}

void Queue::checkCreate(qpid::client::AsyncSession& session, CheckMode mode)
{
    if (enabled(createPolicy, mode)) {
        QPID_LOG(debug, "Auto-creating queue '" << name << "'");
        try {            
            sync(session).queueDeclare(arg::queue=name,
                                       arg::durable=durable,
                                       arg::autoDelete=autoDelete,
                                       arg::exclusive=exclusive,
                                       arg::alternateExchange=alternateExchange,
                                       arg::arguments=arguments);
        } catch (const qpid::Exception& e) {
            throw InvalidAddress((boost::format("Could not create queue %1%; %2%") % name % e.what()).str());
        }
    } else {
        try {
            sync(session).queueDeclare(arg::queue=name, arg::passive=true);
        } catch (const qpid::framing::NotFoundException& /*e*/) {
            throw InvalidAddress((boost::format("Queue %1% does not exist") % name).str());
        } catch (const std::exception& e) {
            throw InvalidAddress(e.what());
        }
    }
}

void Queue::checkDelete(qpid::client::AsyncSession& session, CheckMode mode)
{
    //Note: queue-delete will cause a session exception if the queue
    //does not exist, the query here prevents obvious cases of this
    //but there is a race whenever two deletions are made concurrently
    //so careful use of the delete policy is recommended at present
    if (enabled(deletePolicy, mode) && sync(session).queueQuery(name).getQueue() == name) {
        QPID_LOG(debug, "Auto-deleting queue '" << name << "'");
        sync(session).queueDelete(arg::queue=name);
    }
}

void Queue::checkAssert(qpid::client::AsyncSession& session, CheckMode mode)
{
    if (enabled(assertPolicy, mode)) {
        QueueQueryResult result = sync(session).queueQuery(name);
        if (result.getQueue() != name) {
            throw InvalidAddress((boost::format("Queue not found: %1%") % name).str());
        } else {
            if (durable && !result.getDurable()) {
                throw InvalidAddress((boost::format("Queue not durable: %1%") % name).str());
            }
            if (autoDelete && !result.getAutoDelete()) {
                throw InvalidAddress((boost::format("Queue not set to auto-delete: %1%") % name).str());
            }
            if (exclusive && !result.getExclusive()) {
                throw InvalidAddress((boost::format("Queue not exclusive: %1%") % name).str());
            }
            if (!alternateExchange.empty() && result.getAlternateExchange() != alternateExchange) {
                throw InvalidAddress((boost::format("Alternate exchange does not match for %1%, expected %2%, got %3%") 
                                      % name % alternateExchange % result.getAlternateExchange()).str());
            }
            for (FieldTable::ValueMap::const_iterator i = arguments.begin(); i != arguments.end(); ++i) {
                FieldTable::ValuePtr v = result.getArguments().get(i->first);
                if (!v) {
                    throw InvalidAddress((boost::format("Option %1% not set for %2%") % i->first % name).str());
                } else if (*i->second != *v) {
                    throw InvalidAddress((boost::format("Option %1% does not match for %2%, expected %3%, got %4%")
                                          % i->first % name % *(i->second) % *v).str());
                }
            }
        }
    }
}

void Queue::configure(const Address& address)
{
    const Variant& v = address.getOption(NODE_PROPERTIES);
    if (!v.isVoid()) {
        Variant::Map nodeProps = v.asMap();
        durable = nodeProps[DURABLE];
        Variant::Map::const_iterator x = nodeProps.find(X_PROPERTIES);
        if (x != nodeProps.end()) {
            const Variant::Map& xProps = x->second.asMap();
            Variant::Map passthrough;
            for (Variant::Map::const_iterator i = xProps.begin(); i != xProps.end(); ++i) {
                if (i->first == xamqp::AUTO_DELETE) autoDelete = i->second;
                else if (i->first == xamqp::EXCLUSIVE) exclusive = i->second;
                else if (i->first == xamqp::ALTERNATE_EXCHANGE) alternateExchange = i->second.asString();
                else passthrough[i->first] = i->second;
            }
            translate(passthrough, arguments);
        }
    }
}

Exchange::Exchange(const Address& a) : Node(a),
                                       type(TOPIC_EXCHANGE),
                                       typeSpecified(false),
                                       durable(false),
                                       autoDelete(false)
{
    configure(a);
}

void Exchange::checkCreate(qpid::client::AsyncSession& session, CheckMode mode)
{
    if (enabled(createPolicy, mode)) {
        try {
            sync(session).exchangeDeclare(arg::exchange=name,
                                      arg::type=type,
                                      arg::durable=durable,
                                      arg::autoDelete=autoDelete,
                                      arg::alternateExchange=alternateExchange,
                                      arg::arguments=arguments);
        } catch (const qpid::Exception& e) {
            throw InvalidAddress((boost::format("Could not create exchange %1%; %2%") % name % e.what()).str());
        }
    } else {
        try {
            sync(session).exchangeDeclare(arg::exchange=name, arg::passive=true);
        } catch (const qpid::framing::NotFoundException& /*e*/) {
            throw InvalidAddress((boost::format("Exchange %1% does not exist") % name).str());
        } catch (const std::exception& e) {
            throw InvalidAddress(e.what());
        }
    }
}

void Exchange::checkDelete(qpid::client::AsyncSession& session, CheckMode mode)
{
    //Note: exchange-delete will cause a session exception if the
    //exchange does not exist, the query here prevents obvious cases
    //of this but there is a race whenever two deletions are made
    //concurrently so careful use of the delete policy is recommended
    //at present
    if (enabled(deletePolicy, mode) && !sync(session).exchangeQuery(name).getNotFound()) {
        sync(session).exchangeDelete(arg::exchange=name);
    }
}

void Exchange::checkAssert(qpid::client::AsyncSession& session, CheckMode mode)
{
    if (enabled(assertPolicy, mode)) {
        ExchangeQueryResult result = sync(session).exchangeQuery(name);
        if (result.getNotFound()) {
            throw InvalidAddress((boost::format("Exchange not found: %1%") % name).str());
        } else {
            if (typeSpecified && result.getType() != type) {
                throw InvalidAddress((boost::format("Exchange %1% is of incorrect type, expected %2% but got %3%") 
                                      % name % type % result.getType()).str());
            }
            if (durable && !result.getDurable()) {
                throw InvalidAddress((boost::format("Exchange not durable: %1%") % name).str());
            }
            //Note: Can't check auto-delete or alternate-exchange via
            //exchange-query-result as these are not returned
            //TODO: could use a passive declare to check alternate-exchange
            for (FieldTable::ValueMap::const_iterator i = arguments.begin(); i != arguments.end(); ++i) {
                FieldTable::ValuePtr v = result.getArguments().get(i->first);
                if (!v) {
                    throw InvalidAddress((boost::format("Option %1% not set for %2%") % i->first % name).str());
                } else if (i->second != v) {
                    throw InvalidAddress((boost::format("Option %1% does not match for %2%, expected %3%, got %4%")
                                          % i->first % name % *(i->second) % *v).str());
                }
            }
        }
    }
}

void Exchange::configure(const Address& address)
{
    const Variant& v = address.getOption(NODE_PROPERTIES);
    if (!v.isVoid()) {
        Variant::Map nodeProps = v.asMap();
        durable = nodeProps[DURABLE];
        Variant::Map::const_iterator x = nodeProps.find(X_PROPERTIES);
        if (x != nodeProps.end()) {
            const Variant::Map& xProps = x->second.asMap();
            Variant::Map passthrough;
            for (Variant::Map::const_iterator i = xProps.begin(); i != xProps.end(); ++i) {
                if (i->first == xamqp::AUTO_DELETE) autoDelete = i->second;
                else if (i->first == xamqp::EXCHANGE_TYPE) { type = i->second.asString(); typeSpecified = true; }
                else if (i->first == xamqp::ALTERNATE_EXCHANGE) alternateExchange = i->second.asString();
                else passthrough[i->first] = i->second;
            }
            translate(passthrough, arguments);
        }
    }
}


bool Node::enabled(const Variant& policy, CheckMode mode)
{
    bool result = false;
    switch (mode) {
      case FOR_RECEIVER:
        result = in(policy, RECEIVER_MODES);
        break;
      case FOR_SENDER:
        result = in(policy, SENDER_MODES);
        break;
    }
    return result;
}

bool Node::createEnabled(const Address& address, CheckMode mode)
{
    const Variant& policy = address.getOption(CREATE);
    return enabled(policy, mode);
}

void Node::convert(const Variant& options, FieldTable& arguments)
{
    if (!options.isVoid()) {
        translate(options.asMap(), arguments);
    }    
}
std::vector<std::string> Node::RECEIVER_MODES = list_of<std::string>(ALWAYS) (RECEIVER);
std::vector<std::string> Node::SENDER_MODES = list_of<std::string>(ALWAYS) (SENDER);

}}} // namespace qpid::client::amqp0_10
