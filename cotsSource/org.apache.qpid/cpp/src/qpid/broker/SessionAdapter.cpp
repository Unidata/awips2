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
#include "qpid/broker/SessionAdapter.h"
#include "qpid/broker/Connection.h"
#include "qpid/broker/Queue.h"
#include "qpid/Exception.h"
#include "qpid/framing/reply_exceptions.h"
#include "qpid/framing/enum.h"
#include "qpid/log/Statement.h"
#include "qpid/framing/SequenceSet.h"
#include "qpid/management/ManagementAgent.h"
#include "qmf/org/apache/qpid/broker/EventExchangeDeclare.h"
#include "qmf/org/apache/qpid/broker/EventExchangeDelete.h"
#include "qmf/org/apache/qpid/broker/EventQueueDeclare.h"
#include "qmf/org/apache/qpid/broker/EventQueueDelete.h"
#include "qmf/org/apache/qpid/broker/EventBind.h"
#include "qmf/org/apache/qpid/broker/EventUnbind.h"
#include "qmf/org/apache/qpid/broker/EventSubscribe.h"
#include "qmf/org/apache/qpid/broker/EventUnsubscribe.h"
#include <boost/format.hpp>
#include <boost/cast.hpp>
#include <boost/bind.hpp>

namespace qpid {
namespace broker {

using namespace qpid;
using namespace qpid::framing;
using namespace qpid::framing::dtx;
using namespace qpid::management;
namespace _qmf = qmf::org::apache::qpid::broker;

typedef std::vector<Queue::shared_ptr> QueueVector;

SessionAdapter::SessionAdapter(SemanticState& s) :
    HandlerImpl(s),
    exchangeImpl(s),
    queueImpl(s),
    messageImpl(s),
    executionImpl(s),
    txImpl(s),
    dtxImpl(s)
{}

static const std::string _TRUE("true");
static const std::string _FALSE("false");

void SessionAdapter::ExchangeHandlerImpl::declare(const string& exchange, const string& type, 
                                                  const string& alternateExchange, 
                                                  bool passive, bool durable, bool /*autoDelete*/, const FieldTable& args){

    AclModule* acl = getBroker().getAcl();
    if (acl) {
        std::map<acl::Property, std::string> params;
        params.insert(make_pair(acl::PROP_TYPE, type));
        params.insert(make_pair(acl::PROP_ALTERNATE, alternateExchange));
        params.insert(make_pair(acl::PROP_PASSIVE, std::string(passive ? _TRUE : _FALSE) ));
        params.insert(make_pair(acl::PROP_DURABLE, std::string(durable ? _TRUE : _FALSE)));
        if (!acl->authorise(getConnection().getUserId(),acl::ACT_CREATE,acl::OBJ_EXCHANGE,exchange,&params) )
            throw NotAllowedException(QPID_MSG("ACL denied exchange declare request from " << getConnection().getUserId()));
    }
    
    //TODO: implement autoDelete
    Exchange::shared_ptr alternate;
    if (!alternateExchange.empty()) {
        alternate = getBroker().getExchanges().get(alternateExchange);
    }
    if(passive){
        Exchange::shared_ptr actual(getBroker().getExchanges().get(exchange));
        checkType(actual, type);
        checkAlternate(actual, alternate);
    }else{        
        if(exchange.find("amq.") == 0 || exchange.find("qpid.") == 0) {
            throw framing::NotAllowedException(QPID_MSG("Exchange names beginning with \"amq.\" or \"qpid.\" are reserved. (exchange=\"" << exchange << "\")"));
        }
        try{
            std::pair<Exchange::shared_ptr, bool> response = getBroker().getExchanges().declare(exchange, type, durable, args);
            if (response.second) {
                if (alternate) {
                    response.first->setAlternate(alternate);
                    alternate->incAlternateUsers();
                }
                if (durable) {
                    getBroker().getStore().create(*response.first, args);
                }
            } else {
                checkType(response.first, type);
                checkAlternate(response.first, alternate);
            }

            ManagementAgent* agent = getBroker().getManagementAgent();
            if (agent)
                agent->raiseEvent(_qmf::EventExchangeDeclare(getConnection().getUrl(), getConnection().getUserId(), exchange, type,
                                                             alternateExchange, durable, false, args,
                                                             response.second ? "created" : "existing"));

        }catch(UnknownExchangeTypeException& /*e*/){
            throw CommandInvalidException(QPID_MSG("Exchange type not implemented: " << type));
        }
    }
}

void SessionAdapter::ExchangeHandlerImpl::checkType(Exchange::shared_ptr exchange, const std::string& type)
{
    if (!type.empty() && exchange->getType() != type) {
        throw NotAllowedException(QPID_MSG("Exchange declared to be of type " << exchange->getType() << ", requested " << type));
    }
}

void SessionAdapter::ExchangeHandlerImpl::checkAlternate(Exchange::shared_ptr exchange, Exchange::shared_ptr alternate)
{
    if (alternate && ((exchange->getAlternate() && alternate != exchange->getAlternate())
                      || !exchange->getAlternate()))
        throw NotAllowedException(QPID_MSG("Exchange declared with alternate-exchange "
                                           << (exchange->getAlternate() ? exchange->getAlternate()->getName() : "<nonexistent>")
                                           << ", requested " 
                                           << alternate->getName()));
}
                
void SessionAdapter::ExchangeHandlerImpl::delete_(const string& name, bool /*ifUnused*/)
{
    AclModule* acl = getBroker().getAcl();
    if (acl) {
        if (!acl->authorise(getConnection().getUserId(),acl::ACT_DELETE,acl::OBJ_EXCHANGE,name,NULL) )
            throw NotAllowedException(QPID_MSG("ACL denied exchange delete request from " << getConnection().getUserId()));
    }

    //TODO: implement unused
    Exchange::shared_ptr exchange(getBroker().getExchanges().get(name));
    if (exchange->inUseAsAlternate()) throw NotAllowedException(QPID_MSG("Exchange in use as alternate-exchange."));
    if (exchange->isDurable()) getBroker().getStore().destroy(*exchange);
    if (exchange->getAlternate()) exchange->getAlternate()->decAlternateUsers();
    getBroker().getExchanges().destroy(name);

    ManagementAgent* agent = getBroker().getManagementAgent();
    if (agent)
        agent->raiseEvent(_qmf::EventExchangeDelete(getConnection().getUrl(), getConnection().getUserId(), name));
}

ExchangeQueryResult SessionAdapter::ExchangeHandlerImpl::query(const string& name)
{
    AclModule* acl = getBroker().getAcl();
    if (acl) {
        if (!acl->authorise(getConnection().getUserId(),acl::ACT_ACCESS,acl::OBJ_EXCHANGE,name,NULL) )
            throw NotAllowedException(QPID_MSG("ACL denied exchange query request from " << getConnection().getUserId()));
    }

    try {
        Exchange::shared_ptr exchange(getBroker().getExchanges().get(name));
        return ExchangeQueryResult(exchange->getType(), exchange->isDurable(), false, exchange->getArgs());
    } catch (const NotFoundException& /*e*/) {
        return ExchangeQueryResult("", false, true, FieldTable());        
    }
}

void SessionAdapter::ExchangeHandlerImpl::bind(const string& queueName, 
                                           const string& exchangeName, const string& routingKey, 
                                           const FieldTable& arguments)
{
    AclModule* acl = getBroker().getAcl();
    if (acl) {
        std::map<acl::Property, std::string> params;
        params.insert(make_pair(acl::PROP_QUEUENAME, queueName));
        params.insert(make_pair(acl::PROP_ROUTINGKEY, routingKey));

        if (!acl->authorise(getConnection().getUserId(),acl::ACT_BIND,acl::OBJ_EXCHANGE,exchangeName,&params))
            throw NotAllowedException(QPID_MSG("ACL denied exchange bind request from " << getConnection().getUserId()));
    }

    Queue::shared_ptr queue = getQueue(queueName);
    Exchange::shared_ptr exchange = getBroker().getExchanges().get(exchangeName);
    if(exchange){
        string exchangeRoutingKey = routingKey.empty() && queueName.empty() ? queue->getName() : routingKey;
        if (exchange->bind(queue, exchangeRoutingKey, &arguments)) {
            queue->bound(exchangeName, routingKey, arguments);
            if (exchange->isDurable() && queue->isDurable()) {
                getBroker().getStore().bind(*exchange, *queue, routingKey, arguments);
            }

            ManagementAgent* agent = getBroker().getManagementAgent();
            if (agent)
                agent->raiseEvent(_qmf::EventBind(getConnection().getUrl(), getConnection().getUserId(), exchangeName, queueName, exchangeRoutingKey, arguments));
        }
    }else{
        throw NotFoundException("Bind failed. No such exchange: " + exchangeName);
    }
}
 
void SessionAdapter::ExchangeHandlerImpl::unbind(const string& queueName,
                                                 const string& exchangeName,
                                                 const string& routingKey)
{
    AclModule* acl = getBroker().getAcl();
    if (acl) {
        std::map<acl::Property, std::string> params;
        params.insert(make_pair(acl::PROP_QUEUENAME, queueName));
        params.insert(make_pair(acl::PROP_ROUTINGKEY, routingKey));
        if (!acl->authorise(getConnection().getUserId(),acl::ACT_UNBIND,acl::OBJ_EXCHANGE,exchangeName,&params) )
            throw NotAllowedException(QPID_MSG("ACL denied exchange unbind request from " << getConnection().getUserId()));
    }

    Queue::shared_ptr queue = getQueue(queueName);
    if (!queue.get()) throw NotFoundException("Unbind failed. No such exchange: " + exchangeName);

    Exchange::shared_ptr exchange = getBroker().getExchanges().get(exchangeName);
    if (!exchange.get()) throw NotFoundException("Unbind failed. No such exchange: " + exchangeName);

    //TODO: revise unbind to rely solely on binding key (not args)
    if (exchange->unbind(queue, routingKey, 0)) {
        if (exchange->isDurable() && queue->isDurable())
            getBroker().getStore().unbind(*exchange, *queue, routingKey, FieldTable());

        ManagementAgent* agent = getBroker().getManagementAgent();
        if (agent)
            agent->raiseEvent(_qmf::EventUnbind(getConnection().getUrl(), getConnection().getUserId(), exchangeName, queueName, routingKey));
    }
}

ExchangeBoundResult SessionAdapter::ExchangeHandlerImpl::bound(const std::string& exchangeName,
                                                                  const std::string& queueName,
                                                                  const std::string& key,
                                                                  const framing::FieldTable& args)
{
    AclModule* acl = getBroker().getAcl();
    if (acl) {
        std::map<acl::Property, std::string> params;
        params.insert(make_pair(acl::PROP_QUEUENAME, queueName));
        params.insert(make_pair(acl::PROP_ROUTINGKEY, key));
        if (!acl->authorise(getConnection().getUserId(),acl::ACT_ACCESS,acl::OBJ_EXCHANGE,exchangeName,&params) )
            throw NotAllowedException(QPID_MSG("ACL denied exchange bound request from " << getConnection().getUserId()));
    }
    
    Exchange::shared_ptr exchange;
    try {
        exchange = getBroker().getExchanges().get(exchangeName);
    } catch (const NotFoundException&) {}

    Queue::shared_ptr queue;
    if (!queueName.empty()) {
        queue = getBroker().getQueues().find(queueName);
    }

    if (!exchange) {
        return ExchangeBoundResult(true, (!queueName.empty() && !queue), false, false, false);
    } else if (!queueName.empty() && !queue) {
        return ExchangeBoundResult(false, true, false, false, false);
    } else if (exchange->isBound(queue, key.empty() ? 0 : &key, args.count() > 0 ? &args : &args)) {
        return ExchangeBoundResult(false, false, false, false, false);
    } else {
        //need to test each specified option individually
        bool queueMatched = queueName.empty() || exchange->isBound(queue, 0, 0);
        bool keyMatched = key.empty() || exchange->isBound(Queue::shared_ptr(), &key, 0);
        bool argsMatched = args.count() == 0 || exchange->isBound(Queue::shared_ptr(), 0, &args);

        return ExchangeBoundResult(false, false, !queueMatched, !keyMatched, !argsMatched);
    }
}

SessionAdapter::QueueHandlerImpl::QueueHandlerImpl(SemanticState& session) : HandlerHelper(session), broker(getBroker())
{}


SessionAdapter::QueueHandlerImpl::~QueueHandlerImpl()
{
    try {
        destroyExclusiveQueues();
    } catch (std::exception& e) {
        QPID_LOG(error, e.what());
    }
}

void SessionAdapter::QueueHandlerImpl::destroyExclusiveQueues()
{
    while (!exclusiveQueues.empty()) {
        Queue::shared_ptr q(exclusiveQueues.front());
        q->releaseExclusiveOwnership();
        if (q->canAutoDelete()) {
            Queue::tryAutoDelete(broker, q);
        }
        exclusiveQueues.erase(exclusiveQueues.begin());
    }
}
    
bool SessionAdapter::QueueHandlerImpl::isLocal(const ConnectionToken* t) const 
{ 
    return session.isLocal(t); 
}


QueueQueryResult SessionAdapter::QueueHandlerImpl::query(const string& name)
{
    AclModule* acl = getBroker().getAcl();
    if (acl) {
        if (!acl->authorise(getConnection().getUserId(),acl::ACT_ACCESS,acl::OBJ_QUEUE,name,NULL) )
            throw NotAllowedException(QPID_MSG("ACL denied queue query request from " << getConnection().getUserId()));
    }
    
    Queue::shared_ptr queue = session.getBroker().getQueues().find(name);
    if (queue) {

        Exchange::shared_ptr alternateExchange = queue->getAlternateExchange();
        
        return QueueQueryResult(queue->getName(), 
                                alternateExchange ? alternateExchange->getName() : "", 
                                queue->isDurable(), 
                                queue->hasExclusiveOwner(),
                                queue->isAutoDelete(),
                                queue->getSettings(),
                                queue->getMessageCount(),
                                queue->getConsumerCount());
    } else {
        return QueueQueryResult();
    }
}

void SessionAdapter::QueueHandlerImpl::declare(const string& name, const string& alternateExchange,
                                               bool passive, bool durable, bool exclusive, 
                                               bool autoDelete, const qpid::framing::FieldTable& arguments)
{ 
    AclModule* acl = getBroker().getAcl();
    if (acl) {
        std::map<acl::Property, std::string> params;
        params.insert(make_pair(acl::PROP_ALTERNATE, alternateExchange));
        params.insert(make_pair(acl::PROP_PASSIVE, std::string(passive ? _TRUE : _FALSE) ));
        params.insert(make_pair(acl::PROP_DURABLE, std::string(durable ? _TRUE : _FALSE)));
        params.insert(make_pair(acl::PROP_EXCLUSIVE, std::string(exclusive ? _TRUE : _FALSE)));
        params.insert(make_pair(acl::PROP_AUTODELETE, std::string(autoDelete ? _TRUE : _FALSE)));
        params.insert(make_pair(acl::PROP_POLICYTYPE, arguments.getAsString("qpid.policy_type")));
        params.insert(make_pair(acl::PROP_MAXQUEUECOUNT, boost::lexical_cast<string>(arguments.getAsInt("qpid.max_count"))));
        params.insert(make_pair(acl::PROP_MAXQUEUESIZE, boost::lexical_cast<string>(arguments.getAsInt64("qpid.max_size"))));

        if (!acl->authorise(getConnection().getUserId(),acl::ACT_CREATE,acl::OBJ_QUEUE,name,&params) )
            throw NotAllowedException(QPID_MSG("ACL denied queue create request from " << getConnection().getUserId()));
    }

    Exchange::shared_ptr alternate;
    if (!alternateExchange.empty()) {
        alternate = getBroker().getExchanges().get(alternateExchange);
    }
    Queue::shared_ptr queue;
    if (passive && !name.empty()) {
    queue = getQueue(name);
        //TODO: check alternate-exchange is as expected
    } else {
        std::pair<Queue::shared_ptr, bool> queue_created =  
            getBroker().getQueues().declare(name, durable,
                                            autoDelete,
                                            exclusive ? &session : 0);
        queue = queue_created.first;
        assert(queue);
        if (queue_created.second) { // This is a new queue
            if (alternate) {
                queue->setAlternateExchange(alternate);
                alternate->incAlternateUsers();
            }

            //apply settings & create persistent record if required
            queue_created.first->create(arguments);

            //add default binding:
            getBroker().getExchanges().getDefault()->bind(queue, name, 0);
            queue->bound(getBroker().getExchanges().getDefault()->getName(), name, arguments);

            //handle automatic cleanup:
            if (exclusive) {
                exclusiveQueues.push_back(queue);
            }
        } else {
            if (exclusive && queue->setExclusiveOwner(&session)) {
                exclusiveQueues.push_back(queue);
            }
        }

        ManagementAgent* agent = getBroker().getManagementAgent();
        if (agent)
            agent->raiseEvent(_qmf::EventQueueDeclare(getConnection().getUrl(), getConnection().getUserId(),
                                                      name, durable, exclusive, autoDelete, arguments,
                                                      queue_created.second ? "created" : "existing"));
    }

    if (exclusive && !queue->isExclusiveOwner(&session)) 
        throw ResourceLockedException(QPID_MSG("Cannot grant exclusive access to queue "
                                               << queue->getName()));
} 
        
        
void SessionAdapter::QueueHandlerImpl::purge(const string& queue){
    AclModule* acl = getBroker().getAcl();
    if (acl)
    {
         if (!acl->authorise(getConnection().getUserId(),acl::ACT_PURGE,acl::OBJ_QUEUE,queue,NULL) )
             throw NotAllowedException(QPID_MSG("ACL denied queue purge request from " << getConnection().getUserId()));
    }
    getQueue(queue)->purge();
} 
        
void SessionAdapter::QueueHandlerImpl::delete_(const string& queue, bool ifUnused, bool ifEmpty){

    AclModule* acl = getBroker().getAcl();
    if (acl)
    {
         if (!acl->authorise(getConnection().getUserId(),acl::ACT_DELETE,acl::OBJ_QUEUE,queue,NULL) )
             throw NotAllowedException(QPID_MSG("ACL denied queue delete request from " << getConnection().getUserId()));
    }

    Queue::shared_ptr q = getQueue(queue);
    if (q->hasExclusiveOwner() && !q->isExclusiveOwner(&session)) 
        throw ResourceLockedException(QPID_MSG("Cannot delete queue "
                                               << queue << "; it is exclusive to another session"));
    if(ifEmpty && q->getMessageCount() > 0){
        throw PreconditionFailedException("Queue not empty.");
    }else if(ifUnused && q->getConsumerCount() > 0){
        throw PreconditionFailedException("Queue in use.");
    }else{
        //remove the queue from the list of exclusive queues if necessary
        if(q->isExclusiveOwner(&getConnection())){
            QueueVector::iterator i = std::find(getConnection().exclusiveQueues.begin(), getConnection().exclusiveQueues.end(), q);
            if(i < getConnection().exclusiveQueues.end()) getConnection().exclusiveQueues.erase(i);
        }
        q->destroy();
        getBroker().getQueues().destroy(queue);
        q->unbind(getBroker().getExchanges(), q);

        ManagementAgent* agent = getBroker().getManagementAgent();
        if (agent)
            agent->raiseEvent(_qmf::EventQueueDelete(getConnection().getUrl(), getConnection().getUserId(), queue));
    }
} 

SessionAdapter::MessageHandlerImpl::MessageHandlerImpl(SemanticState& s) : 
    HandlerHelper(s),
    releaseRedeliveredOp(boost::bind(&SemanticState::release, &state, _1, _2, true)),
    releaseOp(boost::bind(&SemanticState::release, &state, _1, _2, false)),
    rejectOp(boost::bind(&SemanticState::reject, &state, _1, _2))
 {}

//
// Message class method handlers
//

void SessionAdapter::MessageHandlerImpl::transfer(const string& /*destination*/,
                                  uint8_t /*acceptMode*/,
                                  uint8_t /*acquireMode*/)
{
    //not yet used (content containing assemblies treated differently at present
    std::cout << "SessionAdapter::MessageHandlerImpl::transfer() called" << std::endl;
}

void SessionAdapter::MessageHandlerImpl::release(const SequenceSet& transfers, bool setRedelivered)
{
    transfers.for_each(setRedelivered ? releaseRedeliveredOp : releaseOp);
}

void
SessionAdapter::MessageHandlerImpl::subscribe(const string& queueName,
                                              const string& destination,
                                              uint8_t acceptMode,
                                              uint8_t acquireMode,
                                              bool exclusive,
                                              const string& resumeId,
                                              uint64_t resumeTtl,
                                              const FieldTable& arguments)
{

    AclModule* acl = getBroker().getAcl();
    if (acl)
    {        
         if (!acl->authorise(getConnection().getUserId(),acl::ACT_CONSUME,acl::OBJ_QUEUE,queueName,NULL) )
             throw NotAllowedException(QPID_MSG("ACL denied Queue subscribe request from " << getConnection().getUserId()));
    }

    Queue::shared_ptr queue = getQueue(queueName);
    if(!destination.empty() && state.exists(destination))
        throw NotAllowedException(QPID_MSG("Consumer tags must be unique"));
    if (queue->hasExclusiveOwner() && !queue->isExclusiveOwner(&session)) 
        throw ResourceLockedException(QPID_MSG("Cannot subscribe to exclusive queue "
                                               << queue->getName()));

    state.consume(destination, queue, 
                  acceptMode == 0, acquireMode == 0, exclusive, 
                  resumeId, resumeTtl, arguments);

    ManagementAgent* agent = getBroker().getManagementAgent();
    if (agent)
        agent->raiseEvent(_qmf::EventSubscribe(getConnection().getUrl(), getConnection().getUserId(),
                                               queueName, destination, exclusive, arguments));
}

void
SessionAdapter::MessageHandlerImpl::cancel(const string& destination )
{
    state.cancel(destination);

    ManagementAgent* agent = getBroker().getManagementAgent();
    if (agent)
        agent->raiseEvent(_qmf::EventUnsubscribe(getConnection().getUrl(), getConnection().getUserId(), destination));
}

void
SessionAdapter::MessageHandlerImpl::reject(const SequenceSet& transfers, uint16_t /*code*/, const string& /*text*/ )
{
    transfers.for_each(rejectOp);
}

void SessionAdapter::MessageHandlerImpl::flow(const std::string& destination, uint8_t unit, uint32_t value)
{
    if (unit == 0) {
        //message
        state.addMessageCredit(destination, value);
    } else if (unit == 1) {
        //bytes
        state.addByteCredit(destination, value);
    } else {
        //unknown
        throw InvalidArgumentException(QPID_MSG("Invalid value for unit " << unit));
    }
    
}
    
void SessionAdapter::MessageHandlerImpl::setFlowMode(const std::string& destination, uint8_t mode)
{
    if (mode == 0) {
        //credit
        state.setCreditMode(destination);
    } else if (mode == 1) {
        //window
        state.setWindowMode(destination);
    } else{
        throw InvalidArgumentException(QPID_MSG("Invalid value for mode " << mode));        
    }
}
    
void SessionAdapter::MessageHandlerImpl::flush(const std::string& destination)
{
    state.flush(destination);        
}

void SessionAdapter::MessageHandlerImpl::stop(const std::string& destination)
{
    state.stop(destination);        
}

void SessionAdapter::MessageHandlerImpl::accept(const framing::SequenceSet& commands)
{
    state.accepted(commands);
}

framing::MessageAcquireResult SessionAdapter::MessageHandlerImpl::acquire(const framing::SequenceSet& transfers)
{
    // FIXME aconway 2008-05-12: create SequenceSet directly, no need for intermediate results vector.
    SequenceNumberSet results;
    RangedOperation f = boost::bind(&SemanticState::acquire, &state, _1, _2, boost::ref(results));
    transfers.for_each(f);

    results = results.condense();
    SequenceSet acquisitions;
    RangedOperation g = boost::bind(&SequenceSet::add, &acquisitions, _1, _2);
    results.processRanges(g);

    return MessageAcquireResult(acquisitions);
}

framing::MessageResumeResult SessionAdapter::MessageHandlerImpl::resume(const std::string& /*destination*/,
                                                                        const std::string& /*resumeId*/)
{
    throw NotImplementedException("resuming transfers not yet supported");
}
    


void SessionAdapter::ExecutionHandlerImpl::sync() {} //essentially a no-op

void SessionAdapter::ExecutionHandlerImpl::result(const SequenceNumber& /*commandId*/, const string& /*value*/)
{
    //TODO: but currently never used client->server
}

void SessionAdapter::ExecutionHandlerImpl::exception(uint16_t /*errorCode*/,
                                                     const SequenceNumber& /*commandId*/,
                                                     uint8_t /*classCode*/,
                                                     uint8_t /*commandCode*/,
                                                     uint8_t /*fieldIndex*/,
                                                     const std::string& /*description*/,
                                                     const framing::FieldTable& /*errorInfo*/)
{
    //TODO: again, not really used client->server but may be important
    //for inter-broker links
}



void SessionAdapter::TxHandlerImpl::select()
{
    state.startTx();
}

void SessionAdapter::TxHandlerImpl::commit()
{
    state.commit(&getBroker().getStore());
}

void SessionAdapter::TxHandlerImpl::rollback()
{    
    state.rollback();
}

std::string SessionAdapter::DtxHandlerImpl::convert(const framing::Xid& xid)
{
    std::string encoded;
    encode(xid, encoded);
    return encoded;
}

void SessionAdapter::DtxHandlerImpl::select()
{
    state.selectDtx();
}

XaResult SessionAdapter::DtxHandlerImpl::end(const Xid& xid,
                                                            bool fail,
                                                            bool suspend)
{
    try {
        if (fail) {
            state.endDtx(convert(xid), true);
            if (suspend) {
                throw CommandInvalidException(QPID_MSG("End and suspend cannot both be set."));
            } else {
                return XaResult(XA_STATUS_XA_RBROLLBACK);
            }
        } else {
            if (suspend) {
                state.suspendDtx(convert(xid));
            } else {
                state.endDtx(convert(xid), false);
            }
            return XaResult(XA_STATUS_XA_OK);
        }
    } catch (const DtxTimeoutException& /*e*/) {
        return XaResult(XA_STATUS_XA_RBTIMEOUT);        
    }
}

XaResult SessionAdapter::DtxHandlerImpl::start(const Xid& xid,
                                                                bool join,
                                                                bool resume)
{
    if (join && resume) {
        throw CommandInvalidException(QPID_MSG("Join and resume cannot both be set."));
    }
    try {
        if (resume) {
            state.resumeDtx(convert(xid));
        } else {
            state.startDtx(convert(xid), getBroker().getDtxManager(), join);
        }
        return XaResult(XA_STATUS_XA_OK);
    } catch (const DtxTimeoutException& /*e*/) {
        return XaResult(XA_STATUS_XA_RBTIMEOUT);        
    }
}

XaResult SessionAdapter::DtxHandlerImpl::prepare(const Xid& xid)
{
    try {
        bool ok = getBroker().getDtxManager().prepare(convert(xid));
        return XaResult(ok ? XA_STATUS_XA_OK : XA_STATUS_XA_RBROLLBACK);
    } catch (const DtxTimeoutException& /*e*/) {
        return XaResult(XA_STATUS_XA_RBTIMEOUT);        
    }
}

XaResult SessionAdapter::DtxHandlerImpl::commit(const Xid& xid,
                            bool onePhase)
{
    try {
        bool ok = getBroker().getDtxManager().commit(convert(xid), onePhase);
        return XaResult(ok ? XA_STATUS_XA_OK : XA_STATUS_XA_RBROLLBACK);
    } catch (const DtxTimeoutException& /*e*/) {
        return XaResult(XA_STATUS_XA_RBTIMEOUT);        
    }
}


XaResult SessionAdapter::DtxHandlerImpl::rollback(const Xid& xid)
{
    try {
        getBroker().getDtxManager().rollback(convert(xid));
        return XaResult(XA_STATUS_XA_OK);
    } catch (const DtxTimeoutException& /*e*/) {
        return XaResult(XA_STATUS_XA_RBTIMEOUT);        
    }
}

DtxRecoverResult SessionAdapter::DtxHandlerImpl::recover()
{
    std::set<std::string> xids;
    getBroker().getStore().collectPreparedXids(xids);        
    /*
     * create array of long structs
     */
    Array indoubt(0xAB);
    for (std::set<std::string>::iterator i = xids.begin(); i != xids.end(); i++) {
        boost::shared_ptr<FieldValue> xid(new Struct32Value(*i));
        indoubt.add(xid);
    }
    return DtxRecoverResult(indoubt);
}

void SessionAdapter::DtxHandlerImpl::forget(const Xid& xid)
{
    //Currently no heuristic completion is supported, so this should never be used.
    throw NotImplementedException(QPID_MSG("Forget not implemented. Branch with xid "  << xid << " not heuristically completed!"));
}

DtxGetTimeoutResult SessionAdapter::DtxHandlerImpl::getTimeout(const Xid& xid)
{
    uint32_t timeout = getBroker().getDtxManager().getTimeout(convert(xid));
    return DtxGetTimeoutResult(timeout);    
}


void SessionAdapter::DtxHandlerImpl::setTimeout(const Xid& xid,
                                                uint32_t timeout)
{
    getBroker().getDtxManager().setTimeout(convert(xid), timeout);
}


Queue::shared_ptr SessionAdapter::HandlerHelper::getQueue(const string& name) const {
    Queue::shared_ptr queue;
    if (name.empty()) {
        throw framing::IllegalArgumentException(QPID_MSG("No queue name specified."));
    } else {
        queue = session.getBroker().getQueues().find(name);
        if (!queue)
            throw framing::NotFoundException(QPID_MSG("Queue not found: "<<name));
    }
    return queue;
}

}} // namespace qpid::broker


