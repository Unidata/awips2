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
package org.apache.qpid.server.transport;

import org.apache.qpid.transport.*;
import org.apache.qpid.server.registry.IApplicationRegistry;
import org.apache.qpid.server.registry.ApplicationRegistry;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.exchange.*;
import org.apache.qpid.server.queue.QueueRegistry;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.queue.AMQQueueFactory;
import org.apache.qpid.server.message.MessageTransferMessage;
import org.apache.qpid.server.message.MessageMetaData_0_10;
import org.apache.qpid.server.subscription.Subscription_0_10;
import org.apache.qpid.server.flow.*;
import org.apache.qpid.server.store.MessageStore;
import org.apache.qpid.server.store.StoredMessage;
import org.apache.qpid.server.store.DurableConfigurationStore;
import org.apache.qpid.AMQException;
import org.apache.qpid.AMQUnknownExchangeType;
import org.apache.qpid.framing.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

public class ServerSessionDelegate extends SessionDelegate
{
    private final IApplicationRegistry _appRegistry;

    public ServerSessionDelegate(IApplicationRegistry appRegistry)
    {
        _appRegistry = appRegistry;
    }

    @Override
    public void command(Session session, Method method)
    {
        super.command(session, method);
        if (method.isSync())
        {
            session.flushProcessed();
        }
    }

    @Override
    public void messageAccept(Session session, MessageAccept method)
    {
        ((ServerSession)session).accept(method.getTransfers());
    }



    @Override
    public void messageReject(Session session, MessageReject method)
    {
        ((ServerSession)session).reject(method.getTransfers());
    }

    @Override
    public void messageRelease(Session session, MessageRelease method)
    {
        ((ServerSession)session).release(method.getTransfers());
    }

    @Override
    public void messageAcquire(Session session, MessageAcquire method)
    {
        RangeSet acquiredRanges = ((ServerSession)session).acquire(method.getTransfers());

        Acquired result = new Acquired(acquiredRanges);


        session.executionResult((int) method.getId(), result);


    }

    @Override
    public void messageResume(Session session, MessageResume method)
    {
        super.messageResume(session, method);
    }

    @Override
    public void messageSubscribe(Session session, MessageSubscribe method)
    {

        //TODO - work around broken Python tests
        if(!method.hasAcceptMode())
        {
            method.setAcceptMode(MessageAcceptMode.EXPLICIT);
        }
        if(!method.hasAcquireMode())
        {
            method.setAcquireMode(MessageAcquireMode.PRE_ACQUIRED);

        }

       /* if(!method.hasAcceptMode())
        {
            exception(session,method,ExecutionErrorCode.ILLEGAL_ARGUMENT, "Accept-mode not supplied");
        }
        else if(!method.hasAcquireMode())
        {
            exception(session,method,ExecutionErrorCode.ILLEGAL_ARGUMENT, "Acquire-mode not supplied");
        }
        else */if(!method.hasQueue())
        {
            exception(session,method,ExecutionErrorCode.ILLEGAL_ARGUMENT, "queue not supplied");
        }
        else
        {
            String destination = method.getDestination();

            if(((ServerSession)session).getSubscription(destination)!=null)
            {
                exception(session, method, ExecutionErrorCode.NOT_ALLOWED, "Subscription already exists with destaination: '"+destination+"'");
            }
            else
            {
                String queueName = method.getQueue();
                QueueRegistry queueRegistry = getQueueRegistry(session);


                AMQQueue queue = queueRegistry.getQueue(queueName);

                if(queue == null)
                {
                    exception(session,method,ExecutionErrorCode.NOT_FOUND, "Queue: " + queueName + " not found");
                }
                else if(queue.getPrincipalHolder() != null && queue.getPrincipalHolder() != session)
                {
                    exception(session,method,ExecutionErrorCode.RESOURCE_LOCKED, "Exclusive Queue: " + queueName + " owned exclusively by another session");
                }
                else
                {

                    FlowCreditManager_0_10 creditManager = new WindowCreditManager(0L,0L);

                    // TODO filters

                    Subscription_0_10 sub = new Subscription_0_10((ServerSession)session,
                                                                  destination,
                                                                  method.getAcceptMode(),
                                                                  method.getAcquireMode(),
                                                                  MessageFlowMode.WINDOW,
                                                                  creditManager, null);

                    ((ServerSession)session).register(destination, sub);
                    try
                    {
                        queue.registerSubscription(sub, method.getExclusive());
                    }
                    catch (AMQQueue.ExistingExclusiveSubscription existing)
                    {
                        exception(session, method, ExecutionErrorCode.RESOURCE_LOCKED, "Queue has an exclusive consumer");
                    }
                    catch (AMQQueue.ExistingSubscriptionPreventsExclusive exclusive)
                    {
                        exception(session, method, ExecutionErrorCode.RESOURCE_LOCKED, "Queue has an existing consumer - can't subscribe exclusively");
                    }
                    catch (AMQException e)
                    {
                        // TODO
                        throw new RuntimeException(e);
                    }
                }
            }
        }
    }


    @Override
    public void messageTransfer(Session ssn, MessageTransfer xfr)
    {
        ExchangeRegistry exchangeRegistry = getExchangeRegistry(ssn);
        Exchange exchange;
        if(xfr.hasDestination())
        {
            exchange = exchangeRegistry.getExchange(xfr.getDestination());
            if(exchange == null)
            {
                exchange = exchangeRegistry.getDefaultExchange();
            }
        }
        else
        {
            exchange = exchangeRegistry.getDefaultExchange();
        }


        DeliveryProperties delvProps = null;
        if(xfr.getHeader() != null && (delvProps = xfr.getHeader().get(DeliveryProperties.class)) != null && delvProps.hasTtl() && !delvProps.hasExpiration())
        {
            delvProps.setExpiration(System.currentTimeMillis() + delvProps.getTtl());
        }

        MessageMetaData_0_10 messageMetaData = new MessageMetaData_0_10(xfr);
        final MessageStore store = getVirtualHost(ssn).getMessageStore();
        StoredMessage<MessageMetaData_0_10> storeMessage = store.addMessage(messageMetaData);
        storeMessage.addContent(0,xfr.getBody());
        storeMessage.flushToStore();
        MessageTransferMessage message = new MessageTransferMessage(storeMessage, ((ServerSession)ssn).getReference());

        ArrayList<AMQQueue> queues = exchange.route(message);



        if(queues != null && queues.size() != 0)
        {
            ((ServerSession) ssn).enqueue(message, queues);
        }
        else
        {
            if(delvProps == null || !delvProps.hasDiscardUnroutable() || !delvProps.getDiscardUnroutable())
            {
                if(xfr.getAcceptMode() == MessageAcceptMode.EXPLICIT)
                {
                    RangeSet rejects = new RangeSet();
                    rejects.add(xfr.getId());
                    MessageReject reject = new MessageReject(rejects, MessageRejectCode.UNROUTABLE, "Unroutable");
                    ssn.invoke(reject);
                }
                else
                {
                    Exchange alternate = exchange.getAlternateExchange();
                    if(alternate != null)
                    {
                        queues = alternate.route(message);
                        if(queues != null && queues.size() != 0)
                        {
                            ((ServerSession) ssn).enqueue(message, queues);
                        }
                        else
                        {
                            //TODO - log the message discard
                        }
                    }
                    else
                    {
                        //TODO - log the message discard
                    }


                }
            }


        }

        ssn.processed(xfr);

    }

    @Override
    public void messageCancel(Session session, MessageCancel method)
    {
        String destination = method.getDestination();

        Subscription_0_10 sub = ((ServerSession)session).getSubscription(destination);

        if(sub == null)
        {
            exception(session, method, ExecutionErrorCode.NOT_FOUND, "not-found: destination '"+destination+"'");
        }
        else
        {
            ((ServerSession)session).unregister(sub);
        }
    }

    @Override
    public void messageFlush(Session session, MessageFlush method)
    {
        String destination = method.getDestination();

        Subscription_0_10 sub = ((ServerSession)session).getSubscription(destination);

        if(sub == null)
        {
            exception(session, method, ExecutionErrorCode.NOT_FOUND, "not-found: destination '"+destination+"'");
        }
        else
        {

            try
            {
                sub.flush();
            }
            catch (AMQException e)
            {
                //TODO
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                throw new RuntimeException(e);
            }
        }
    }

    @Override
    public void txSelect(Session session, TxSelect method)
    {
        // TODO - check current tx mode
        ((ServerSession)session).selectTx();
    }

    @Override
    public void txCommit(Session session, TxCommit method)
    {
        // TODO - check current tx mode
        ((ServerSession)session).commit();
    }

    @Override
    public void txRollback(Session session, TxRollback method)
    {
        // TODO - check current tx mode
        ((ServerSession)session).rollback();
    }


    @Override
    public void exchangeDeclare(Session session, ExchangeDeclare method)
    {
        String exchangeName = method.getExchange();
        VirtualHost virtualHost = getVirtualHost(session);
        Exchange exchange = getExchange(session, exchangeName);

        if(method.getPassive())
        {
            if(exchange == null)
            {
                exception(session, method, ExecutionErrorCode.NOT_FOUND, "not-found: exchange-name '"+exchangeName+"'");

            }
            else
            {
                // TODO - check exchange has same properties
                if(!exchange.getType().toString().equals(method.getType()))
                {
                    exception(session, method, ExecutionErrorCode.NOT_ALLOWED, "Cannot redeclare with a different exchange type");
                }
            }

        }
        else
        {
            if (!virtualHost.getAccessManager().authoriseCreateExchange((ServerSession)session, method.getAutoDelete(),
                    method.getDurable(), new AMQShortString(method.getExchange()), false, false, method.getPassive(),
                    new AMQShortString(method.getType())))
            {

                ExecutionErrorCode errorCode = ExecutionErrorCode.NOT_ALLOWED;
                String description = "permission denied: exchange-name '" + exchangeName + "'";

                exception(session, method, errorCode, description);


            }
            else if(exchange == null)
            {
                ExchangeRegistry exchangeRegistry = getExchangeRegistry(session);
                ExchangeFactory exchangeFactory = virtualHost.getExchangeFactory();



                try
                {

                    exchange = exchangeFactory.createExchange(method.getExchange(),
                                                              method.getType(),
                                                              method.getDurable(),
                                                              method.getAutoDelete());

                    String alternateExchangeName = method.getAlternateExchange();
                    if(alternateExchangeName != null && alternateExchangeName.length() != 0)
                    {
                        Exchange alternate = getExchange(session, alternateExchangeName);
                        exchange.setAlternateExchange(alternate);
                    }

                    if (exchange.isDurable())
                    {
                        DurableConfigurationStore store = virtualHost.getDurableConfigurationStore();
                        store.createExchange(exchange);
                    }

                    exchangeRegistry.registerExchange(exchange);
                }
                catch(AMQUnknownExchangeType e)
                {
                    exception(session, method, ExecutionErrorCode.NOT_FOUND, "Unknown Exchange Type: " + method.getType());
                }
                catch (AMQException e)
                {
                    //TODO
                    throw new RuntimeException(e);
                }

            }
            else
            {
                if(!exchange.getType().toString().equals(method.getType()))
                {
                    exception(session, method, ExecutionErrorCode.NOT_ALLOWED, "Cannot redeclare with a different exchange type");
                }
            }

        }
    }

    private void exception(Session session, Method method, ExecutionErrorCode errorCode, String description)
    {
        ExecutionException ex = new ExecutionException();
        ex.setErrorCode(errorCode);
        ex.setCommandId(method.getId());
        ex.setDescription(description);

        session.invoke(ex);

    }

    private Exchange getExchange(Session session, String exchangeName)
    {
        ExchangeRegistry exchangeRegistry = getExchangeRegistry(session);
        return exchangeRegistry.getExchange(exchangeName);
    }

    private ExchangeRegistry getExchangeRegistry(Session session)
    {
        VirtualHost virtualHost = getVirtualHost(session);
        return virtualHost.getExchangeRegistry();

    }

    private VirtualHost getVirtualHost(Session session)
    {
        ServerConnection conn = getServerConnection(session);
        VirtualHost vhost = conn.getVirtualHost();
        return vhost;
    }

    private ServerConnection getServerConnection(Session session)
    {
        ServerConnection conn = (ServerConnection) session.getConnection();
        return conn;
    }

    @Override
    public void exchangeDelete(Session session, ExchangeDelete method)
    {
        VirtualHost virtualHost = getVirtualHost(session);
        ExchangeRegistry exchangeRegistry = virtualHost.getExchangeRegistry();

        //Perform ACLs
        if (!virtualHost.getAccessManager().authoriseDelete((ServerSession)session,
                exchangeRegistry.getExchange(method.getExchange())))
        {
            exception(session,method, ExecutionErrorCode.NOT_ALLOWED, "Permission denied");

        }
        else
        {

            try
            {
                Exchange exchange = getExchange(session, method.getExchange());

                if(exchange != null && exchange.hasReferrers())
                {
                    exception(session, method, ExecutionErrorCode.NOT_ALLOWED, "Exchange in use as an alternate exchange");
                }
                else
                {
                    exchangeRegistry.unregisterExchange(method.getExchange(), method.getIfUnused());

                    if (exchange.isDurable() && !exchange.isAutoDelete())
                    {
                        DurableConfigurationStore store = virtualHost.getDurableConfigurationStore();
                        store.removeExchange(exchange);
                    }

                }
            }
            catch (ExchangeInUseException e)
            {
                exception(session, method, ExecutionErrorCode.PRECONDITION_FAILED, "Exchange in use");
            }
            catch (AMQException e)
            {
                // TODO
                throw new RuntimeException(e);
            }
        }

    }

    @Override
    public void exchangeQuery(Session session, ExchangeQuery method)
    {

        ExchangeQueryResult result = new ExchangeQueryResult();

        Exchange exchange = getExchange(session, method.getName());

        if(exchange != null)
        {
            result.setDurable(exchange.isDurable());
            result.setType(exchange.getType().toString());
            result.setNotFound(false);
        }
        else
        {
            result.setNotFound(true);
        }

        session.executionResult((int) method.getId(), result);
    }

    @Override
    public void exchangeBind(Session session, ExchangeBind method)
    {

        VirtualHost virtualHost = getVirtualHost(session);
        ExchangeRegistry exchangeRegistry = virtualHost.getExchangeRegistry();
        QueueRegistry queueRegistry = virtualHost.getQueueRegistry();

        if (!method.hasQueue())
        {
            exception(session, method, ExecutionErrorCode.ILLEGAL_ARGUMENT, "queue not set");
        }
        else if (!method.hasExchange())
        {
            exception(session, method, ExecutionErrorCode.ILLEGAL_ARGUMENT, "exchange not set");
        }
/*
        else if (!method.hasBindingKey())
        {
            exception(session, method, ExecutionErrorCode.ILLEGAL_ARGUMENT, "binding-key not set");
        }
*/
        else
        {
            //TODO - here because of non-compiant python tests
            if (!method.hasBindingKey())
            {
                method.setBindingKey(method.getQueue());
            }
            AMQQueue queue = queueRegistry.getQueue(method.getQueue());
            Exchange exchange = exchangeRegistry.getExchange(method.getExchange());
            if(queue == null)
            {
                exception(session, method, ExecutionErrorCode.NOT_FOUND, "Queue: '" + method.getQueue() + "' not found");
            }
            else if(exchange == null)
            {
                exception(session, method, ExecutionErrorCode.NOT_FOUND, "Exchange: '" + method.getExchange() + "' not found");
            }
            else if (!virtualHost.getAccessManager().authoriseBind((ServerSession)session, exchange,
                    queue, new AMQShortString(method.getBindingKey())))
            {
                exception(session, method, ExecutionErrorCode.NOT_ALLOWED, "Bind Exchange: '" + method.getExchange()
                                                                           + "' to Queue: '" + method.getQueue()
                                                                           + "' not allowed");
            }
            else if(exchange.getType().equals(HeadersExchange.TYPE.getName()) && (!method.hasArguments() || method.getArguments() == null || !method.getArguments().containsKey("x-match")))
            {
                exception(session, method, ExecutionErrorCode.INTERNAL_ERROR, "Bindings to an exchange of type " + HeadersExchange.TYPE.getName() + " require an x-match header");
            }
            else
            {
                try
                {
                    AMQShortString routingKey = new AMQShortString(method.getBindingKey());
                    FieldTable fieldTable = FieldTable.convertToFieldTable(method.getArguments());

                    if (!exchange.isBound(routingKey, fieldTable, queue))
                    {
                        queue.bind(exchange, routingKey, fieldTable);

                    }
                    else
                    {
                        // todo
                    }
                }
                catch (AMQException e)
                {
                    e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                }
            }


        }



    }

    @Override
    public void exchangeUnbind(Session session, ExchangeUnbind method)
    {
        VirtualHost virtualHost = getVirtualHost(session);
        ExchangeRegistry exchangeRegistry = virtualHost.getExchangeRegistry();
        QueueRegistry queueRegistry = virtualHost.getQueueRegistry();

        if (!method.hasQueue())
        {
            exception(session, method, ExecutionErrorCode.ILLEGAL_ARGUMENT, "queue not set");
        }
        else if (!method.hasExchange())
        {
            exception(session, method, ExecutionErrorCode.ILLEGAL_ARGUMENT, "exchange not set");
        }
        else if (!method.hasBindingKey())
        {
            exception(session, method, ExecutionErrorCode.ILLEGAL_ARGUMENT, "binding-key not set");
        }
        else
        {
            AMQQueue queue = queueRegistry.getQueue(method.getQueue());
            Exchange exchange = exchangeRegistry.getExchange(method.getExchange());
            if(queue == null)
            {
                exception(session, method, ExecutionErrorCode.NOT_FOUND, "Queue: '" + method.getQueue() + "' not found");
            }
            else if(exchange == null)
            {
                exception(session, method, ExecutionErrorCode.NOT_FOUND, "Exchange: '" + method.getExchange() + "' not found");
            }
            else
            {
                try
                {
                    queue.unBind(exchange, new AMQShortString(method.getBindingKey()), null);
                }
                catch (AMQException e)
                {
                    throw new RuntimeException(e);
                }
            }
        }


        super.exchangeUnbind(session, method);
    }

    @Override
    public void exchangeBound(Session session, ExchangeBound method)
    {

        ExchangeBoundResult result = new ExchangeBoundResult();
        Exchange exchange;
        AMQQueue queue;
        if(method.hasExchange())
        {
            exchange = getExchange(session, method.getExchange());

            if(exchange == null)
            {
                result.setExchangeNotFound(true);
            }
        }
        else
        {
            exchange = getExchangeRegistry(session).getDefaultExchange();
        }


        if(method.hasQueue())
        {

            queue = getQueue(session, method.getQueue());
            if(queue == null)
            {
                result.setQueueNotFound(true);
            }


            if(exchange != null && queue != null)
            {

                boolean queueMatched = exchange.isBound(queue);

                result.setQueueNotMatched(!queueMatched);


                if(method.hasBindingKey())
                {

                    if(method.hasArguments())
                    {
                        // TODO
                    }
                    if(queueMatched)
                    {
                        result.setKeyNotMatched(!exchange.isBound(method.getBindingKey(), queue));
                    }
                    else
                    {
                        result.setKeyNotMatched(!exchange.isBound(method.getBindingKey()));
                    }
                }
                else if (method.hasArguments())
                {
                    // TODO

                }

                result.setQueueNotMatched(!exchange.isBound(queue));

            }
            else if(exchange != null && method.hasBindingKey())
            {
                if(method.hasArguments())
                {
                    // TODO
                }
                result.setKeyNotMatched(!exchange.isBound(method.getBindingKey()));

            }

        }
        else if(exchange != null && method.hasBindingKey())
        {
            if(method.hasArguments())
            {
                // TODO
            }
            result.setKeyNotMatched(!exchange.isBound(method.getBindingKey()));

        }


        session.executionResult((int) method.getId(), result);


    }

    private AMQQueue getQueue(Session session, String queue)
    {
        QueueRegistry queueRegistry = getQueueRegistry(session);
        return queueRegistry.getQueue(queue);
    }

    private QueueRegistry getQueueRegistry(Session session)
    {
        return getVirtualHost(session).getQueueRegistry();
    }

    @Override
    public void queueDeclare(Session session, QueueDeclare method)
    {

        VirtualHost virtualHost = getVirtualHost(session);
        DurableConfigurationStore store = virtualHost.getDurableConfigurationStore();

        String queueName = method.getQueue();

        if (!method.getPassive())
        {
            // Perform ACL if request is not passive

            if (!virtualHost.getAccessManager().authoriseCreateQueue(((ServerSession)session), method.getAutoDelete(), method.getDurable(),
                    method.getExclusive(), false, method.getPassive(), new AMQShortString(queueName)))
            {
                ExecutionErrorCode errorCode = ExecutionErrorCode.NOT_ALLOWED;
                String description = "permission denied: queue-name '" + queueName + "'";

                exception(session, method, errorCode, description);

                // TODO control flow
                return;
            }
        }


        AMQQueue queue;
        QueueRegistry queueRegistry = getQueueRegistry(session);
        //TODO: do we need to check that the queue already exists with exactly the same "configuration"?

        synchronized (queueRegistry)
        {

            if (((queue = queueRegistry.getQueue(queueName)) == null))
            {

                if (method.getPassive())
                {
                    String description = "Queue: " + queueName + " not found on VirtualHost(" + virtualHost + ").";
                    ExecutionErrorCode errorCode = ExecutionErrorCode.NOT_FOUND;

                    exception(session, method, errorCode, description);

                    return;
                }
                else
                {
                    try
                    {
                        queue = createQueue(queueName, method, virtualHost, (ServerSession)session);
                        if(method.getExclusive())
                        {
                            queue.setPrincipalHolder((ServerSession)session);
                            queue.setExclusiveOwner(session);
                        }
                        else if(method.getAutoDelete())
                        {
                            queue.setDeleteOnNoConsumers(true);
                        }
                        
                        final String alternateExchangeName = method.getAlternateExchange();
                        if(alternateExchangeName != null && alternateExchangeName.length() != 0)
                        {
                            Exchange alternate = getExchange(session, alternateExchangeName);
                            queue.setAlternateExchange(alternate);
                        }

                        if(method.hasArguments()  && method.getArguments() != null)
                        {
                            if(method.getArguments().containsKey("no-local"))
                            {
                                Object no_local = method.getArguments().get("no-local");
                                if(no_local instanceof Boolean && ((Boolean)no_local))
                                {
                                    queue.setNoLocal(true);
                                }
                            }
                        }


                        if (queue.isDurable() && !queue.isAutoDelete())
                        {
                            if(method.hasArguments() && method.getArguments() != null)
                            {
                                Map<String,Object> args = method.getArguments();
                                FieldTable ftArgs = new FieldTable();
                                for(Map.Entry<String, Object> entry : args.entrySet())
                                {
                                    ftArgs.put(new AMQShortString(entry.getKey()), entry.getValue());
                                }
                                store.createQueue(queue, ftArgs);
                            }
                            else
                            {
                                store.createQueue(queue);
                            }
                        }
                        queueRegistry.registerQueue(queue);
                        boolean autoRegister = ApplicationRegistry.getInstance().getConfiguration().getQueueAutoRegister();

                        if (autoRegister)
                        {
                            ExchangeRegistry exchangeRegistry = getExchangeRegistry(session);

                            Exchange defaultExchange = exchangeRegistry.getDefaultExchange();

                            queue.bind(defaultExchange, new AMQShortString(queueName), null);

                        }

                        if(method.hasAutoDelete()
                           && method.getAutoDelete()
                           && method.hasExclusive()
                           && method.getExclusive())
                        {
                            final AMQQueue q = queue;
                            final ServerSession.Task deleteQueueTask = new ServerSession.Task()
                            {

                                public void doTask(ServerSession session)
                                {
                                    try
                                    {
                                        q.delete();
                                    }
                                    catch (AMQException e)
                                    {
                                        throw new RuntimeException(e);
                                    }
                                }
                            };
                            final ServerSession s = (ServerSession) session;
                            s.addSessionCloseTask(deleteQueueTask);
                            queue.addQueueDeleteTask(new AMQQueue.Task()
                            {

                                public void doTask(AMQQueue queue) throws AMQException
                                {
                                    s.removeSessionCloseTask(deleteQueueTask);
                                }
                            });
                        }
                        else if(method.getExclusive())
                        {
                            {
                            final AMQQueue q = queue;
                            final ServerSession.Task removeExclusive = new ServerSession.Task()
                            {

                                public void doTask(ServerSession session)
                                {
                                    q.setPrincipalHolder(null);
                                }
                            };
                            final ServerSession s = (ServerSession) session;
                            s.addSessionCloseTask(removeExclusive);
                            queue.addQueueDeleteTask(new AMQQueue.Task()
                            {

                                public void doTask(AMQQueue queue) throws AMQException
                                {
                                    s.removeSessionCloseTask(removeExclusive);
                                }
                            });
                        }
                        }
                    }
                    catch (AMQException e)
                    {
                        throw new RuntimeException(e);
                    }
                }
            }
            else if (method.getExclusive() && (queue.getPrincipalHolder() != null && !queue.getPrincipalHolder().equals(session)))
            {

                    String description = "Cannot declare queue('" + queueName + "'),"
                                                                           + " as exclusive queue with same name "
                                                                           + "declared on another session";
                    ExecutionErrorCode errorCode = ExecutionErrorCode.RESOURCE_LOCKED;

                    exception(session, method, errorCode, description);

                    return;
            }

        }
    }


    protected AMQQueue createQueue(final String queueName,
                                   QueueDeclare body,
                                   VirtualHost virtualHost,
                                   final ServerSession session)
            throws AMQException
    {
        final QueueRegistry registry = virtualHost.getQueueRegistry();

        String owner = body.getExclusive() ? session.getPrincipal().getName() : null;

        final AMQQueue queue = AMQQueueFactory.createAMQQueueImpl(queueName, body.getDurable(), owner, body.getAutoDelete(), virtualHost,
                                                                  body.getArguments());


        if (body.getExclusive() && !body.getDurable())
        {
            final ServerSession.Task deleteQueueTask =
                    new ServerSession.Task()
                    {
                        public void doTask(ServerSession session)
                        {
                            if (registry.getQueue(queueName) == queue)
                            {
                                try
                                {
                                    queue.delete();
                                }
                                catch (AMQException e)
                                {
                                    //TODO
                                    throw new RuntimeException(e);
                                }
                            }
                        }
                    };

            session.addSessionCloseTask(deleteQueueTask);

            queue.addQueueDeleteTask(new AMQQueue.Task()
            {
                public void doTask(AMQQueue queue)
                {
                    session.removeSessionCloseTask(deleteQueueTask);
                }
            });
        }// if exclusive and not durable

        return queue;
    }

    @Override
    public void queueDelete(Session session, QueueDelete method)
    {

        String queueName = method.getQueue();
        if(queueName == null || queueName.length()==0)
        {
            exception(session, method, ExecutionErrorCode.INVALID_ARGUMENT, "No queue name supplied");

        }
        else
        {
            AMQQueue queue = getQueue(session, queueName);


            if (queue == null)
            {
                exception(session, method, ExecutionErrorCode.NOT_FOUND, "No queue " + queueName + " found");
            }
            else
            {
                if(queue.getPrincipalHolder() != null && queue.getPrincipalHolder() != session)
                {
                    exception(session,method,ExecutionErrorCode.RESOURCE_LOCKED, "Exclusive Queue: " + queueName + " owned exclusively by another session");
                }
                else if (method.getIfEmpty() && !queue.isEmpty())
                {
                    exception(session, method, ExecutionErrorCode.PRECONDITION_FAILED, "Queue " + queueName + " not empty");
                }
                else if (method.getIfUnused() && !queue.isUnused())
                {
                    // TODO - Error code
                    exception(session, method, ExecutionErrorCode.PRECONDITION_FAILED, "Queue " + queueName + " in use");

                }
                else
                {
                    VirtualHost virtualHost = getVirtualHost(session);

                    //Perform ACLs
                    if (!virtualHost.getAccessManager().authoriseDelete(((ServerSession)session), queue))
                    {
                        exception(session, method, ExecutionErrorCode.NOT_ALLOWED, "Cannot delete queue " + queueName);
                    }
                    else
                    {
                        try
                        {
                            int purged = queue.delete();
                            if (queue.isDurable() && !queue.isAutoDelete())
                            {
                                DurableConfigurationStore store = virtualHost.getDurableConfigurationStore();
                                store.removeQueue(queue);
                            }

                        }
                        catch (AMQException e)
                        {
                            //TODO
                            throw new RuntimeException(e);
                        }

                    }

                }
            }
        }

    }

    @Override
    public void queuePurge(Session session, QueuePurge method)
    {
        String queueName = method.getQueue();
        if(queueName == null || queueName.length()==0)
        {
            exception(session, method, ExecutionErrorCode.ILLEGAL_ARGUMENT, "No queue name supplied");

        }
        else
        {
            AMQQueue queue = getQueue(session, queueName);


            if (queue == null)
            {
                exception(session, method, ExecutionErrorCode.NOT_FOUND, "No queue " + queueName + " found");
            }
            else
            {
                //TODO
                queue.clearQueue();
            }
        }

    }

    @Override
    public void queueQuery(Session session, QueueQuery method)
    {
        QueueQueryResult result = new QueueQueryResult();

        AMQQueue queue = getQueue(session, method.getQueue());

        if(queue != null)
        {
            result.setQueue(queue.getName().toString());
            result.setDurable(queue.isDurable());
            result.setExclusive(queue.isExclusive());
            result.setAutoDelete(queue.isAutoDelete());
            result.setArguments(queue.getArguments());
            result.setMessageCount(queue.getMessageCount());
            result.setSubscriberCount(queue.getConsumerCount());

        }


        session.executionResult((int) method.getId(), result);

    }

    @Override
    public void messageSetFlowMode(Session session, MessageSetFlowMode sfm)
    {
        String destination = sfm.getDestination();

        Subscription_0_10 sub = ((ServerSession)session).getSubscription(destination);

        if(sub == null)
        {
            exception(session, sfm, ExecutionErrorCode.NOT_FOUND, "not-found: destination '"+destination+"'");
        }

        if(sub.isStopped())
        {
            sub.setFlowMode(sfm.getFlowMode());
        }
    }

    @Override
    public void messageStop(Session session, MessageStop stop)
    {
        String destination = stop.getDestination();

        Subscription_0_10 sub = ((ServerSession)session).getSubscription(destination);

        if(sub == null)
        {
            exception(session, stop, ExecutionErrorCode.NOT_FOUND, "not-found: destination '"+destination+"'");
        }

        sub.stop();

    }

    @Override
    public void messageFlow(Session session, MessageFlow flow)
    {
        String destination = flow.getDestination();

        Subscription_0_10 sub = ((ServerSession)session).getSubscription(destination);

        if(sub == null)
        {
            exception(session, flow, ExecutionErrorCode.NOT_FOUND, "not-found: destination '"+destination+"'");
        }

        sub.addCredit(flow.getUnit(), flow.getValue());

    }

    @Override
    public void closed(Session session)
    {
        super.closed(session);
        for(Subscription_0_10 sub : getSubscriptions(session))
        {
            ((ServerSession)session).unregister(sub);
        }
        ((ServerSession)session).onClose();
    }

    public Collection<Subscription_0_10> getSubscriptions(Session session)
    {
        return ((ServerSession)session).getSubscriptions();
    }

}
