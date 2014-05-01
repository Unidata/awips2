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

package org.apache.qpid.server.security.access;

import junit.framework.TestCase;

import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.qpid.framing.AMQShortString;
import org.apache.qpid.framing.FieldTable;
import org.apache.qpid.framing.amqp_0_9.ExchangeDeclareBodyImpl;
import org.apache.qpid.framing.amqp_8_0.QueueBindBodyImpl;
import org.apache.qpid.server.configuration.VirtualHostConfiguration;
import org.apache.qpid.server.exchange.DirectExchange;
import org.apache.qpid.server.queue.AMQQueue;
import org.apache.qpid.server.queue.AMQQueueFactory;
import org.apache.qpid.server.security.access.ACLPlugin.AuthzResult;
import org.apache.qpid.server.virtualhost.VirtualHostImpl;
import org.apache.qpid.server.virtualhost.VirtualHost;
import org.apache.qpid.server.registry.ApplicationRegistry;

public class PrincipalPermissionsTest extends TestCase
{

    private String _user = "user";
    private PrincipalPermissions _perms;

    // Common things that are passed to frame constructors
    private AMQShortString _queueName = new AMQShortString(this.getClass().getName()+"queue");
    private AMQShortString _tempQueueName = new AMQShortString(this.getClass().getName()+"tempqueue");
    private AMQShortString _exchangeName = new AMQShortString("amq.direct");
    private AMQShortString _routingKey = new AMQShortString(this.getClass().getName()+"route");
    private int _ticket = 1;
    private FieldTable _arguments = null;
    private boolean _nowait = false;
    private boolean _passive = false;
    private boolean _durable = false;
    private boolean _autoDelete = false;
    private AMQShortString _exchangeType = new AMQShortString("direct");
    private boolean _internal = false;

    private DirectExchange _exchange;
    private VirtualHost _virtualHost;
    private AMQShortString _owner = new AMQShortString(this.getClass().getName()+"owner");
    private AMQQueue _queue;
    private AMQQueue _temporaryQueue;
    private Boolean _temporary = false;
    private Boolean _ownQueue = false;

    @Override
    public void setUp()
    {
        //Highlight that this test will cause a new AR to be created
        ApplicationRegistry.getInstance();

        _perms = new PrincipalPermissions(_user);
        try
        {
            PropertiesConfiguration env = new PropertiesConfiguration();
            _virtualHost = new VirtualHostImpl(new VirtualHostConfiguration("test", env));
            _exchange = DirectExchange.TYPE.newInstance(_virtualHost, _exchangeName, _durable, _ticket, _autoDelete);
            _queue = AMQQueueFactory.createAMQQueueImpl(_queueName, false, _owner , false, _virtualHost, _arguments);
            _temporaryQueue = AMQQueueFactory.createAMQQueueImpl(_tempQueueName, false, _owner , true, _virtualHost, _arguments);
        }
        catch (Exception e)
        {
            fail(e.getMessage());
        }
    }

    @Override
    protected void tearDown() throws Exception
    {
        //Ensure we close the opened Registry
        ApplicationRegistry.remove();
    }


    public void testPrincipalPermissions()
    {
        assertNotNull(_perms);
        assertEquals(AuthzResult.ALLOWED, _perms.authorise(Permission.ACCESS, (Object[]) null));
    }

    // FIXME: test has been disabled since the permissions assume that the user has tried to create
    // the queue first. QPID-1597
    public void disableTestBind() throws Exception
    {
        QueueBindBodyImpl bind = new QueueBindBodyImpl(_ticket, _queueName, _exchangeName, _routingKey, _nowait, _arguments);
        Object[] args = new Object[]{bind, _exchange, _queue, _routingKey};
        
        assertEquals(AuthzResult.DENIED, _perms.authorise(Permission.BIND, args));
        _perms.grant(Permission.BIND, (Object[]) null);
        assertEquals(AuthzResult.ALLOWED, _perms.authorise(Permission.BIND, args));
    }

    public void testQueueCreate()
    {
        Object[] grantArgs = new Object[]{_temporary , _queueName, _exchangeName, _routingKey};
        Object[] authArgs = new Object[]{_autoDelete, _queueName};
        
        assertEquals(AuthzResult.DENIED, _perms.authorise(Permission.CREATEQUEUE, authArgs));
        _perms.grant(Permission.CREATEQUEUE, grantArgs);
        assertEquals(AuthzResult.ALLOWED, _perms.authorise(Permission.CREATEQUEUE, authArgs));
    }

     public void testQueueCreateWithNullRoutingKey()
    {
        Object[] grantArgs = new Object[]{_temporary , _queueName, _exchangeName, null};
        Object[] authArgs = new Object[]{_autoDelete, _queueName};

        assertEquals(AuthzResult.DENIED, _perms.authorise(Permission.CREATEQUEUE, authArgs));
        _perms.grant(Permission.CREATEQUEUE, grantArgs);
        assertEquals(AuthzResult.ALLOWED, _perms.authorise(Permission.CREATEQUEUE, authArgs));
    }

    // FIXME disabled, this fails due to grant putting the grant into the wrong map QPID-1598
    public void disableTestExchangeCreate()
    {
        ExchangeDeclareBodyImpl exchangeDeclare =
            new ExchangeDeclareBodyImpl(_ticket, _exchangeName, _exchangeType, _passive, _durable,
                                        _autoDelete, _internal, _nowait, _arguments);
        Object[] authArgs = new Object[]{exchangeDeclare};
        Object[] grantArgs = new Object[]{_exchangeName, _exchangeType};

        assertEquals(AuthzResult.DENIED, _perms.authorise(Permission.CREATEEXCHANGE, authArgs));
        _perms.grant(Permission.CREATEEXCHANGE, grantArgs);
        assertEquals(AuthzResult.ALLOWED, _perms.authorise(Permission.CREATEEXCHANGE, authArgs));
    }

    public void testConsume()
    {
        Object[] authArgs = new Object[]{_queue};
        Object[] grantArgs = new Object[]{_queueName, _ownQueue};

        /* FIXME: This throws a null pointer exception QPID-1599
         * assertFalse(_perms.authorise(Permission.CONSUME, authArgs));
         */
        _perms.grant(Permission.CONSUME, grantArgs);
        assertEquals(AuthzResult.ALLOWED, _perms.authorise(Permission.CONSUME, authArgs));
    }
    
    public void testPublish()
    {
        Object[] authArgs = new Object[]{_exchange, _routingKey};
        Object[] grantArgs = new Object[]{_exchange.getName(), _routingKey};
        
        assertEquals(AuthzResult.DENIED, _perms.authorise(Permission.PUBLISH, authArgs));
        _perms.grant(Permission.PUBLISH, grantArgs);
        assertEquals(AuthzResult.ALLOWED, _perms.authorise(Permission.PUBLISH, authArgs));
    }

    public void testVhostAccess()
    {
        //Tests that granting a user Virtualhost level access allows all authorisation requests
        //where previously they would be denied 
        
        //QPID-2133 createExchange rights currently allow all exchange creation unless rights for creating some
        //specific exchanges are granted. Grant a specific exchange creation to cause all others to be denied.
        Object[] createArgsCreateExchange = new Object[]{new AMQShortString("madeup"), _exchangeType};
        Object[] authArgsCreateExchange = new Object[]{_exchangeName,_exchangeType};
        assertEquals("Exchange creation was not allowed", AuthzResult.ALLOWED, _perms.authorise(Permission.CREATEEXCHANGE, authArgsCreateExchange));
        _perms.grant(Permission.CREATEEXCHANGE, createArgsCreateExchange);
        
        Object[] authArgsPublish = new Object[]{_exchange, _routingKey};        
        Object[] authArgsConsume = new Object[]{_queue};
        Object[] authArgsCreateQueue = new Object[]{_autoDelete, _queueName};
        QueueBindBodyImpl bind = new QueueBindBodyImpl(_ticket, _queueName, _exchangeName, _routingKey, _nowait, _arguments);
        Object[] authArgsBind = new Object[]{bind, _exchange, _queue, _routingKey};
        
        assertEquals("Exchange creation was not denied", AuthzResult.DENIED, _perms.authorise(Permission.CREATEEXCHANGE, authArgsCreateExchange));
        assertEquals("Publish was not denied", AuthzResult.DENIED, _perms.authorise(Permission.PUBLISH, authArgsPublish));
        assertEquals("Consume creation was not denied", AuthzResult.DENIED, _perms.authorise(Permission.CONSUME, authArgsConsume));
        assertEquals("Queue creation was not denied", AuthzResult.DENIED, _perms.authorise(Permission.CREATEQUEUE, authArgsCreateQueue));
        //BIND pre-grant authorise check disabled due to QPID-1597
        //assertEquals("Binding creation was not denied", AuthzResult.DENIED, _perms.authorise(Permission.BIND, authArgsBind));
        
        _perms.grant(Permission.ACCESS);

        assertEquals("Exchange creation was not allowed", AuthzResult.ALLOWED, _perms.authorise(Permission.CREATEEXCHANGE, authArgsCreateExchange));
        assertEquals("Publish was not allowed", AuthzResult.ALLOWED, _perms.authorise(Permission.PUBLISH, authArgsPublish));
        assertEquals("Consume creation was not allowed", AuthzResult.ALLOWED, _perms.authorise(Permission.CONSUME, authArgsConsume));
        assertEquals("Queue creation was not allowed", AuthzResult.ALLOWED, _perms.authorise(Permission.CREATEQUEUE, authArgsCreateQueue));
        assertEquals("Binding creation was not allowed", AuthzResult.ALLOWED, _perms.authorise(Permission.BIND, authArgsBind));
    }

    /**
    * If the consume permission for temporary queues is for an unnamed queue then is should
    * be global for any temporary queue but not for any non-temporary queue
    */
    public void testTemporaryUnnamedQueueConsume()
    {
       Object[] authNonTempQArgs = new Object[]{_queue};
       Object[] authTempQArgs = new Object[]{_temporaryQueue};
       Object[] grantArgs = new Object[]{true};

       _perms.grant(Permission.CONSUME, grantArgs);

       //Next line shows up bug - non temp queue should be denied
       assertEquals(AuthzResult.DENIED, _perms.authorise(Permission.CONSUME, authNonTempQArgs));
       assertEquals(AuthzResult.ALLOWED, _perms.authorise(Permission.CONSUME, authTempQArgs));
    }

    /**
    * Test that temporary queue permissions before queue perms in the ACL config work correctly
    */
    public void testTemporaryQueueFirstConsume()
    {
       Object[] authNonTempQArgs = new Object[]{_queue};
       Object[] authTempQArgs = new Object[]{_temporaryQueue};
       Object[] grantArgs = new Object[]{true};
       Object[] grantNonTempQArgs = new Object[]{_queueName, _ownQueue};

       //should not matter if the temporary permission is processed first or last
       _perms.grant(Permission.CONSUME, grantNonTempQArgs);
       _perms.grant(Permission.CONSUME, grantArgs);

       assertEquals(AuthzResult.ALLOWED, _perms.authorise(Permission.CONSUME, authNonTempQArgs));
       assertEquals(AuthzResult.ALLOWED, _perms.authorise(Permission.CONSUME, authTempQArgs));
    }

    /**
    * Test that temporary queue permissions after queue perms in the ACL config work correctly
    */
    public void testTemporaryQueueLastConsume()
    {
       Object[] authNonTempQArgs = new Object[]{_queue};
       Object[] authTempQArgs = new Object[]{_temporaryQueue};
       Object[] grantArgs = new Object[]{true};
       Object[] grantNonTempQArgs = new Object[]{_queueName, _ownQueue};

       //should not matter if the temporary permission is processed first or last
       _perms.grant(Permission.CONSUME, grantArgs);
       _perms.grant(Permission.CONSUME, grantNonTempQArgs);

       assertEquals(AuthzResult.ALLOWED, _perms.authorise(Permission.CONSUME, authNonTempQArgs));
       assertEquals(AuthzResult.ALLOWED, _perms.authorise(Permission.CONSUME, authTempQArgs));
    }

}
