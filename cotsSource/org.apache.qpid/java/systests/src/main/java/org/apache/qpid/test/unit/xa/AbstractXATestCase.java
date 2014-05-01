/* Licensed to the Apache Software Foundation (ASF) under one
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
 */
package org.apache.qpid.test.unit.xa;

import org.apache.qpid.dtx.XidImpl;
import org.apache.qpid.test.utils.QpidTestCase;

import javax.transaction.xa.Xid;
import javax.transaction.xa.XAResource;
import javax.jms.*;
import java.util.Random;

/**
 *
 *
 */
public abstract  class AbstractXATestCase extends QpidTestCase
{
    protected static final String _sequenceNumberPropertyName = "seqNumber";

      /**
     * the xaResource associated with the standard session
     */
    protected static XAResource _xaResource = null;

    /**
     * producer registered with the standard session
     */
    protected static MessageProducer _producer = null;

    /**
     * consumer registered with the standard session
     */
    protected static MessageConsumer _consumer = null;

    /**
     * a standard message
     */
    protected static TextMessage _message = null;

     /**
     * xid counter
     */
    private static int _xidCounter = (new Random()).nextInt(1000000);


     protected void setUp() throws Exception
    {
        super.setUp();
        init();
    }

    public abstract  void init() throws Exception;


    
    /**
         * construct a new Xid
         *
         * @return a new Xid
         */
        protected Xid getNewXid()
        {
            byte[] branchQualifier;
            byte[] globalTransactionID;
            int format = _xidCounter;
            String branchQualifierSt = "branchQualifier" + _xidCounter;
            String globalTransactionIDSt = "globalTransactionID" + _xidCounter;
            branchQualifier = branchQualifierSt.getBytes();
            globalTransactionID = globalTransactionIDSt.getBytes();
            _xidCounter++;
            return new XidImpl(branchQualifier, format, globalTransactionID);
        }

        public void init(XASession session, Destination destination)
        {
               // get the xaResource
            try
            {
                _xaResource = session.getXAResource();
            }
            catch (Exception e)
            {
                fail("cannot access the xa resource: " + e.getMessage());
            }
            // create standard producer
            try
            {
                _producer = session.createProducer(destination);
                _producer.setDeliveryMode(DeliveryMode.PERSISTENT);
            }
            catch (JMSException e)
            {
                e.printStackTrace();
                fail("cannot create message producer: " + e.getMessage());
            }
            // create standard consumer
            try
            {
                _consumer = session.createConsumer(destination);
            }
            catch (JMSException e)
            {
                fail("cannot create message consumer: " + e.getMessage());
            }
            // create a standard message
            try
            {
                _message = session.createTextMessage();
                _message.setText("test XA");
            }
            catch (JMSException e)
            {
                fail("cannot create standard message: " + e.getMessage());
            }
        }
}
