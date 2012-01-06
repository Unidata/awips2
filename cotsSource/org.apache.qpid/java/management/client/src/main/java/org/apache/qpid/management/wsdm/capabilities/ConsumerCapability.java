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
package org.apache.qpid.management.wsdm.capabilities;

import org.apache.muse.core.AbstractCapability;
import org.apache.muse.ws.addressing.soap.SoapFault;
import org.apache.muse.ws.notification.NotificationConsumer;
import org.apache.muse.ws.notification.NotificationMessage;
import org.apache.muse.ws.notification.NotificationMessageListener;
import org.apache.muse.ws.notification.WsnConstants;

/**
 * WS-Notifications consumer capability.
 * At the moment QMan is not a consumer of itself so this capability is here only
 * for test purposes.
 * 
 * @author Andrea Gazzarini
 */
public class ConsumerCapability extends AbstractCapability implements NotificationMessageListener
{
	/**
	 * Initializes this capability and register itself as message listener.
	 * 
	 * @throws SoapFault when the initialization fails.
	 */
    public void initializeCompleted() throws SoapFault
    {
        super.initializeCompleted();
        
        NotificationConsumer wsn = (NotificationConsumer)getResource().getCapability(WsnConstants.CONSUMER_URI);
        wsn.addMessageListener(this);
    }
    
    /**
     * Returns true if this consumer can accepts the message.
     * 
     * @return true;
     */
    public boolean accepts(NotificationMessage message)
    {
        return true;
    }

    /**
     * On Message callback.
     */
    public void process(NotificationMessage message)
    {
    }
}