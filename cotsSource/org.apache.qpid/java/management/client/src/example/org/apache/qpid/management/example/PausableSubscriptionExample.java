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
package org.apache.qpid.management.example;

import java.net.URI;

import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.muse.ws.notification.remote.NotificationProducerClient;
import org.apache.muse.ws.notification.remote.SubscriptionClient;

/**
 * This example is demonstrating a WS-Notification scenario 
 * when (for simplicity) QMan is at the same time consumer 
 * and producer.
 * 
 * Specifically the example shows how a requestor can create, pause and resume 
 * a subscription.
 * 
 * @author Andrea Gazzarini
 *
 */
public class PausableSubscriptionExample extends AbstractQManExample
{
	@Override
	void executeExample(String host, int port) throws Exception
	{ 
		// This is QMan... 
		URI producerURI = URI.create("http://"+host+":"+port+"/qman/services/adapter");
		
		// ...and this is QMan too! Note that it has an hidden consumer capability that is used in 
		// order to run successfully this example...
		URI consumerURI = URI.create("http://"+host+":"+port+"/qman/services/consumer");
		
		EndpointReference producerEPR = new EndpointReference(producerURI);		
		EndpointReference consumerEPR = new EndpointReference(consumerURI);
	
		NotificationProducerClient producerClient = new NotificationProducerClient(producerEPR);
        producerClient.setTrace(true);

        // 1) Creates a subscription and gets the corresponding reference.
        SubscriptionClient subscriptionClient = producerClient.subscribe(
        		consumerEPR,	// Consumer Endpoint reference
        		null,			// Filter, if null that means "all messages"
        		null);			// Termination Time : if null the subscription will never expire.
        subscriptionClient.setTrace(true);
        
        
        // 2) Pauses the subscription.
        subscriptionClient.pauseSubscription();

        // 3) Resumes the subscription.
        subscriptionClient.resumeSubscription();
	}	
	
	@Override
	void printOutExampleDescription()
	{
		System.out.println("This example is demonstrating a WS-Notification scenario ");
		System.out.println("when (for simplicity) QMan is at the same time consumer ");
		System.out.println("and producer.");
		System.out.println();
		System.out.println("Specifically the example shows how a requestor can create,");
		System.out.println("pause and resume a subscription.");
	}
	
	public static void main(String[] args)
	{
		new PausableSubscriptionExample().execute(new String[]{"romagazzarini","8080"});
	}
}
