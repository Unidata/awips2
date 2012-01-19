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
import java.util.Date;

import org.apache.muse.util.xml.XPathUtils;
import org.apache.muse.ws.addressing.EndpointReference;
import org.apache.muse.ws.addressing.soap.SoapFault;
import org.apache.muse.ws.notification.impl.FilterCollection;
import org.apache.muse.ws.notification.impl.MessagePatternFilter;
import org.apache.muse.ws.notification.impl.ProducerPropertiesFilter;
import org.apache.muse.ws.notification.impl.TopicFilter;
import org.apache.muse.ws.notification.remote.NotificationProducerClient;
import org.apache.qpid.management.Names;

/**
 * This example is demonstrating a WS-Notification scenario 
 * when (for simplicity) QMan is at the same time consumer 
 * and producer.
 * 
 * Basically we have (on producer side) two topics : one for 
 * lifecycle events of object instance (objects created & removed) 
 * and another one for lifecycle of event (events created).
 * 
 * On consumer side there are many options that you can use in 
 * order to made a sunscription :
 * 
 * <ul>
 * 	<li>you could be an observer of all messages (all topics);</li>
 * 	<li>you could be an observer of one specific topic;</li>
 * 	<li>
 * 		you could be an observer of all messages that match 
 * 		a condition expressed in XPath;
 *		</li>
 * </ul>
 * 
 * All those options are provided with or withour a termination time. 
 * A subscription with a termination time will have a predefined expiry 
 * date while if there's no termination the subscription will never expire.
 * 
 * @author Andrea Gazzarini
 *
 */
public class ConsumerAndProducerExample extends AbstractQManExample
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
	
		// Example 1 : all messages on all topics without termination time.
        subscribeAllMessagesWithoutTerminationTime(producerEPR,consumerEPR);
        
		// Example 2 : all messages on all topics with termination time.
        subscribeAllMessagesWithTerminationTime(producerEPR,consumerEPR);

        // Example 3: Topic filter without termination time.
        topicSubscriptionWithoutTerminationTime(producerEPR,consumerEPR);
        
        // Example 4: Topic filter with termination time.        
        topicSubscriptionWithTerminationTime(producerEPR,consumerEPR);

        // Example 5: a MessageFilter is installed in order to listen only for connection events 
        // (connections created or removed). The subscription never expire.
        allMessagesWithMessageFilterWithoutTerminationTime(producerEPR,consumerEPR);
        
        // Example 6: a MessageFilter is installed in order to listen only for connection events 
        // (connections created or removed). The subscription will expire in 10 seconds.
        allMessagesWithMessageFilterAndTerminationTime(producerEPR,consumerEPR);
		
		// Example 7 : a subscription with more than one filter.
		complexSubscription(producerEPR, consumerEPR);
	}	

	/**
	 * Makes a subscription on all topics / all messages without an expiry date.
	 * 
	 * @param producer the producer endpoint reference.
	 * @param consumer the consumer endpoint reference .
	 * @throws SoapFault when the subscription cannot be made.
	 */
	private void subscribeAllMessagesWithoutTerminationTime(EndpointReference producer, EndpointReference consumer) throws SoapFault
	{
		NotificationProducerClient producerClient = new NotificationProducerClient(producer);
        producerClient.setTrace(true);

        producerClient.subscribe(
        		consumer,	// Consumer Endpoint reference
        		null,			// Filter, if null that means "all messages"
        		null);			// Termination Time : if null the subscription will never expire.
	}

	/**
	 * Makes a subscription on all topics / all messages with 10 seconds as termination time.
	 * The subscription will expire after 10 seconds.
	 * 
	 * @param producer the producer endpoint reference.
	 * @param consumer the consumer endpoint reference .
	 * @throws SoapFault when the subscription cannot be made.
	 */
	private void subscribeAllMessagesWithTerminationTime(EndpointReference producer, EndpointReference consumer) throws SoapFault
	{
		NotificationProducerClient producerClient = new NotificationProducerClient(producer);
        producerClient.setTrace(true);

        producerClient.subscribe(
        		consumer,	// Consumer Endpoint reference
        		null,			// Filter, if null that means "all messages"
        		new Date(System.currentTimeMillis() + 10000));	// Termination Time 
	}

	/**
	 * Makes a subscription on a specifc topic without an expiry date.
	 * Only messages published on the given topic will be delivered to the given consumer.
	 * 
	 * @param producer the producer endpoint reference.
	 * @param consumer the consumer endpoint reference .
	 * @throws SoapFault when the subscription cannot be made.
	 */
	private void topicSubscriptionWithoutTerminationTime(EndpointReference producer, EndpointReference consumer) throws SoapFault
	{
		NotificationProducerClient producerClient = new NotificationProducerClient(producer);
        producerClient.setTrace(true);

		TopicFilter filter = new TopicFilter(Names.EVENTS_LIFECYLE_TOPIC_NAME);

        producerClient.subscribe(
        		consumer,	// Consumer Endpoint reference
        		filter,			// Topic Filter
        		null);			// Termination Time : if null the subscription will never expire.
	}

	/**
	 * Makes a subscription on a specifc topic with an expiry date.
	 * Only messages published on the given topic will be delivered to the given consumer.
	 * The subscription will end after 10 seconds
	 * 
	 * @param producer the producer endpoint reference.
	 * @param consumer the consumer endpoint reference .
	 * @throws SoapFault when the subscription cannot be made.
	 */
	private void topicSubscriptionWithTerminationTime(EndpointReference producer, EndpointReference consumer) throws SoapFault
	{
		NotificationProducerClient producerClient = new NotificationProducerClient(producer);
        producerClient.setTrace(true);

		TopicFilter filter = new TopicFilter(Names.EVENTS_LIFECYLE_TOPIC_NAME);

        producerClient.subscribe(
        		consumer,	// Consumer Endpoint reference
        		filter,			// Topic Filter
        		new Date(System.currentTimeMillis() + 10000));	// Termination Time 
	}

	/**
	 * Makes a subscription on all topics with a message filter without an expiry date.
	 * 
	 * @param producer the producer endpoint reference.
	 * @param consumer the consumer endpoint reference .
	 * @throws SoapFault when the subscription cannot be made.
	 */
	private void allMessagesWithMessageFilterWithoutTerminationTime(EndpointReference producer, EndpointReference consumer) throws SoapFault
	{
		NotificationProducerClient producerClient = new NotificationProducerClient(producer);
        producerClient.setTrace(true);

        // Applying this filter will result in a subscription that wll be notified only when a "connection" 
        // object is created or removed
        MessagePatternFilter filter= new MessagePatternFilter(
        		"/wsnt:NotificationMessage/wsnt:Message/qman:LifeCycleEvent/qman:Resource/qman:Name/text()='connection'", // expression (XPath)
        		XPathUtils.NAMESPACE_URI); // Dialect : the only supported dialect is XPath 1.0
        
        producerClient.subscribe(
        		consumer,	// Consumer Endpoint reference
        		filter,			// Message Filter
        		null);			// Termination Time : if null the subscription will never expire.
	}
	
	/**
	 * Makes a subscription on all topics with a message filter and an expiry date.
	 * 
	 * @param producer the producer endpoint reference.
	 * @param consumer the consumer endpoint reference .
	 * @throws SoapFault when the subscription cannot be made.
	 */
	private void allMessagesWithMessageFilterAndTerminationTime(EndpointReference producer, EndpointReference consumer) throws SoapFault
	{
		NotificationProducerClient producerClient = new NotificationProducerClient(producer);
        producerClient.setTrace(true);

        // Applying this filter will result in a subscription that wll be notified only when a "connection" 
        // object is created or removed
        MessagePatternFilter filter= new MessagePatternFilter(
        		"/wsnt:NotificationMessage/wsnt:Message/qman:LifeCycleEvent/qman:Resource/qman:Name/text()='connection'", // expression (XPath)
        		XPathUtils.NAMESPACE_URI); // Dialect : the only supported dialect is XPath 1.0
        
        producerClient.subscribe(
        		consumer,	// Consumer Endpoint reference
        		filter,			// Message Filter
        		new Date(System.currentTimeMillis() + 10000));	// Termination Time 
	}
	
	/**
	 * Makes a subscription on a specifc topic with an expiry date.
	 * Only messages published on the given topic will be delivered to the given consumer.
	 * The subscription will end after 10 seconds
	 * 
	 * @param producer the producer endpoint reference.
	 * @param consumer the consumer endpoint reference .
	 * @throws SoapFault when the subscription cannot be made.
	 */
	private void complexSubscription(EndpointReference producer, EndpointReference consumer) throws SoapFault
	{
		NotificationProducerClient producerClient = new NotificationProducerClient(producer);
        producerClient.setTrace(true);

        FilterCollection filter = new FilterCollection();
        
		TopicFilter topicFilter = new TopicFilter(Names.EVENTS_LIFECYLE_TOPIC_NAME);
        MessagePatternFilter messageFilter= new MessagePatternFilter(
        		"/wsnt:NotificationMessage/wsnt:Message/qman:LifeCycleEvent/qman:Resource/qman:Name/text()='connection'", // expression (XPath)
        		XPathUtils.NAMESPACE_URI); // Dialect : the only supported dialect is XPath 1.0

        ProducerPropertiesFilter producerFilter = new ProducerPropertiesFilter(
        		"boolean(/*/MgtPubInterval > 100 and /*/MsgTotalEnqueues > 56272)",
        		XPathUtils.NAMESPACE_URI);
        
        filter.addFilter(topicFilter);
        filter.addFilter(messageFilter);
        filter.addFilter(producerFilter);
		
        producerClient.subscribe(
        		consumer,	// Consumer Endpoint reference
        		filter,			// Topic Filter
        		new Date(System.currentTimeMillis() + 10000));	// Termination Time 
	}
	
	@Override
	void printOutExampleDescription()
	{
		System.out.println("This example is demonstrating a WS-Notification scenario ");
		System.out.println("when (for simplicity) QMan is at the same time consumer ");
		System.out.println("and producer.");
		System.out.println();
		System.out.println("Basically we have (on producer side) two topics : one for"); 
		System.out.println("lifecycle events of object instance (objects created & removed) ");
		System.out.println("and another one for lifecycle of event (events created).");
		System.out.println();
		System.out.println("On consumer side there are many options that you can use in"); 
		System.out.println("order to made a sunscription :");
		System.out.println();
		System.out.println("- you could be an observer of all messages (all topics);");
		System.out.println("- you could be an observer of one specific topic;");
		System.out.println("- you could be an observer of all messages that match a condition expressed in XPath;");
		System.out.println();
		System.out.println("All those options are provided with or withour a termination time."); 
		System.out.println("A subscription with a termination time will have a predefined expiry"); 
		System.out.println("date while if there's no termination the subscription will never expire.");
	}
	
	public static void main(String[] args)
	{
		new ConsumerAndProducerExample().execute(new String[]{"localhost","8080"});
	}
}
