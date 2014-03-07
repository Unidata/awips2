/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.esb.camel.jms;

import java.util.Map;

import org.apache.camel.Endpoint;
import org.apache.camel.component.jms.JmsComponent;
import org.apache.camel.component.jms.JmsEndpoint;

import com.raytheon.uf.edex.esb.camel.spring.JmsThreadPoolTaskExecutor;

/**
 * Custom JMS component that makes dedicated thread pools for each JmsEndpoint
 * based on the concurrent consumers needed. Each pool is named based on the JMS
 * endpoint. Each endpoint also overrides the message listener container factory
 * to monitor the created containers to see if they need to be restarted in a
 * disconnect scenario.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 07, 2014 2357       rjpeter     Initial creation.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class DedicatedThreadJmsComponent extends JmsComponent {

    public DedicatedThreadJmsComponent(
            org.apache.camel.component.jms.JmsConfiguration jmsconfig) {
        super(jmsconfig);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.camel.component.jms.JmsComponent#createEndpoint(java.lang.
     * String, java.lang.String, java.util.Map)
     */
    @Override
    protected Endpoint createEndpoint(String uri, String remaining,
            Map<String, Object> parameters) throws Exception {
        String threadName = (String) parameters.remove("threadName");
        Endpoint e = super.createEndpoint(uri, remaining, parameters);
        if (e instanceof JmsEndpoint) {
            JmsEndpoint jmsE = (JmsEndpoint) e;
            if ((threadName != null) && (threadName.length() > 0)
                    && !threadName.endsWith("-")) {
                threadName += "-";
            } else {
                threadName = jmsE.getDestinationName() + "-";
            }
            /*
             * This is used for a SimpleMessageListenerContainer use case.
             * 
             * JmsSimpleMessageListenerTaskExecutor executor = new
             * JmsSimpleMessageListenerTaskExecutor(
             * jmsE.getConcurrentConsumers(), threadName);
             */
            // DefaultMessageListenerContainer use case
            JmsThreadPoolTaskExecutor executor = new JmsThreadPoolTaskExecutor();
            executor.setThreadNamePrefix(threadName);
            executor.setCorePoolSize(jmsE.getConcurrentConsumers());
            executor.setMaxPoolSize(Math.max(jmsE.getConcurrentConsumers(),
                    jmsE.getMaxConcurrentConsumers()));
            executor.setQueueCapacity(0);
            executor.afterPropertiesSet();

            jmsE.setTaskExecutor(executor);
            jmsE.setMessageListenerContainerFactory(MonitoredDefaultMessageListenerContainerFactory
                    .getInstance());

            return jmsE;
        }
        throw new Exception(
                "JmsComponent did not create a JmsEnpoint. Check Camel Jms Override");
    }

}
