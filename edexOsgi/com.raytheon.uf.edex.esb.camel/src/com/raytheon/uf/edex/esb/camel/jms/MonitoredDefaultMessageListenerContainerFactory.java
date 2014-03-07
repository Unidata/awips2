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

import java.util.Collection;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.camel.component.jms.DefaultJmsMessageListenerContainer;
import org.apache.camel.component.jms.JmsEndpoint;
import org.apache.camel.component.jms.MessageListenerContainerFactory;
import org.springframework.jms.listener.AbstractMessageListenerContainer;
import org.springframework.jms.listener.DefaultMessageListenerContainer;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Creates DefaultMessageListenerContainer instances that are then monitored
 * once a minute for paused tasks. If a paused task is found the container is
 * restarted. This is necessary in broker restart scenarios.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 8, 2014  2357      rjpeter     Initial creation.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class MonitoredDefaultMessageListenerContainerFactory implements
        MessageListenerContainerFactory {
    private static final AtomicInteger threadCount = new AtomicInteger(1);

    private final Collection<DefaultJmsMessageListenerContainer> containers = new ConcurrentLinkedQueue<DefaultJmsMessageListenerContainer>();

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MonitoredDefaultMessageListenerContainerFactory.class);

    private static final MonitoredDefaultMessageListenerContainerFactory instance = new MonitoredDefaultMessageListenerContainerFactory();

    public static MonitoredDefaultMessageListenerContainerFactory getInstance() {
        return instance;
    }

    private MonitoredDefaultMessageListenerContainerFactory() {
        Thread containerChecker = new Thread("MessageListenerContainerMonitor-"
                + threadCount.getAndIncrement()) {
            /*
             * (non-Javadoc)
             * 
             * @see java.lang.Thread#run()
             */
            @Override
            public void run() {
                while (!EDEXUtil.isRunning()) {
                    try {
                        Thread.sleep(TimeUtil.MILLIS_PER_MINUTE);
                    } catch (InterruptedException e) {
                        // ignore
                    }
                }

                while (true) {
                    try {
                        for (DefaultMessageListenerContainer container : containers) {
                            if (container.getPausedTaskCount() > 0) {
                                StringBuilder msg = new StringBuilder(160);
                                msg.append("Container[")
                                        .append(container.getDestinationName())
                                        .append("] has paused tasks.  Container is ");
                                if (!container.isRunning()) {
                                    msg.append("not ");
                                }
                                msg.append("running.  Container is ");
                                if (container.isActive()) {
                                    msg.append("not ");
                                }
                                msg.append("active.  Restarting container.");
                                statusHandler.warn(msg.toString());
                                container.start();
                            }
                        }

                        try {
                            Thread.sleep(TimeUtil.MILLIS_PER_MINUTE);
                        } catch (InterruptedException e) {
                            // ignore
                        }
                    } catch (Throwable e) {
                        statusHandler
                                .error("Error occurred in checking message listener containers",
                                        e);
                    }
                }
            }
        };
        containerChecker.start();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.camel.component.jms.MessageListenerContainerFactory#
     * createMessageListenerContainer
     * (org.apache.camel.component.jms.JmsEndpoint)
     */
    @Override
    public AbstractMessageListenerContainer createMessageListenerContainer(
            JmsEndpoint endpoint) {
        // track the container for monitoring in the case of a provider
        // reconnect
        DefaultJmsMessageListenerContainer container = new DefaultJmsMessageListenerContainer(
                endpoint);
        containers.add(container);
        return container;
    }

}
