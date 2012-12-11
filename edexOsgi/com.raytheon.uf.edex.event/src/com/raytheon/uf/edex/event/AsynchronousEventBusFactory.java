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
package com.raytheon.uf.edex.event;

import java.util.concurrent.Executors;

import com.google.common.eventbus.AsyncEventBus;
import com.google.common.eventbus.EventBus;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * {@link GoogleEventBusFactory} implementation that creates an asynchronous
 * {@link EventBus}. Intentionally package-private as it should only be used
 * within this package, and not part of the public API.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2012 1407       djohnson     Moved in from EventBus.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
class AsynchronousEventBusFactory implements GoogleEventBusFactory {


    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AsynchronousEventBusFactory.class);

    private static final int DEFAULT_THREAD_COUNT = 15;

    private static final String EVENT_BUS_THREAD_COUNT_PROPERTY = "eventBusThreadCount";

    static final String EVENT_BUS_NAME = "EventBus";

    /**
     * {@inheritDoc}
     */
    @Override
    public EventBus getEventBus() {
        int threadCount = 15;
        try {
            threadCount = Integer.getInteger(EVENT_BUS_THREAD_COUNT_PROPERTY,
                    DEFAULT_THREAD_COUNT);
        } catch (Exception e) {
            final String logMessage = String
                    .format("Unable to set thread pool size from property %s; defaulting size to %s.",
                            EVENT_BUS_THREAD_COUNT_PROPERTY, threadCount);
            statusHandler.error(logMessage, e);
        }
        return new AsyncEventBus(EVENT_BUS_NAME,
                Executors.newFixedThreadPool(threadCount));
    }

}
