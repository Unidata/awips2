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
package com.raytheon.uf.viz.event;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.event.EventPublishRequest;
import com.raytheon.uf.common.event.IEventBusHandler;
import com.raytheon.uf.common.serialization.comm.RequestRouter;

/**
 * Cave implementation of the {@link IEventBusHandler}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 5, 2013    1580     mpduff      Initial creation.
 * May 28, 2013  1650      djohnson    Return subscriber on register method for Spring.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class CaveEventBusHandler implements IEventBusHandler {
    /** Core pool size */
    private final int corePoolSize = 1;

    /** Max pool size */
    private final int maxPoolSize = 3;

    /** Time for threads to live */
    private final int keepAliveTime = 1;

    /** Max queue size */
    private final int maxQueueSize = 10;

    /** Thread pool executor */
    private final ThreadPoolExecutor executor = new ThreadPoolExecutor(
            corePoolSize, maxPoolSize, keepAliveTime, TimeUnit.MINUTES,
            new ArrayBlockingQueue<Runnable>(maxQueueSize));
    {
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void publish(final Event event) {
        if (event == null) {
            throw new IllegalArgumentException("Cannot publish a null event");
        }

        executor.execute(new Runnable() {
            @Override
            public void run() {
                EventPublishRequest request = new EventPublishRequest(event);
                try {
                    RequestRouter.route(request);
                } catch (Exception e) {
                    // ignore failed
                }
            }
        });
    }

    /**
     * This method is not supported in CAVE and will throw and
     * UnsupportedOperationException.
     */
    @Override
    public Object register(Object subscriber) {
        throw new UnsupportedOperationException();
    }

    /**
     * This method is not supported in CAVE and will throw and
     * UnsupportedOperationException.
     */
    @Override
    public void unregister(Object subscriber) {
        throw new UnsupportedOperationException();
    }
}
