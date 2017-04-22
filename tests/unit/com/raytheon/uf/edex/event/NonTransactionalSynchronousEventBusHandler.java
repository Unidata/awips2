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

import java.util.Arrays;
import java.util.List;

import org.junit.Ignore;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.common.event.IEventBusHandler;

/**
 * {@link IEventBusHandler} which uses a synchronous {@link EventBus} and does
 * not participate in transactions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 5, 2013    1580     mpduff     Initial creation
 * May 28, 2013   1650     djohnson   Add getEventBuses.
 * Jun 20, 2013   1802     djohnson   Explicitly denote as non-transactional.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@Ignore
public class NonTransactionalSynchronousEventBusHandler extends EdexEventBusHandler {
    static class SynchronousEventBusFactory implements
            GoogleEventBusFactory {
        @Override
        public List<EventBus> getEventBuses() {
            return Arrays
                    .<EventBus> asList(new com.google.common.eventbus.EventBus(
                            AsynchronousEventBusFactory.EVENT_BUS_NAME));
        }
    }

    public NonTransactionalSynchronousEventBusHandler() {
        super(new SynchronousEventBusFactory());
    }

    /**
     * Overridden to return false, which will force events to be sent
     * immediately regardless of transaction status.
     */
    @Override
    protected boolean isTransactionActive() {
        return false;
    }

}
