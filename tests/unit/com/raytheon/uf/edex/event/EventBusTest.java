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

import org.junit.Ignore;

/**
 * Test {@link EventBus}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 10, 2012 1104       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public class EventBusTest {

    /**
     * Creates a synchronous Google EventBus.
     */
    private static class SynchronousEventBusFactory implements
            GoogleEventBusFactory {
        @Override
        public com.google.common.eventbus.EventBus getEventBus() {
            return new com.google.common.eventbus.EventBus(
                    AsynchronousEventBusFactory.EVENT_BUS_NAME);
        }
    }

    /**
     * Overrides the {@link EventBus} instance to be a synchronous version, useful for unit testing.
     */
    public static void initSynchronous() {
        EventBus.eventBusFactory = new SynchronousEventBusFactory();
    }
}
