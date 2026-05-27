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
package com.raytheon.uf.common.event;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import com.google.common.eventbus.Subscribe;

/**
 * Base test for {@link EventBus} implementations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 28, 2013 1650       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public abstract class BaseEventBusTest<EVENT_TYPE, EVENT_BUS extends IBaseEventBusHandler<EVENT_TYPE>> {

    private final EVENT_BUS eventBus = getEventBus();

    private int eventsReceived;

    @After
    public void tearDown() {
        eventBus.unregister(this);
    }

    @Test
    public void subscriberReceivesEventWhileRegistered() {
        eventBus.register(this);

        eventBus.publish(getEvent());

        assertThat(eventsReceived, is(1));
    }

    @Test
    public void subscriberDoesNotReceiveEventWhileNotRegistered() {
        eventBus.publish(getEvent());

        assertThat(eventsReceived, is(0));
    }

    @Test
    public void subscriberRegisteredMultipleTimesStillReceivesOneEvent() {
        eventBus.register(this);
        eventBus.register(this);
        eventBus.register(this);

        eventBus.publish(getEvent());

        assertThat(eventsReceived, is(1));
    }

    @Test
    public void subscriberRegisteredMultipleTimesOnlyHasToUnregisterOnce() {
        eventBus.register(this);
        eventBus.register(this);
        eventBus.register(this);

        eventBus.unregister(this);
        eventBus.publish(getEvent());

        assertThat(eventsReceived, is(0));
    }

    @Test(expected = IllegalArgumentException.class)
    public void publishNullEventThrowsException() {
        eventBus.publish(null);
    }

    @Subscribe
    public void receivedEvent(EVENT_TYPE event) {
        eventsReceived++;
    }

    /**
     * Get the event bus under test.
     * 
     * @return the event bus
     */
    protected abstract EVENT_BUS getEventBus();

    /**
     * Get an event that can be used with the event bus.
     * 
     * @return the event
     */
    protected abstract EVENT_TYPE getEvent();
}
