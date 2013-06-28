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

import static com.raytheon.uf.common.event.TransactionalEventSendingService.OUTER_TRANSACTION_EVENT_MESSAGE;
import static com.raytheon.uf.common.event.TransactionalEventSendingService.REQUIRES_NEW_TRANSACTION_EVENT_MESSAGE;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.util.List;

import org.hibernate.HibernateException;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.google.common.collect.Lists;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.stats.ProcessEvent;
import com.raytheon.uf.common.util.SpringFiles;

/**
 * Test {@link EventBus} transactional behavior.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2013 1802       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { SpringFiles.UNIT_TEST_DB_BEANS_XML,
        EventBusTransactionBehaviorTest.SPRING_FILE })
public class EventBusTransactionBehaviorTest {

    protected static final String SPRING_FILE = "eventBusTransactionBehaviorTest.xml";

    @Autowired
    private TransactionalEventSendingService service;

    private final List<ProcessEvent> eventsReceived = Lists.newArrayList();

    @Before
    public void setUp() {
        EventBus.register(this);
    }

    @After
    public void tearDown() {
        EventBus.unregister(this);
    }

    /**
     * Tests that events published in an outer transaction are not made
     * available if that transaction is rolled back, even if a new transaction
     * is started in the meantime and commits successfully.
     */
    @Test
    public void rolledBackOuterTransactionEventsAreNotPublishedWhenRequiresNewTransactionCommits() {
        try {
            service.sendEventFromTransactionThenInvokeRequiresNewTransactionAndRollbackOriginal();

            fail("A HibernateException should have been thrown!");
        } catch (HibernateException ex) {
            // Expected path
        }

        boolean foundOuterTransactionEventMessage = findProcessEventWithMessage(OUTER_TRANSACTION_EVENT_MESSAGE);

        assertThat(
                "Should not have received the event sent during a transaction that rolled back!",
                foundOuterTransactionEventMessage, is(false));
    }

    /**
     * Tests that events published in an outer transaction are not made
     * available if that transaction is rolled back, even if a new transaction
     * is started in the meantime and commits successfully.
     */
    @Test
    public void committedOuterTransactionEventsArePublishedWhenTransactionCommits() {
        service.sendEventFromTransactionThenInvokeRequiresNewTransactionAndCommitOriginal();

        boolean foundOuterTransactionEventMessage = findProcessEventWithMessage(OUTER_TRANSACTION_EVENT_MESSAGE);

        assertThat(
                "Should have received the event sent during a transaction that committed!",
                foundOuterTransactionEventMessage, is(true));
    }

    /**
     * Tests that events published in an outer transaction are published when it
     * is committed, even if a requires new transaction is rolled back.
     */
    @Test
    public void committedOuterTransactionEventsArePublishedWhenTransactionCommitsAndRequiresNewTransactionRollsBack() {
        service.sendEventFromTransactionThenInvokeRequiresNewTransactionThatRollsBackAndCommitOriginal();

        boolean foundRequiresNewEventMessage = findProcessEventWithMessage(OUTER_TRANSACTION_EVENT_MESSAGE);

        assertThat(
                "Should have received the event sent during a transaction that committed, even when the requires new transaction inside of it rolls back!",
                foundRequiresNewEventMessage, is(true));
    }

    /**
     * Tests that events published in a requires new transaction are made
     * available if that transaction is committed, even if the outer transaction
     * is rolled back.
     */
    @Test
    public void requiresNewTransactionEventsArePublishedWhenOuterTransactionRollsBack() {
        try {
            service.sendEventFromTransactionThenInvokeRequiresNewTransactionAndRollbackOriginal();

            fail("A HibernateException should have been thrown!");
        } catch (HibernateException ex) {
            // Expected path
        }

        boolean foundRequiresNewEventMessage = findProcessEventWithMessage(REQUIRES_NEW_TRANSACTION_EVENT_MESSAGE);

        assertThat(
                "Should have received the event sent during a requires new transaction when the outer transaction rolls back!",
                foundRequiresNewEventMessage, is(true));
    }

    /**
     * Tests that events published in a requires new transaction are made
     * available if that transaction is committed, and the outer transaction is
     * committed as well.
     */
    @Test
    public void requiresNewTransactionEventsArePublishedWhenOuterTransactionCommits() {
        service.sendEventFromTransactionThenInvokeRequiresNewTransactionAndCommitOriginal();

        boolean foundRequiresNewEventMessage = findProcessEventWithMessage(REQUIRES_NEW_TRANSACTION_EVENT_MESSAGE);

        assertThat(
                "Should have received the event sent during a requires new transaction when the outer transaction rolls back!",
                foundRequiresNewEventMessage, is(true));
    }

    /**
     * Tests that events published in a requires new transaction are NOT made
     * available if that transaction is rolled back, even if the outer
     * transaction is committed.
     */
    @Test
    public void requiresNewTransactionEventsAreNotPublishedWhenItRollsBackAndOuterTransactionCommits() {
        service.sendEventFromTransactionThenInvokeRequiresNewTransactionThatRollsBackAndCommitOriginal();

        boolean foundRequiresNewEventMessage = findProcessEventWithMessage(REQUIRES_NEW_TRANSACTION_EVENT_MESSAGE);

        assertThat(
                "Should NOT have received the event sent during a requires new transaction when it rolls back!",
                foundRequiresNewEventMessage, is(false));
    }

    /**
     * @param outerTransactionEventMessage
     * @return
     */
    private boolean findProcessEventWithMessage(String message) {
        for (ProcessEvent event : eventsReceived) {
            if (message.equals(event.getMessage())) {
                return true;
            }
        }
        return false;
    }

    @Subscribe
    public void receivedEvent(ProcessEvent event) {
        eventsReceived.add(event);
    }
}
