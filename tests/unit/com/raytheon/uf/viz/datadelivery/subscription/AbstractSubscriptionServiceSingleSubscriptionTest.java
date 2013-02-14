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
package com.raytheon.uf.viz.datadelivery.subscription;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.mockito.Mockito.when;

import org.junit.Ignore;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlersUtil;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService.ISubscriptionServiceResult;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.ForceApplyPromptResponse;

/**
 * Provides additional tests for operations that take only a single
 * subscription, such as update(Subscription) and store(Subscription).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2012  1286       djohnson     Initial creation
 * Jan 02, 2012 1345       djohnson     Fix broken tests from using VizApp to move work off the UI thread.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public abstract class AbstractSubscriptionServiceSingleSubscriptionTest extends
        AbstractSubscriptionServiceTest {

    @Test
    public void testIncreaseLatencyCallsSchedule()
            throws RegistryHandlerException {
        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsIncreaseLatency();

        performServiceInteraction();

        verifyScheduleCalled();
    }

    @Test
    public void testIncreaseLatencyUpdatesSubscriptionLatency()
            throws RegistryHandlerException {
        final ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        subscriptionHandler.store(sub2);

        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        returnRequiredLatencyWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsIncreaseLatency();

        performServiceInteraction();

        verifySubscriptionLatencyIsIncreasedToRequiredAmount();
    }

    @Test
    public void testIncreaseLatencyInvokesRegistryHandler()
            throws RegistryHandlerException {
        RegistryObjectHandlersUtil.initMocks();

        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsIncreaseLatency();

        performServiceInteraction();

        verifyServiceInteractionWithRegistryHandler();
    }

    @Test
    public void testIncreaseLatencyReturnsSuccessMessage()
            throws RegistryHandlerException {
        final ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        subscriptionHandler.store(sub2);

        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsIncreaseLatency();
        returnZeroSubscriptionNamesWhenScheduleCalled();

        String expectedMessage = getSuccessfulServiceInteractionMessage();
        String actualMessage = performServiceInteraction()
                .getMessageToDisplay();

        assertEquals("Incorrect message returned", expectedMessage,
                actualMessage);
    }

    @Test
    public void testIncreaseLatencyDisallowsFurtherEdits()
            throws RegistryHandlerException {
        final ISubscriptionHandler subscriptionHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        subscriptionHandler.store(sub2);

        returnTwoSubscriptionNamesWhenProposeScheduleCalled();
        whenForceApplyPromptedUserSelectsIncreaseLatency();
        returnZeroSubscriptionNamesWhenScheduleCalled();

        ISubscriptionServiceResult result = performServiceInteraction();
        assertFalse("No further edits should be requested",
                result.isAllowFurtherEditing());
    }

    /**
     * When the force apply prompt is displayed, the user selects yes.
     */
    private void whenForceApplyPromptedUserSelectsIncreaseLatency() {
        when(mockDisplay.getForceApplyPromptResponse())
                .thenReturn(
                ForceApplyPromptResponse.INCREASE_LATENCY);
    }

    /**
     * Verifies that the latency on expected subscriptions
     */
    abstract void verifySubscriptionLatencyIsIncreasedToRequiredAmount();
}
