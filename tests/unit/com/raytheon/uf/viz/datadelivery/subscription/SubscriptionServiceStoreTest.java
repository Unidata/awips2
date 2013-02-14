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
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService.ISubscriptionServiceResult;

/**
 * Test 
 * {@link SubscriptionService#store(com.raytheon.uf.common.datadelivery.registry.Subscription, com.raytheon.viz.ui.presenter.IDisplay).
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2012 1286       djohnson     Initial creation
 *
 * </pre>
 *
 * @author djohnson
 * @version 1.0	
 */
public class SubscriptionServiceStoreTest extends
        AbstractSubscriptionServiceSingleSubscriptionTest {

    /**
     * {@inheritDoc}
     */
    @Override
    void returnTwoSubscriptionNamesWhenProposeScheduleCalled() {
        when(mockProposeScheduleResponse.getUnscheduledSubscriptions())
                .thenReturn(subNameResults);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    ISubscriptionServiceResult performServiceInteraction()
            throws RegistryHandlerException {
        return service.store(sub1, mockPromptDisplayText);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void verifyProposeScheduleCalled() {
        verify(mockBandwidthService).proposeSchedule(Arrays.asList(sub1));
    }

    @Override
    void verifyScheduleCalled() {
        verify(mockBandwidthService).schedule(Arrays.asList(sub1));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void returnZeroSubscriptionNamesWhenProposeScheduleCalled() {
        when(mockBandwidthService.schedule(Arrays.asList(sub1))).thenReturn(
                Collections.<String> emptySet());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void returnSub2NameWhenScheduleCalled() {
        when(mockBandwidthService.schedule(Arrays.asList(sub1))).thenReturn(
                CollectionUtil.asSet(sub2Name));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void returnZeroSubscriptionNamesWhenScheduleCalled() {
        when(mockBandwidthService.schedule(Arrays.asList(sub1))).thenReturn(
                Collections.<String> emptySet());
    }

    @Override
    String getExpectedForceApplyMessage() {
        return "The following subscriptions would not fully schedule with the bandwidth management system if this action were performed:\n"
                + sub1Name + "\n" + sub2Name + "\n\nWhat would you like to do?";
    }

    @Override
    String getSuccessfulServiceInteractionMessage() {
        return "Subscription " + sub1Name + " has been created.";
    }

    @Override
    void verifyServiceInteractionWithRegistryHandler()
            throws RegistryHandlerException {
        verify(DataDeliveryHandlers.getSubscriptionHandler()).store(sub1);
    }

    @Override
    void verifySubscriptionLatencyIsIncreasedToRequiredAmount()
    {
        assertEquals(
                "Expected the latency to have been set to the required amount!",
                REQUIRED_LATENCY, sub1.getLatencyInMinutes());
    }

    @Override
    String getExpectedSuccessfulForceApplyMessageWithSub2Unscheduled() {
        return "Subscription "
                + sub1Name
                + " has been created.\n\nThe following subscriptions did not fully schedule with the bandwidth management system:\n"
                + sub2Name;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void whenRegistryInteractionOccursThrowException()
            throws RegistryHandlerException {
        doThrow(new RegistryHandlerException("thrown on purpose")).when(
                DataDeliveryHandlers.getSubscriptionHandler()).store(sub1);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Subscription getExpectedDisplayForceApplyPromptSubscription() {
        return sub1;
    }
}
