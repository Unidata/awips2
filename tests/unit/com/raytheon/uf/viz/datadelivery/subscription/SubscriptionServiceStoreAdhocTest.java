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
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService.ISubscriptionServiceResult;

/**
 * Test 
 * {@link SubscriptionService#store(com.raytheon.uf.common.datadelivery.registry.AdhocSubscription, com.raytheon.viz.ui.presenter.IDisplay).
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
public class SubscriptionServiceStoreAdhocTest extends
        AbstractSubscriptionServiceSingleSubscriptionTest {

    @Override
    public void testFailedRegistryActionInvokesBandwidthManagerReinitialize()
            throws RegistryHandlerException {
        // Adhoc subscriptions no longer interact with the registry
    }

    /**
     * {@inheritDoc}
     */
    @Override
    ISubscriptionServiceResult performServiceInteraction()
            throws RegistryHandlerException {
        return service.store(adhoc, mockPromptDisplayText);
    }

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
    void verifyProposeScheduleCalled() {
        verify(mockBandwidthService).proposeSchedule(
                Arrays.<Subscription> asList(adhoc));
    }

    @Override
    void verifyScheduleCalled() {
        verify(mockBandwidthService).schedule(
                Arrays.<Subscription> asList(adhoc));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void returnZeroSubscriptionNamesWhenProposeScheduleCalled() {
        when(mockBandwidthService.schedule(Arrays.<Subscription> asList(adhoc)))
                .thenReturn(Collections.<String> emptySet());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void returnSub2NameWhenScheduleCalled() {
        when(mockBandwidthService.schedule(Arrays.<Subscription> asList(adhoc)))
                .thenReturn(CollectionUtil.asSet(sub2Name));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void returnZeroSubscriptionNamesWhenScheduleCalled() {
        when(mockBandwidthService.schedule(Arrays.<Subscription> asList(adhoc)))
                .thenReturn(Collections.<String> emptySet());
    }

    @Override
    String getExpectedForceApplyMessage() {
        return "The following subscriptions would not fully schedule with the bandwidth management system if this action were performed:\n"
                + sub1Name + "\n" + sub2Name + "\n\nWhat would you like to do?";
    }

    @Override
    String getSuccessfulServiceInteractionMessage() {
        return "The query was successfully stored.";
    }

    @Override
    void verifyServiceInteractionWithRegistryHandler()
            throws RegistryHandlerException {
        // Adhoc subscriptions no longer interact with the registry
    }

    @Override
    String getExpectedSuccessfulForceApplyMessageWithSub2Unscheduled() {
        return "The query was successfully stored.\n\nThe following subscriptions did not fully schedule with the bandwidth management system:\n"
                + sub2Name;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void whenRegistryInteractionOccursThrowException()
            throws RegistryHandlerException {
        // Adhoc subscriptions no longer interact with the registry
    }

    /**
     * {@inheritDoc}
     */
    @Override
    void verifySubscriptionLatencyIsIncreasedToRequiredAmount() {
        assertEquals(
                "Expected the latency to have been set to the required amount!",
                REQUIRED_LATENCY, adhoc.getLatencyInMinutes());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Subscription getExpectedDisplayForceApplyPromptSubscription() {
        return adhoc;
    }

}
