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

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;

import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.viz.datadelivery.subscription.ISubscriptionService.ISubscriptionServiceResult;

/**
 * Test 
 * {@link SubscriptionService#update(com.raytheon.uf.common.datadelivery.registry.Subscription, com.raytheon.viz.ui.presenter.IDisplay).
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
public class SubscriptionServiceUpdateTest extends SubscriptionServiceStoreTest {
    /**
     * {@inheritDoc}
     */
    @Override
    ISubscriptionServiceResult performServiceInteraction()
            throws RegistryHandlerException {
        return service.update(sub1, mockPromptDisplayText);
    }

    @Override
    void verifyServiceInteractionWithRegistryHandler()
            throws RegistryHandlerException {
        verify(DataDeliveryHandlers.getSubscriptionHandler()).update(sub1);
    }

    @Override
    String getExpectedForceApplyMessage() {
        return "The following subscriptions would not fully schedule with the bandwidth management system if this action were performed:\n"
                + sub1Name + "\n" + sub2Name + "\n\nWhat would you like to do?";
    }

    @Override
    String getSuccessfulServiceInteractionMessage() {
        return "Subscription " + sub1Name + " has been updated.";
    }

    @Override
    String getExpectedSuccessfulForceApplyMessageWithSub2Unscheduled() {
        return "Subscription "
                + sub1Name
                + " has been updated.\n\nThe following subscriptions did not fully schedule with the bandwidth management system:\n"
                + sub2Name;
    }

    @Override
    void whenRegistryInteractionOccursThrowException()
            throws RegistryHandlerException {
        doThrow(new RegistryHandlerException("thrown on purpose")).when(
                DataDeliveryHandlers.getSubscriptionHandler()).update(sub1);
    }
}
