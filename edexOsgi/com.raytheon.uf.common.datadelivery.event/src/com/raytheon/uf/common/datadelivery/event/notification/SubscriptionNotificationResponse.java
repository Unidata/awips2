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
package com.raytheon.uf.common.datadelivery.event.notification;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.IBaseSubscriptionHandler;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * SubscriptionNotificationResponse object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2012            mpduff     Initial creation.
 * Aug 21, 2012     712    mpduff     Add a Subscription Object.
 * Jan 17, 2013 1501       djohnson     Allow a response to specify the subscription handler.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class SubscriptionNotificationResponse extends
        BaseSubscriptionNotificationResponse<Subscription>{

    /**
     * {@inheritDoc}
     */
    @Override
    public IBaseSubscriptionHandler<Subscription> getSubscriptionHandler() {
        return DataDeliveryHandlers.getSubscriptionHandler();
    }
}
