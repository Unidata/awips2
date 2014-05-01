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
package com.raytheon.uf.common.datadelivery.service;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Thrift Request for subscriptions.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2012            mpduff      Initial creation.
 * Aug 21, 2012     712    mpduff      Add a Subscription object.
 * Aug 31, 2012    1128    mpudff      Add Category and Priority.
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public class SubscriptionNotificationRequest extends
        BaseSubscriptionNotificationRequest<Subscription> {

    /**
     * Default Constructor.
     */
    public SubscriptionNotificationRequest() {

    }

    @Override
    public BaseSubscriptionNotificationResponse<Subscription> getResponse() {
        return new SubscriptionNotificationResponse();
    }
}
