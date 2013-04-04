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
import com.raytheon.uf.common.datadelivery.registry.handlers.IBaseSubscriptionHandler;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Base abstract class for the subscription notification response.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2012    1157    mpduff      Initial creation
 * Jan 17, 2013 1501       djohnson     Allow a response to specify the subscription handler.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@DynamicSerialize
public abstract class BaseSubscriptionNotificationResponse<T extends Subscription>
        implements ISerializableObject, IServerRequest {

    @DynamicSerializeElement
    private String message;

    @DynamicSerializeElement
    private T subscription;

    /**
     * @param message
     *            the message to set
     */

    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * @return the message
     */

    public String getMessage() {
        return message;
    }

    /**
     * @return the subscription
     */

    public T getSubscription() {
        return subscription;
    }

    /**
     * @param subscription
     *            the subscription to set
     */

    public void setSubscription(T subscription) {
        this.subscription = subscription;
    }

    /**
     * Get the subscription handler that corresponds to the subscription type
     * for this notification.
     * 
     * @return the subscription handler
     */
    public abstract IBaseSubscriptionHandler<T> getSubscriptionHandler();
}
