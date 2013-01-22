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
package com.raytheon.uf.edex.datadelivery.event.handler;

import java.util.Calendar;

import com.raytheon.uf.common.datadelivery.event.notification.NotificationRecord;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.IBaseSubscriptionHandler;
import com.raytheon.uf.common.datadelivery.service.BaseSubscriptionNotificationRequest;
import com.raytheon.uf.common.datadelivery.service.BaseSubscriptionNotificationResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Subscription Notification Handler.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 25, 2012            mpduff     Initial creation.
 * Aug 21, 2012     712    mpduff     Pass the subscription object from
 *                                    the request to the response.
 * Aug 31, 2012    1128    mpduff     Set priority and category from request.
 * Sep 06, 2012     687    mpduff     Send a SubscriptionNotificationResponse object.
 * Sep 24, 2012    1157    mpduff     Changed to use BaseSubscriptionNotificationRequest.
 * Jan 17, 2013 1501       djohnson     If a subscription is still in the registry, use it for the notification response.
 * Jan 21, 2013 1501       djohnson     Throw an exception if subscription is not provided on the request.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionNotificationHandler<T extends Subscription> extends
        AbstractHandler implements
        IRequestHandler<BaseSubscriptionNotificationRequest<T>> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionNotificationHandler.class);

    private final String uri;

    /**
     * Constructor
     * 
     * @param uri
     *            the jms uri to send the response
     */
    public SubscriptionNotificationHandler(String uri) {
        this.uri = uri;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public BaseSubscriptionNotificationResponse<T> handleRequest(
            BaseSubscriptionNotificationRequest<T> request) throws Exception {
        NotificationRecord record = new NotificationRecord();
        record.setDate(Calendar.getInstance());
        record.setCategory(request.getCategory());
        record.setUsername(request.getUserId());
        record.setPriority(request.getPriority());
        record.setMessage(request.getMessage());

        storeAndSend(record, uri);

        BaseSubscriptionNotificationResponse<T> response = request
                .getResponse();
        response.setMessage(request.getMessage());

        final IBaseSubscriptionHandler<T> subscriptionHandler = response
                .getSubscriptionHandler();
        final T requestSubscription = request.getSubscription();

        if (requestSubscription == null) {
            throw new IllegalArgumentException(
                    "Unable to send a notification for a null subscription!",
                    new NullPointerException("requestSubscription"));
        }

        final T registryVersion = subscriptionHandler
                .getByName(requestSubscription.getName());

        // If the subscription is still in the registry, use that version which
        // will reflect any updates that have occurred since the notification
        // was sent, otherwise pass along the one provided with the request
        response.setSubscription((registryVersion != null) ? registryVersion
                : requestSubscription);

        send(response, uri);

        return response;
    }
}
