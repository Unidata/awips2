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
package com.raytheon.uf.edex.datadelivery.retrieval.util;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.event.notification.NotificationRecord;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.datadelivery.event.notification.NotificationDao;

/**
 * A class to hold various subscription checks.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2013    2450    mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@Transactional
public class SubscriptionChecker {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionChecker.class);

    /** Notification DAO for sending/storing notification messages */
    private NotificationDao notificationDao;

    /** End point URI */
    private String uri;

    /**
     * Constructor.
     */
    public SubscriptionChecker() {

    }

    /**
     * Constructor with notification URI parameter.
     * 
     * @param uri
     *            Notification uri
     */
    public SubscriptionChecker(String uri) {
        this.uri = uri;
    }

    /**
     * Check for subscriptions that have expired in the last hour.
     */
    public void expirationCheck() {
        statusHandler.info("Starting expiration check...");
        ISubscriptionHandler handler = DataDeliveryHandlers
                .getSubscriptionHandler();
        List<Subscription> subList = null;
        try {
            subList = handler.getAll();

            List<Subscription> expiredList = getNewlyExpiredSubscriptions(
                    subList, TimeUtil.MILLIS_PER_HOUR);

            if (!expiredList.isEmpty()) {
                StringBuilder buffer = new StringBuilder(
                        "Expired Subscriptions: ");

                for (Subscription sub : expiredList) {
                    buffer.append(sub.getName()).append(" ");
                }

                NotificationRecord record = new NotificationRecord();
                record.setCategory("Subscription");
                record.setDate(TimeUtil.newGmtCalendar());
                record.setMessage(buffer.toString());
                record.setPriority(3);
                record.setUsername("System");

                // Send the notification
                storeAndSend(record, uri);
                statusHandler.debug(buffer.toString());
            }
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Check for subscriptions that will become inactive in the next 24 hours.
     */
    public void activePeriodEndCheck() {
        statusHandler.info("Starting active period check...");
        ISubscriptionHandler handler = DataDeliveryHandlers
                .getSubscriptionHandler();
        List<Subscription> subList = null;
        try {
            subList = handler.getAll();

            List<Subscription> stoppingList = getSubscriptionsNearingEndOfActivePeriod(subList);

            if (!stoppingList.isEmpty()) {
                StringBuilder buffer = new StringBuilder(
                        "Subscriptions becoming inactive in Next 24 hrs: ");

                for (Subscription sub : stoppingList) {
                    buffer.append(sub.getName()).append(" ");
                }

                NotificationRecord record = new NotificationRecord();
                record.setCategory("Subscription");
                record.setDate(TimeUtil.newGmtCalendar());
                record.setMessage(buffer.toString());
                record.setPriority(3);
                record.setUsername("System");

                // Send the notification
                storeAndSend(record, uri);
                statusHandler.debug(buffer.toString());
            }
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }

    }

    /**
     * Check for subscriptions that have expired within a time period defined by
     * expiredPeriod.
     * 
     * If expiredPeriod is one hour then a subscription will be flagged as
     * expired if it expired within the last hour
     * 
     * @param subList
     *            List of subscriptions to check
     * @param expiredPeriod
     *            amount of time for expiration, in milliseconds
     * 
     * @return List of subscriptions marked as expired
     */
    @VisibleForTesting
    List<Subscription> getNewlyExpiredSubscriptions(List<Subscription> subList,
            long expiredPeriod) {
        List<Subscription> expired = new ArrayList<Subscription>();
        statusHandler.debug("Checking for expired subs");
        if (subList != null && !subList.isEmpty()) {
            Date now = TimeUtil.newGmtCalendar().getTime();
            for (Subscription sub : subList) {
                Date end = sub.getSubscriptionEnd();
                if (end == null) {
                    continue;
                } else if (end.before(now)) {
                    if (now.getTime() - end.getTime() < expiredPeriod) {
                        expired.add(sub);
                    }
                }
            }
        }

        return expired;
    }

    /**
     * Check for subscriptions that are nearing the end of their active period.
     * 
     * @param subList
     *            List of subscriptions to check
     * @return List of subscriptions that are nearing the end of their active
     *         period
     */
    @VisibleForTesting
    List<Subscription> getSubscriptionsNearingEndOfActivePeriod(
            List<Subscription> subList) {
        List<Subscription> endingList = new ArrayList<Subscription>();
        statusHandler.debug("Checking for subs nearing end of active period");

        if (subList != null && !subList.isEmpty()) {
            Calendar now = TimeUtil.newGmtCalendar();
            Calendar cal = TimeUtil.newGmtCalendar();

            for (Subscription sub : subList) {
                Date end = sub.getActivePeriodEnd();
                if (end == null) {
                    continue;
                }

                cal.setTimeInMillis(end.getTime());
                cal.set(Calendar.YEAR, now.get(Calendar.YEAR));
                if (now.before(cal)
                        && (cal.getTimeInMillis() - now.getTimeInMillis() < TimeUtil.MILLIS_PER_DAY)) {
                    endingList.add(sub);
                }
            }
        }

        return endingList;
    }

    /**
     * Stores the record in the notification table.
     * 
     * @param record
     */
    private void storeAndSend(NotificationRecord record, String endpoint) {
        if (record != null) {
            notificationDao.createOrUpdate(record);
            try {
                byte[] bytes = SerializationUtil.transformToThrift(record);
                EDEXUtil.getMessageProducer().sendAsyncUri(endpoint, bytes);
            } catch (EdexException e) {
                statusHandler.error("Error sending record to " + endpoint, e);
            } catch (SerializationException e) {
                statusHandler.error("Error serializing record to " + endpoint,
                        e);
            }
        }
    }

    /**
     * @return the notificationDao
     */
    public NotificationDao getNotificationDao() {
        return notificationDao;
    }

    /**
     * @param notificationDao
     *            the notificationDao to set
     */
    public void setNotificationDao(NotificationDao notificationDao) {
        this.notificationDao = notificationDao;
    }
}
