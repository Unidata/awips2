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
package com.raytheon.uf.edex.plugin.text.subscription.util;

import java.util.Date;

import org.quartz.CronExpression;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.text.subscription.db.SubscriptionRecord;

/**
 * Utility for matching stored subscriptions to query triggers
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2014 2536       bclement     Initial creation
 * 12.13.14     DR 14638   dhuffman     Cut whitespace.
 * Jan 18, 2016 4562       tjensen      Moved from edex.plugin.text to
 *                                      edex.plugin.text.subscription
 * Dec  7, 2018 7674       tgurney      Allow product stored with abbreviated
 *                                      PIL to match a CCC trigger
 *
 * </pre>
 *
 * @author bclement
 */
public class TriggerMatcher {

    private static final Logger logger = LoggerFactory
            .getLogger(TriggerMatcher.class);

    /**
     * Determines if the specified trigger value matches the trigger value of
     * the provided SubscriptionRecord.
     *
     * @param record
     * @param trigger
     *            the trigger value to compare
     *
     * @return true if the trigger matches, false otherwise
     */
    public static boolean matches(SubscriptionRecord record, String trigger) {
        String type = record.getType();
        String recordTrigger = record.getTrigger();
        boolean retVal = false;
        if ("timer".equalsIgnoreCase(type)) {
            try {
                long date = Long.parseLong(trigger);
                CronExpression cexp = new CronExpression(recordTrigger);
                retVal = cexp.isSatisfiedBy(new Date(date));
            } catch (Exception e) {
                logger.warn("Failed to parse time trigger", e);
                retVal = false;
            }
        } else if ("ldad".equalsIgnoreCase(type)) {
            // Below, "subscriptionTrigger" is data from the subscription
            // database table.
            String subscriptionTrigger = recordTrigger.replaceAll("\\s+$", "");
            String requestTrigger = trigger.replaceAll("\\s+$", "");
            retVal = (requestTrigger.equalsIgnoreCase(subscriptionTrigger))
                    || ((requestTrigger.length() == 6)
                            && ("CCC" + requestTrigger)
                                    .equalsIgnoreCase(subscriptionTrigger))
                    || ((requestTrigger.substring(0, 6) + "XXX")
                            .equalsIgnoreCase(subscriptionTrigger))
                    || (("CCC" + requestTrigger.substring(3, 6) + "XXX")
                            .equalsIgnoreCase(subscriptionTrigger))
                    || ((requestTrigger.substring(0, 3) + "NNNXXX")
                            .equalsIgnoreCase(subscriptionTrigger))
                    || ((requestTrigger.substring(0, 3) + "NNN"
                            + requestTrigger.substring(6))
                                    .equalsIgnoreCase(subscriptionTrigger)
                            || (subscriptionTrigger
                                    .startsWith(requestTrigger)));
        } else {
            String pattern = recordTrigger.replaceAll("\\*", ".+");
            retVal = trigger.matches(pattern);
        }
        return retVal;
    }

}
