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

import java.util.Set;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.datadelivery.bandwidth.IProposeScheduleResponse;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.datadelivery.subscription.SubscriptionService.IForceApplyPromptDisplayText;

/**
 * Configuration for a force apply prompt.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2013 1650       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class ForceApplyPromptConfiguration {

    public final String title;

    public final String message;

    public final int requiredLatency;

    public final int maximumLatency;

    public final long maximumAllowedSize;

    public final IForceApplyPromptDisplayText displayTextStrategy;

    public final Subscription subscription;

    public final Set<String> wouldBeUnscheduledSubscriptions;

    /**
     * Constructor used when there is a single subscription failing to schedule.
     * 
     * @param title
     * @param message
     * @param requiredLatency
     * @param maximumLatency
     * @param maximumAllowedSize
     * @param displayTextStrategy
     * @param subscription
     * @param wouldBeUnscheduledSubscriptions
     */
    public ForceApplyPromptConfiguration(String title, String message,
            int requiredLatency, int maximumLatency, long maximumAllowedSize,
            IForceApplyPromptDisplayText displayTextStrategy,
            Subscription subscription,
            Set<String> wouldBeUnscheduledSubscriptions) {
        this.title = title;
        this.message = message;
        this.requiredLatency = requiredLatency;
        this.maximumLatency = maximumLatency;
        this.maximumAllowedSize = maximumAllowedSize;
        this.displayTextStrategy = displayTextStrategy;
        this.subscription = subscription;
        this.wouldBeUnscheduledSubscriptions = wouldBeUnscheduledSubscriptions;
    }

    /**
     * Constructor used when there are multiple subscriptions failing to
     * schedule.
     * 
     * @param title
     * @param message
     * @param displayTextStrategy
     */
    public ForceApplyPromptConfiguration(String title, String message,
            IForceApplyPromptDisplayText displayTextStrategy,
            Set<String> wouldBeUnscheduledSubscriptions) {
        this(title, message, IProposeScheduleResponse.VALUE_NOT_SET,
                IProposeScheduleResponse.VALUE_NOT_SET,
                IProposeScheduleResponse.VALUE_NOT_SET, displayTextStrategy,
                null, wouldBeUnscheduledSubscriptions);
    }

    /**
     * Returns true if the configuration would unschedule subscriptions other
     * than the one being modified/created.
     * 
     * @return true or false
     */
    public boolean hasUnscheduledSubscriptions() {
        return subscription == null
                || wouldBeUnscheduledSubscriptions.size() > 1
                || !wouldBeUnscheduledSubscriptions.contains(subscription
                        .getName());
    }

    /**
     * Returns true if the configuration contains bandwidth manager details.
     * 
     * @return true or false
     */
    public boolean hasBandwidthDetails() {
        return requiredLatency != IProposeScheduleResponse.VALUE_NOT_SET;
    }

    /**
     * Return true if the subscription attempting to be scheduled is the only
     * subscription that wouldn't be able to schedule.
     * 
     * @return true or false
     */
    public boolean isNotAbleToScheduleOnlyTheSubscription() {
        return subscription != null
                && wouldBeUnscheduledSubscriptions.size() == 1
                && wouldBeUnscheduledSubscriptions.contains(subscription
                        .getName());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof ForceApplyPromptConfiguration) {
            ForceApplyPromptConfiguration other = (ForceApplyPromptConfiguration) obj;

            EqualsBuilder builder = new EqualsBuilder();
            builder.append(this.title, other.title);
            builder.append(this.message, other.message);
            builder.append(this.requiredLatency, other.requiredLatency);
            builder.append(this.maximumLatency, other.maximumLatency);
            builder.append(this.maximumAllowedSize, other.maximumAllowedSize);
            builder.append(this.subscription, other.subscription);
            builder.append(this.wouldBeUnscheduledSubscriptions,
                    other.wouldBeUnscheduledSubscriptions);
            return builder.isEquals();
        }
        return super.equals(obj);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        HashCodeBuilder builder = new HashCodeBuilder();
        builder.append(this.title);
        builder.append(this.message);
        builder.append(this.requiredLatency);
        builder.append(this.maximumLatency);
        builder.append(this.maximumAllowedSize);
        builder.append(this.subscription);
        builder.append(this.wouldBeUnscheduledSubscriptions);
        return builder.toHashCode();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("title: ").append(title).append(StringUtil.NEWLINE);
        sb.append("message: ").append(message).append(StringUtil.NEWLINE);
        sb.append("required latency: ").append(requiredLatency)
                .append(StringUtil.NEWLINE);
        sb.append("maximum latency: ").append(maximumLatency)
                .append(StringUtil.NEWLINE);
        sb.append("maximum allowed size: ").append(maximumAllowedSize)
                .append(StringUtil.NEWLINE);
        sb.append("subscription: ")
                .append(subscription == null ? "null" : subscription.getName())
                .append(StringUtil.NEWLINE);
        sb.append("wouldBeUnscheduledSubscriptions: ").append(
                wouldBeUnscheduledSubscriptions);

        return sb.toString();
    }

}
