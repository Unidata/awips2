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

import com.raytheon.uf.common.datadelivery.bandwidth.data.SubscriptionStatusSummary;

/**
 * Subscription service result object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2013   2292     mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SubscriptionServiceResult {
    /** Edit flag */
    private boolean allowFurtherEditing;

    /** Response message */
    private String message;

    /** The subscription status */
    private SubscriptionStatusSummary subscriptionStatusSummary;

    /**
     * Constructor.
     * 
     * @param message
     */
    public SubscriptionServiceResult(String message) {
        this(false, message);
    }

    /**
     * @param allowFurtherEditing
     * @param message
     */
    public SubscriptionServiceResult(boolean allowFurtherEditing, String message) {
        this.allowFurtherEditing = allowFurtherEditing;
        this.message = message;
    }

    /**
     * Constructor
     * 
     * @param allowFurtherEditing
     */
    public SubscriptionServiceResult(boolean allowFurtherEditing) {
        this(allowFurtherEditing, null);
    }

    /**
     * Constructor
     * 
     * @param message
     * @param subscriptionStatusSummary
     */
    public SubscriptionServiceResult(String message,
            SubscriptionStatusSummary subscriptionStatusSummary) {
        this.message = message;
        this.subscriptionStatusSummary = subscriptionStatusSummary;
    }

    /**
     * @return the allowFurtherEditing
     */
    public boolean isAllowFurtherEditing() {
        return allowFurtherEditing;
    }

    /**
     * @param allowFurtherEditing
     *            the allowFurtherEditing to set
     */
    public void setAllowFurtherEditing(boolean allowFurtherEditing) {
        this.allowFurtherEditing = allowFurtherEditing;
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * @param message
     *            the message to set
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * @return the subStatusSummary
     */
    public SubscriptionStatusSummary getSubscriptionStatusSummary() {
        return subscriptionStatusSummary;
    }

    /**
     * @param subscriptionStatusSummary
     *            the subStatusSummary to set
     */
    public void setSubscriptionStatusSummary(
            SubscriptionStatusSummary subscriptionStatusSummary) {
        this.subscriptionStatusSummary = subscriptionStatusSummary;
    }

    /**
     * @return true if message is not null
     */
    public boolean hasMessageToDisplay() {
        return message != null;
    }
}
