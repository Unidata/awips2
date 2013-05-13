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
package com.raytheon.uf.common.datadelivery.service.subscription;

import com.raytheon.uf.common.datadelivery.registry.Subscription;

/**
 * Checks subscriptions to see if they would be considered duplicates.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 09, 2013 2000       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface ISubscriptionOverlapService {

    int ONE_HUNDRED_PERCENT = 100;

    String OVERLAPPING_SUBSCRIPTIONS = "The following subscriptions overlap with this one "
            + "and are candidates for a shared subscription: ";

    /**
     * Response interface for a subscription overlap check.
     */
    public static interface ISubscriptionOverlapResponse {
        /**
         * Check whether the two subscriptions were exact duplicates.
         * 
         * @return true if the subscriptions are duplicates
         */
        boolean isDuplicate();

        /**
         * Check whether the two subscriptions were determined to overlap.
         * 
         * @return true if the subscriptions should be considered as overlapping
         *         according to the configuration
         */
        boolean isOverlapping();
    };

    /**
     * Returns whether sub2 exceeds configured overlap criteria to sub1
     * according to the configuration.
     * 
     * @param sub1
     * @param sub2
     * 
     * @return the overlap check response
     */
    ISubscriptionOverlapResponse isOverlapping(Subscription sub1,
            Subscription sub2);
}