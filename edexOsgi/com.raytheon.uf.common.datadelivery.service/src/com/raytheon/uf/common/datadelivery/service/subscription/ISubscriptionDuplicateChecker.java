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
 * Checks for duplication among subscriptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 02, 2013 2000       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public interface ISubscriptionDuplicateChecker {

    /**
     * Returns the percent, 0-100, of how many parameters from sub2 are
     * satisfied by sub1.
     * 
     * @param sub1
     * @param sub2
     * 
     * @return 0-100
     */
    int getParameterDuplicationPercent(Subscription sub1, Subscription sub2);

    /**
     * Returns the percent, 0-100, of how many forecast hours from sub2 are
     * satisfied by sub1.
     * 
     * @param sub1
     * @param sub2
     * 
     * @return 0-100
     */
    int getForecastHourDuplicationPercent(Subscription sub1, Subscription sub2);

    /**
     * Returns the percent, 0-100, of how many cycle hours from sub2 are
     * satisfied by sub1.
     * 
     * @param sub1
     * @param sub2
     * 
     * @return 0-100
     */
    int getCycleDuplicationPercent(Subscription sub2, Subscription sub1);

    /**
     * Returns the percent, 0-100, of how much spatial coverage from sub2 is
     * satisfied by sub1.
     * 
     * @param sub1
     * @param sub2
     * 
     * @return 0-100
     */
    int getSpatialDuplicationPercent(Subscription sub1, Subscription sub2);
}
