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
package com.raytheon.uf.viz.collaboration.comm.provider.account;

import com.raytheon.uf.viz.collaboration.comm.identity.roster.SubscriptionResponse;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * Interface for post request actions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 04, 2014    2785    mpduff      Initial creation
 * Apr 14, 2014 2903       bclement    moved from session subpackage to account
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public interface ISubscriptionRequestCompleteAction {
    /**
     * Called after subscription request is complete.
     * 
     * @param userId
     *            The originating userId
     * @param response
     *            The response
     */
    public void executeSubscriptionRequestComplete(UserId userId,
            SubscriptionResponse response);
}
