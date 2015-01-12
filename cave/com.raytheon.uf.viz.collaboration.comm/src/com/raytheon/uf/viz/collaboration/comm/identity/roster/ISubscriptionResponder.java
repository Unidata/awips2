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
package com.raytheon.uf.viz.collaboration.comm.identity.roster;

import com.raytheon.uf.viz.collaboration.comm.provider.account.ISubscriptionRequestCompleteAction;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * Interface for handling subscription invitation events from other users
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012            jkorman     Initial creation
 * Jan 27, 2014 2700       bclement    handle subscribe request returns a boolean
 *                                     all methods take user id instead of qualified id
 * Feb 13, 2014 2755       bclement    handleSubscribeRequest now returns SubscriptionResponse
 * Apr 07, 2014 2785       mpduff      Remove return type from and add parameter to handleSubscribeRequest
 *                                        Removed unused methods
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public interface ISubscriptionResponder {

    /**
     * Triggered when a contact requests a presence subscription
     * 
     * @param fromID
     * @param action
     */
    public void handleSubscribeRequest(UserId fromID,
            ISubscriptionRequestCompleteAction action);
}
