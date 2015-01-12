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
package com.raytheon.uf.viz.collaboration.comm.identity;

import java.util.Map;

import org.jivesoftware.smack.packet.Presence;

import com.raytheon.uf.viz.collaboration.comm.identity.roster.ISubscriptionResponder;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * Chat server account management interface
 * 
 * 
 * 
 * 
 * <ul>
 * EventBus subscription events.
 * <li>ISubscriptionResponseEvent : This event is posted when a subscription
 * request has been responded to.</li>
 * </ul>
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012            jkorman     Initial creation
 * Jan 27, 2014 2700       bclement    reworked responder methods
 *                                     added method to send presence without updating status
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public interface IAccountManager {


    /**
     * @return true if there is a request responder set to handle subscription
     *         requests and events. If there is not a responder set, the default
     *         action is to accept all requests.
     */
    public boolean isSubscriptionRequestResponderSet();

    /**
     * Set a responder to handle subscription requests and events (ie prompt
     * user for authorization)
     * 
     * @param responder
     */
    public void setSubscriptionRequestResponder(ISubscriptionResponder responder);

    /**
     * Removes the current subscription request responder. The default action
     * without a responder is to accept all requests.
     */
    public void removeSubscriptionRequestResponder();

    /**
     * 
     * @param name
     * @param password
     * @param attributes
     * @throws CollaborationException
     */
    public void createAccount(String name, char[] password,
            Map<String, String> attributes) throws CollaborationException;

    /**
     * Allows the user to change their account password. If the server does not
     * allow this operation an exception will be thrown.
     * 
     * @param password
     * @throws CollaborationException
     */
    public void changePassword(char[] password) throws CollaborationException;

    /**
     * Allows the user to delete this account on the server. An exception will
     * be thrown if the account deletion fails. If the account is currently
     * connected, it and any associated objects will be closed followed by the
     * account deletion.
     * 
     * @throws CollaborationException
     */
    public void deleteAccount() throws CollaborationException;

    /**
     * Can an account be created on the server.
     * 
     * @return Can an account be created on the server?
     * @throws CollaborationException
     *             The query failed.
     */
    public boolean canCreateAccount() throws CollaborationException;

    /**
     * Allow the user to send presence information to the transport provider.
     * 
     * @param presence
     * @return Return status information.
     * @throws CollaborationException
     */
    public void sendPresence(Presence presence) throws CollaborationException;

    /**
     * Send presence packet directly to user. This does not affect the account's
     * current presence.
     * 
     * @param toId
     * @param userPresence
     * @throws CollaborationException
     */
    public void sendPresence(UserId toId, Presence userPresence)
            throws CollaborationException;
}
