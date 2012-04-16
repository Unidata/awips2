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
package com.raytheon.uf.viz.collaboration.comm.provider.session;

import java.util.Arrays;
import java.util.Map;

import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.util.ECFException;
import org.eclipse.ecf.presence.IPresenceContainerAdapter;
import org.eclipse.ecf.presence.IPresenceSender;
import org.eclipse.ecf.presence.roster.IRosterManager;
import org.eclipse.ecf.presence.roster.IRosterSubscriptionListener;

import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.ISubscriptionResponder;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.provider.Presence;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;

/**
 * TODO Add Description
 * 
 * <ul>
 * EventBus subscription events.
 * <li>ISubscriptionResponseEvent : This event is posted when a subscription
 * request has been responded to.</li>
 * </ul>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class AccountManager implements IAccountManager {

    private IRosterSubscriptionListener autoResponder = new IRosterSubscriptionListener() {

        @Override
        public void handleSubscribeRequest(ID fromID) {

            IQualifiedID fromId = null;

            IPresence.Type subscribedType = IPresence.Type.UNKNOWN;
            if (responder != null) {
                subscribedType = responder.handleSubscribeRequest(fromId);
            } else {
                subscribedType = IPresence.Type.SUBSCRIBED;
            }

            IPresence presence = new Presence(IPresence.Mode.AVAILABLE,
                    subscribedType, null);
            try {
                sendPresence(fromID, presence);
            } catch (CollaborationException e) {
                e.printStackTrace();
            }
        }

        @Override
        public void handleSubscribed(ID fromID) {
            System.out.println("AccountManager.handleSubscribed " + fromID);
        }

        @Override
        public void handleUnsubscribed(ID fromID) {
            System.out.println("AccountManager.handleUnSubscribed " + fromID);
        }
    };

    private boolean autoRespond = true;

    private IPresenceContainerAdapter presenceAdapter;

    private ISubscriptionResponder responder;

    private SessionManager sessionManager = null;

    /**
     * 
     * @param adapter
     */
    AccountManager(IPresenceContainerAdapter adapter, SessionManager manager) {
        sessionManager = manager;
        presenceAdapter = adapter;
        presenceAdapter.getRosterManager().addRosterSubscriptionListener(
                autoResponder);
    }

    /**
     * Set the auto subscription mode to ON or OFF. If set to off then any
     * currently assigned autoresponder is set to null.
     * 
     * @param mode
     *            The auto subscription mode.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#setAutoSubscriptionMode(boolean)
     */
    @Override
    public void setAutoSubscriptionMode(boolean auto) {
        autoRespond = auto;
        if (!auto) {
            responder = null;
        }
    }

    /**
     * 
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#getAutoSubscriptionMode()
     */
    @Override
    public boolean getAutoSubscriptionMode() {
        return autoRespond;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#setSubscriptionRequestResponder(com.raytheon.uf.viz.collaboration.comm.identity.roster.ISubscriptionResponder)
     */
    @Override
    public void setSubscriptionRequestResponder(ISubscriptionResponder responder) {
        this.responder = responder;
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#removeSubscriptionRequestResponder()
     */
    @Override
    public void removeSubscriptionRequestResponder() {
        responder = null;
    }

    /**
     * 
     * @param password
     *            The new password. For security the password is a character
     *            array that will be zero'd after use.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#changePassword(char[])
     */
    @Override
    public void changePassword(char[] password) throws CollaborationException {
        org.eclipse.ecf.presence.IAccountManager manager = presenceAdapter
                .getAccountManager();
        if (manager != null) {
            try {
                manager.changePassword(new String(password));
                // all done so clear the password.
                Arrays.fill(password, (char) 0);
            } catch (ECFException e) {
                throw new CollaborationException(
                        "Could not change account password");
            }
        }
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#deleteAccount()
     */
    @Override
    public void deleteAccount() throws CollaborationException {
        org.eclipse.ecf.presence.IAccountManager manager = presenceAdapter
                .getAccountManager();
        if (manager != null) {
            try {
                manager.deleteAccount();
            } catch (ECFException e) {
                throw new CollaborationException("Could not delete account");
            }
        }
    }

    /**
     * Determines if the server allows new accounts to be created by the user.
     * 
     * @throws CollaborationException
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#canCreateAccount()
     */
    @Override
    public boolean canCreateAccount() throws CollaborationException {
        boolean canCreate = false;
        org.eclipse.ecf.presence.IAccountManager manager = presenceAdapter
                .getAccountManager();
        if (manager != null) {
            try {
                canCreate = manager.isAccountCreationSupported();
            } catch (ECFException e) {
                throw new CollaborationException(
                        "Error attempting to determine if accounts may be created.");
            }
        }
        return canCreate;
    }

    /**
     * TODO : Body of method
     * 
     * @param password
     * @param attributes
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#createAccount(java.lang.String,
     *      char[], java.util.Map)
     */
    @SuppressWarnings("rawtypes")
    @Override
    public void createAccount(String name, char[] password,
            Map<String, String> attributes) throws CollaborationException {
        if (name != null) {
            if (password != null) {
                // create the account
                org.eclipse.ecf.presence.IAccountManager manager = presenceAdapter
                        .getAccountManager();
                if (manager != null) {
                    Map map = null;
                    if (attributes != null) {
                        map = (Map) attributes;
                    }

                    try {
                        manager.createAccount(name, new String(password), map);
                    } catch (ECFException e) {
                        throw new CollaborationException("Could not create account ");
                    }
                }
                // all done so clear the password.
                Arrays.fill(password, (char) 0);
            }
        }
    }

    /**
     * 
     * 
     * @param userPresence
     * @throws CollaborationException
     */
    @Override
    public void sendPresence(IPresence userPresence)
            throws CollaborationException {

        IRosterManager manager = presenceAdapter.getRosterManager();
        IPresenceSender sender = manager.getPresenceSender();

        try {
            sender.sendPresenceUpdate(null,
                    Presence.convertPresence(userPresence));
            sessionManager.setPresence(userPresence);
        } catch (ECFException e) {
            throw new CollaborationException("Could not send presence");
        }
    }

    /**
     * 
     * 
     * @param userPresence
     * @throws CollaborationException
     */
    public void sendPresence(ID toId, IPresence userPresence)
            throws CollaborationException {

        IRosterManager manager = presenceAdapter.getRosterManager();
        IPresenceSender sender = manager.getPresenceSender();

        try {
            sender.sendPresenceUpdate(toId,
                    Presence.convertPresence(userPresence));
            sessionManager.setPresence(userPresence);
        } catch (ECFException e) {
            throw new CollaborationException("Could not send presence");
        }
    }
    
    
    
}
