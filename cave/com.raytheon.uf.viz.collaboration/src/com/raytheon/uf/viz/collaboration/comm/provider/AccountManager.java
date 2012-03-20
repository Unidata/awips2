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
package com.raytheon.uf.viz.collaboration.comm.provider;

import java.util.Arrays;
import java.util.Map;

import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.util.ECFException;
import org.eclipse.ecf.presence.IPresenceContainerAdapter;
import org.eclipse.ecf.presence.roster.IRosterSubscriptionListener;

import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager;
import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.roster.ISubscriptionResponder;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;

/**
 * TODO Add Description
 * 
 * <ul>
 * EventBus subscription events.
 * <li>ISubscriptionResponseEvent : This event is posted when a subscription request has
 * been responded to.</li>
 * </ul>
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
            if(responder != null) {
                subscribedType = responder.handleSubscribeRequest(fromId);
            } else {
                subscribedType = IPresence.Type.SUBSCRIBED;
            }
            org.eclipse.ecf.presence.Presence.Type sType = null;
            switch(subscribedType) {
            case AVAILABLE: {
                sType = org.eclipse.ecf.presence.Presence.Type.AVAILABLE;
                break;
            }
            case ERROR: {
                sType = org.eclipse.ecf.presence.Presence.Type.ERROR;
                break;
            }
            case SUBSCRIBE: {
                sType = org.eclipse.ecf.presence.Presence.Type.SUBSCRIBE;
                break;
            }
            case SUBSCRIBED: {
                sType = org.eclipse.ecf.presence.Presence.Type.SUBSCRIBED;
                break;
            }
            case UNAVAILABLE: {
                sType = org.eclipse.ecf.presence.Presence.Type.UNAVAILABLE;
                break;
            }
            case UNSUBSCRIBE: {
                sType = org.eclipse.ecf.presence.Presence.Type.UNSUBSCRIBE;
                break;
            }
            case UNSUBSCRIBED: {
                sType = org.eclipse.ecf.presence.Presence.Type.UNSUBSCRIBED;
                break;
            }
            case UNKNOWN : {
                sType = org.eclipse.ecf.presence.Presence.Type.UNKNOWN;
                break;
            }
            default :  {
                sType = org.eclipse.ecf.presence.Presence.Type.ERROR;
                break;
            }
            }
            org.eclipse.ecf.presence.IPresence presence = new org.eclipse.ecf.presence.Presence(sType);
            
            try {
                presenceAdapter.getRosterManager().getPresenceSender().sendPresenceUpdate(fromID, presence);
            } catch (ECFException e) {
                // Will have to do something with this sooner or later.
            }
        }

        @Override
        public void handleSubscribed(ID fromID) {
        }

        @Override
        public void handleUnsubscribed(ID fromID) {
        }
    };
    
    private IPresenceContainerAdapter presenceAdapter;
    
    private ISubscriptionResponder responder;
    
    /**
     * 
     * @param adapter
     */
    AccountManager(IPresenceContainerAdapter adapter) {
        presenceAdapter = adapter;
    }
    
    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#setAutoSubscriptionMode(boolean)
     */
    @Override
    public void setAutoSubscriptionMode(boolean mode) {
        if(mode) {
            
        } else {
            responder = null;
        }
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#getAutoSubscriptionMode()
     */
    @Override
    public boolean getAutoSubscriptionMode() {
        return false;
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
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#changePassword(char[])
     */
    @Override
    public void changePassword(char[] password) throws CollaborationException {
        org.eclipse.ecf.presence.IAccountManager manager = presenceAdapter.getAccountManager();
        if(manager != null) {
            try {
                manager.changePassword(new String(password));
                // all done so clear the password.
                Arrays.fill(password, (char) 0);
            } catch (ECFException e) {
                throw new CollaborationException("Could not change account password");
            }
        }
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#deleteAccount()
     */
    @Override
    public void deleteAccount() throws CollaborationException {
        org.eclipse.ecf.presence.IAccountManager manager = presenceAdapter.getAccountManager();
        if(manager != null) {
            try {
                manager.deleteAccount();
            } catch (ECFException e) {
                throw new CollaborationException("Could not delete account");
            }
        }
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#canCreateAccount()
     */
    @Override
    public boolean canCreateAccount() throws CollaborationException {
        boolean canCreate = false;
        org.eclipse.ecf.presence.IAccountManager manager = presenceAdapter.getAccountManager();
        if(manager != null) {
            try {
                canCreate = manager.isAccountCreationSupported();
            } catch (ECFException e) {
                throw new CollaborationException("Could not delete account");
            }
        }
        return canCreate;
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.collaboration.comm.identity.IAccountManager#createAccount(java.lang.String, char[], java.util.Map)
     */
    @Override
    public void createAccount(String name, char[] password, Map attributes)
            throws CollaborationException {
        if (name != null) {
            if (password != null) {

                
                // all done so clear the password.
                Arrays.fill(password, (char) 0);
            }
        }
    }

}
