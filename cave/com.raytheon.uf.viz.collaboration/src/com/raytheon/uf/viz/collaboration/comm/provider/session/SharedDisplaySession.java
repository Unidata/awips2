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

import org.eclipse.ecf.core.IContainer;

import com.google.common.eventbus.EventBus;
import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.VenueInvite;
import com.raytheon.uf.viz.collaboration.comm.identity.user.ParticipantRole;
import com.raytheon.uf.viz.collaboration.comm.provider.TextMessage;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 18, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SharedDisplaySession extends VenueSession implements
        ISharedDisplaySession {

    private UserId sessionLeader = null;

    private UserId dataProvider = null;

    public SharedDisplaySession(IContainer container, EventBus externalBus,
            SessionManager manager) throws CollaborationException {
        super(container, externalBus, manager);
    }

    public SharedDisplaySession(IContainer container, EventBus externalBus,
            SessionManager manager, String sessionId)
            throws CollaborationException {
        super(container, externalBus, manager, sessionId);
    }

    @Override
    public void sendObjectToVenue(Object obj) throws CollaborationException {
        if (obj != null) {
            String message = Tools.marshallData(obj);
            if (message != null) {
                sendMessageToVenue(message);
            }
        }
    }

    @Override
    public void sendObjectToPeer(
            com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID participant,
            Object obj) throws CollaborationException {
        PeerToPeerChat session = getP2PSession();
        if (session != null) {
            String message = Tools.marshallData(obj);
            if (message != null) {

                TextMessage msg = new TextMessage(participant, message);
                msg.setProperty(Tools.PROP_SESSION_ID, getSessionId());

                session.sendPeerToPeer(msg);
            }
        }
    }

    /**
     * Get the identification of the user who is the DataProvider.
     * 
     * @return The DataProvider user identification.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#getCurrentDataProvider()
     */
    @Override
    public UserId getCurrentDataProvider() {
        return dataProvider;
    }

    /**
     * Get the identification of the user who is the Session Leader.
     * 
     * @return The Session Leader user identification.
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#getCurrentSessionLeader()
     */
    @Override
    public UserId getCurrentSessionLeader() {
        return sessionLeader;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.ISharedDisplaySession#hasRole(com.raytheon.uf.viz.collaboration.comm.identity.user.ParticipantRole)
     */
    @Override
    public boolean hasRole(ParticipantRole role) {
        boolean result = true;
        if (role.equals(ParticipantRole.DATA_PROVIDER)
                && !this.getUserID().equals(this.getCurrentDataProvider())) {
            result = false;
        } else if (role.equals(ParticipantRole.SESSION_LEADER)
                && !this.getUserID().equals(this.getCurrentSessionLeader())) {
            result = false;
        }
        return result;
    }

    @Override
    public void setCurrentSessionLeader(UserId id) {
        sessionLeader = id;
    }

    @Override
    public void setCurrentDataProvider(UserId id) {
        dataProvider = id;
    }

    @Override
    protected VenueInvite buildInvite(String msg) {
        VenueInvite invite = super.buildInvite(msg);
        invite.setDataProvider(this.getCurrentDataProvider());
        invite.setSessionLeader(this.getCurrentSessionLeader());
        return invite;
    }

}
