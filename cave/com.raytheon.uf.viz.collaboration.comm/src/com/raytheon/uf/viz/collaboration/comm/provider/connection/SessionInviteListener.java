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
package com.raytheon.uf.viz.collaboration.comm.provider.connection;

import org.apache.commons.lang.StringUtils;
import org.jivesoftware.smack.Connection;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smackx.muc.InvitationListener;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.xmpp.PacketConstants;
import com.raytheon.uf.viz.collaboration.comm.identity.IVenueSession;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.VenueInvite;
import com.raytheon.uf.viz.collaboration.comm.packet.SessionPayload;
import com.raytheon.uf.viz.collaboration.comm.packet.SessionPayload.PayloadType;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;
import com.raytheon.uf.viz.collaboration.comm.provider.event.VenueInvitationEvent;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueId;

/**
 * Handles incoming session invitations and routes to event bus
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2014 2903       bclement     Initial creation
 * May 19, 2014 3180       bclement     added alreadyParticipating()
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class SessionInviteListener implements InvitationListener {

    private static final IUFStatusHandler statusHandler = CollaborationConnection
            .getStatusHandler();

    private final CollaborationConnection manager;

    /**
     * @param manager
     */
    public SessionInviteListener(CollaborationConnection manager) {
        this.manager = manager;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.jivesoftware.smackx.muc.InvitationListener#invitationReceived(org
     * .jivesoftware.smack.Connection, java.lang.String, java.lang.String,
     * java.lang.String, java.lang.String,
     * org.jivesoftware.smack.packet.Message)
     */
    @Override
    public void invitationReceived(Connection conn, String room,
            String inviter, String reason, String password, Message message) {
        // TODO handle password protected rooms
        VenueId venueId = VenueId.fromString(room);
        UserId invitor = IDConverter.convertFrom(inviter);

        if (alreadyParticipating(venueId)) {
            statusHandler.debug("Invited to session we are already in: "
                    + venueId + " by " + inviter);
            return;
        }

        SessionPayload payload = null;
        if (message != null) {
            payload = (SessionPayload) message
                    .getExtension(PacketConstants.COLLAB_XMLNS);
        }
        if (reason != null && reason.startsWith(Tools.CMD_PREAMBLE)) {
            reason = "Shared display invitation from incompatible version of CAVE. "
                    + "Session will be chat-only if invitation is accepted";
        }
        if (payload != null) {
            handleCollabInvite(venueId, invitor, payload);
        } else {
            handleChatRoomInvite(venueId, invitor, reason, message);
        }
    }

    /**
     * @param venue
     * @return true if this user is already participanting in the venue
     */
    private static boolean alreadyParticipating(VenueId venue) {
        boolean rval = false;
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        for (IVenueSession session : connection.getJoinedVenueSessions()) {
            if (venue.isSameVenue(session.getVenue().getId())) {
                rval = true;
                break;
            }
        }
        return rval;
    }

    /**
     * Handles acceptance of a text only session invitation
     * 
     * @param venueId
     * @param invitor
     * @param reason
     * @param message
     */
    private void handleChatRoomInvite(VenueId venueId, UserId invitor,
            String reason, Message message) {
        VenueInvite invite = new VenueInvite();
        if (!StringUtils.isBlank(reason)) {
            invite.setMessage(reason);
        } else if (!StringUtils.isBlank(message.getBody())) {
            invite.setMessage(message.getBody());
        } else {
            invite.setMessage("");
        }
        invite.setSubject(message.getSubject());
        IVenueInvitationEvent event = new VenueInvitationEvent(venueId,
                invitor, invite);
        manager.postEvent(event);
    }

    /**
     * Handles acceptance of a shared display session invitation
     * 
     * @param venueId
     * @param invitor
     * @param payload
     */
    private void handleCollabInvite(VenueId venueId, UserId invitor,
            SessionPayload payload) {
        Object obj = payload.getData();
        if (obj == null
                || !payload.getPayloadType().equals(PayloadType.Invitation)
                || !(obj instanceof VenueInvite)) {
            statusHandler.warn("Received unsupported invite payload");
            return;
        }
        VenueInvite invite = (VenueInvite) obj;
        IVenueInvitationEvent event = new VenueInvitationEvent(venueId,
                invitor, invite);
        manager.postEvent(event);
    }

}
