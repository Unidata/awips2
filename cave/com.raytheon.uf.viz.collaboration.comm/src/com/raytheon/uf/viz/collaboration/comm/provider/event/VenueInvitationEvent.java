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
package com.raytheon.uf.viz.collaboration.comm.provider.event;

import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.invite.VenueInvite;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;

/**
 * Event that is posted when a venue invitation is sent
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2012            jkorman     Initial creation
 * Dec 18, 2013 2562       bclement    removed subject (subject in invite)
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class VenueInvitationEvent implements IVenueInvitationEvent {

    private IQualifiedID venueId;

    private IQualifiedID invitor;

    private VenueInvite invite;

    /**
     * 
     * @param roomId
     * @param invitor
     * @param subject
     * @param body
     */
    public VenueInvitationEvent(IQualifiedID venueId, IQualifiedID invitor,
            VenueInvite invite) {
        this.venueId = venueId;
        this.invitor = invitor;
        this.invite = invite;
    }

    /**
     * Get the room identifier
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent#getRoomId()
     */
    @Override
    public IQualifiedID getRoomId() {
        return venueId;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueInvitationEvent#getInviter()
     */
    @Override
    public IQualifiedID getInviter() {
        return invitor;
    }

    public VenueInvite getInvite() {
        return invite;
    }

}
