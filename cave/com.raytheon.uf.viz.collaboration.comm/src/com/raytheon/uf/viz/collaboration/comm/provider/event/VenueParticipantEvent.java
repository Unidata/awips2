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

import org.jivesoftware.smack.packet.Presence;

import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ParticipantEventType;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;

/**
 * Event that is posted when a participant's status changes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 20, 2012            jkorman     Initial creation
 * Dec 19, 2013 2563       bclement    added description
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class VenueParticipantEvent implements IVenueParticipantEvent {

    private final ParticipantEventType eventType;

    private final UserId participant;

    private Presence presence;

    private String eventDescription;

    public VenueParticipantEvent(UserId participant,
            ParticipantEventType eventType) {
        this.participant = participant;
        this.eventType = eventType;
    }

    public VenueParticipantEvent(UserId participant, Presence presence,
            ParticipantEventType eventType) {
        this.participant = participant;
        this.eventType = eventType;
        this.presence = presence;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent#getEventType()
     */
    @Override
    public ParticipantEventType getEventType() {
        return eventType;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent#getParticipant()
     */
    @Override
    public UserId getParticipant() {
        return participant;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent#getPresence()
     */
    @Override
    public Presence getPresence() {
        return presence;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent
     * #getEventDescription()
     */
    @Override
    public String getEventDescription() {
        return eventDescription;
    }

    /**
     * @param eventDescription
     *            the eventDescription to set
     */
    public void setEventDescription(String eventDescription) {
        this.eventDescription = eventDescription;
    }

}
