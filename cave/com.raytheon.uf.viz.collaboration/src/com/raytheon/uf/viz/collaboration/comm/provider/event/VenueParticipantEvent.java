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

import com.raytheon.uf.viz.collaboration.comm.identity.IPresence;
import com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent;
import com.raytheon.uf.viz.collaboration.comm.identity.event.ParticipantEventType;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IVenueParticipant;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 20, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class VenueParticipantEvent implements IVenueParticipantEvent {

    private final ParticipantEventType eventType;

    private final IVenueParticipant participant;

    private IPresence presence;

    public VenueParticipantEvent(IVenueParticipant participant,
            ParticipantEventType eventType) {
        this.participant = participant;
        this.eventType = eventType;
    }

    public VenueParticipantEvent(IVenueParticipant participant,
            IPresence presence, ParticipantEventType eventType) {
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
    public IVenueParticipant getParticipant() {
        return participant;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.event.IVenueParticipantEvent#getPresence()
     */
    @Override
    public IPresence getPresence() {
        return presence;
    }

}
