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
package com.raytheon.uf.viz.collaboration.comm.identity.event;

import org.jivesoftware.smack.packet.Presence;

import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;

/**
 * Event fired when a venue participant has a change in status
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 20, 2012            jkorman     Initial creation
 * Dec 19, 2013 2563       bclement    added description getter
 * Jan 30, 2014 2698       bclement    changed UserId to VenueParticipant
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public interface IVenueParticipantEvent {

    /**
     * @return type of event
     */
    public ParticipantEventType getEventType();

    /**
     * @return user id of participant
     */
    public VenueParticipant getParticipant();

    /**
     * @return presence of participant, may be null
     */
    public Presence getPresence();

    /**
     * @return description of participant update event, may be null
     */
    public String getEventDescription();
}
