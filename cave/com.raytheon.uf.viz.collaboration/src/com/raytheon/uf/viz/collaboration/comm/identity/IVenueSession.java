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

import java.util.Collection;
import java.util.List;

import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageFilter;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IMessageListener;
import com.raytheon.uf.viz.collaboration.comm.identity.listener.IVenueParticipantListener;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 5, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public interface IVenueSession extends ISession {
    
    /**
     * Joins an existing multiple user collaboration.
     * @param venueName Name of the venue.
     */
    int joinVenue(String venueName);
    
    /**
     * Creates a multiple user collaboration.
     * @param venueName Name of the venue.
     * @param subject The subject of the collaboration.
     */
    int createVenue(String venueName, String subject);
    
    /**
     * Returns information about a venue. 
     * @return Information about a venue
     */
    IVenue getVenue();
    
    /**
     * Send a Text message to the venue.
     * @param message The message text to send.
     */
    int sendTextMessage(String message);

    /**
     * Send a Collaboration message.
     * @param message The message to send.
     */
    int sendCollaborationMessage(IMessage message);

    /**
     * Send a Collaboration message.
     * @param message The message to send.
     */
    int sendCollaborationMessage(String message);

    /**
     * Send an invitation from this venue to another user.
     * @param room The target venue for this invitation.
     * @param id The target user for this invitation.
     * @param subject The intended subject of the venue conversation.
     * @param body Any text that the user may wish to include.
     * @return
     */
    int sendInvitation(String room, String id, String subject, String body);
    
    /**
     * Send an invitation from this venue to another user.
     * @param room The target venue for this invitation.
     * @param ids A list of target users for this invitation.
     * @param subject The intended subject of the venue conversation.
     * @param body Any text that the user may wish to include.
     * @return
     */
    int sendInvitation(String room, List<String> ids, String subject, String body);
    /**
     * 
     * @param message
     */
    int sendMessageToVenue(String message);
    
    /**
     * Add a venue participant listener to this session.
     * @param listener The listener to add.
     * @return
     */
    public IVenueParticipantListener addVenueParticipantListener(IVenueParticipantListener listener);

    /**
     * Get the venue participant listeners defined for the session.
     * @return A not null collection of venue participant listeners defined for the session.
     */
    public Collection<IVenueParticipantListener> getVenueParticipantListeners();

    /**
     * Remove  a venue participant listener from this session.
     * @param listener A listener to remove.
     * @return The listener that was removed. If the listener was not
     * found, a null reference is returned.
     */
    public IVenueParticipantListener removeVenueParticipantListener(IVenueParticipantListener listener);
    
    /**
     * 
     * @param listener
     * @param filter
     * @return
     */
    public IMessageListener addCollaborationListener(IMessageListener listener, IMessageFilter filter);

    /**
     * Get the collaboration listeners defined for the session.
     * @return A not null collection of collaboration listeners defined for the session.
     */
    public Collection<IMessageListener> getCollaborationListeners();

    /**
     * Remove a collaboration listener from the session.
     * @param listener A listener to remove.
     * @return The listener that was removed. If the listener was not
     * found, a null reference is returned.
     */
    public IMessageListener removeCollaborationListener(IMessageListener listener);
    

}
