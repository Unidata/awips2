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

import com.raytheon.uf.viz.collaboration.comm.identity.user.SharedDisplayRole;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;

/**
 * Interface for sessions that have displays shared between clients
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2012            jkorman     Initial creation
 * Jan 30, 2014 2698       bclement    changed UserId to VenueParticipant
 * Feb 13, 2014 2751       bclement    changed sendObjectToPeer id to VenueParticipant
 * Feb 13, 2014 2751       njensen     Added changeLeader()
 * Feb 19, 2014 2751       bclement    Added isClosed()
 * Apr 15, 2014 2822       bclement    added isSharedDisplayClient()
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public interface ISharedDisplaySession extends IVenueSession {

    /**
     * Sends the object to the other collaborators on the session. The object
     * must be serializable and once received by the others, will be posted to
     * the session's event bus.
     * 
     * @param obj
     *            the serializable object to send
     * @throws CollaborationException
     */
    public void sendObjectToVenue(Object obj) throws CollaborationException;

    /**
     * Sends the object to a specific collaborator on the session. The object
     * must be serializable and once received by the other, will be posted to
     * the session's event bus.
     * 
     * @param id
     *            the id of the collaborator to send to
     * @param obj
     *            the serializable object to send
     * @throws CollaborationException
     */
    public void sendObjectToPeer(VenueParticipant id, Object obj)
            throws CollaborationException;

    /**
     * Returns the current Data Provider for the session
     * 
     * @return
     */
    public VenueParticipant getCurrentDataProvider();

    /**
     * Returns the current Session Leader for the session
     * 
     * @return
     */
    public VenueParticipant getCurrentSessionLeader();

    /**
     * Sets the current Data Provider for the session
     * 
     * @param participant
     */
    public void setCurrentDataProvider(VenueParticipant participant);

    /**
     * Sets the current Session Leader for the session
     * 
     * @param participant
     */
    public void setCurrentSessionLeader(VenueParticipant participant);

    /**
     * Checks if the currently logged in user has the role on this session
     * 
     * @param role
     * @return
     */
    public boolean hasRole(SharedDisplayRole role);

    /**
     * Changes the leader (both Data Provider and Session Leader) of the
     * session. Throws an exception if the change fails or if this is called by
     * a non-leader.
     * 
     * @param newLeader
     * @throws CollaborationException
     */
    public void changeLeader(VenueParticipant newLeader)
            throws CollaborationException;

    /**
     * Returns true if the session has been closed
     * 
     * @return
     */
    public boolean isClosed();

    /**
     * @param participant
     * @return true if the participant is viewing the shared display
     */
    public boolean isSharedDisplayClient(VenueParticipant participant);

}
