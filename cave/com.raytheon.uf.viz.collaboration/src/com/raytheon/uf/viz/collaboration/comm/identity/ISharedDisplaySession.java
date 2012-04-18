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

import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IVenueParticipant;
import com.raytheon.uf.viz.collaboration.comm.identity.user.ParticipantRole;

/**
 * 
 * 
 * <ul>
 * <li>EventBus subscription events. Implementors may to post the following
 * events.</li>
 * <ul>
 * <li><strong>IVenueParticipantEvent</strong> : This event is posted when a
 * venue participant enters, leaves a venue, or updates their status in the
 * venue.</li>
 * <li><strong>TextMessage</strong> : Text messages send between users. Meant to
 * be displayed as conversation.</li>
 * <li><strong>IDisplayEvent</strong> : These messages are CAVE to CAVE events</li>
 * <li><strong>IRenderable</strong> : These messages are CAVE to CAVE
 * display......</li>
 * <li><strong>IInitData</strong> : These messages are CAVE to CAVE
 * initialization data......</li>
 * <li><strong>IDisplayEvent</strong> : These messages are CAVE to CAVE
 * display......</li>
 * 
 * </ul>
 * </ul>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2012            jkorman     Initial creation
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
    public void sendObjectToPeer(IQualifiedID id, Object obj)
            throws CollaborationException;

    /**
     * Returns the current Data Provider for the session
     * 
     * @return
     */
    public IVenueParticipant getCurrentDataProvider();

    /**
     * Returns the current Session Leader for the session
     * 
     * @return
     */
    public IVenueParticipant getCurrentSessionLeader();

    /**
     * Sets the current Data Provider for the session
     * 
     * @param participant
     */
    public void setCurrentDataProvider(IVenueParticipant participant);

    /**
     * Sets the current Session Leader for the session
     * 
     * @param participant
     */
    public void setCurrentSessionLeader(IVenueParticipant participant);

    /**
     * Checks if the currently logged in user has the role on this session
     * 
     * @param role
     * @return
     */
    public boolean hasRole(ParticipantRole role);

    /**
     * Gets the connection status of the session.
     * 
     * @return The connection status.
     */
    public boolean isConnected();

    /**
     * Get the session identifier.
     * 
     * @return The session identifier.
     */
    public String getSessionId();

}
