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
package com.raytheon.uf.viz.collaboration.comm.identity.info;

import java.util.Collection;

import org.jivesoftware.smack.packet.Presence;

import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.provider.user.UserId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;

/**
 * Provides information about a venue.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 1, 2012             jkorman     Initial creation
 * Jan 28, 2014 2698       bclement    removed getInfo, added methods to replace
 * Jan 30, 2014 2698       bclement    changed UserId to VenueParticipant
 * Mar 06, 2014 2751       bclement    added getParticipantUserid()
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public interface IVenue {

    /**
     * @return list of users in venue
     */
    public Collection<VenueParticipant> getParticipants();

    /**
     * Get the presence for a user in the session.
     * 
     * @param participant
     * @return
     */
    public Presence getPresence(VenueParticipant participant);

    /**
     * @return id of venue "name@service"
     */
    public String getId();

    /**
     * @return name of venue
     */
    public String getName();

    /**
     * @return number of users in venue
     */
    public int getParticipantCount();

    /**
     * @return venue subject
     */
    public String getSubject();

    /**
     * Attempt to find actual userid for participant. The success of this method
     * may depend on admin rights in the venue.
     * 
     * @param participant
     * @return null if no userid was found
     * @throws CollaborationException
     */
    public UserId getParticipantUserid(VenueParticipant participant);

}
