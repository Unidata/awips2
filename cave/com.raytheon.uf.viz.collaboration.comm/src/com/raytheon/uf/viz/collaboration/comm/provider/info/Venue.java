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
package com.raytheon.uf.viz.collaboration.comm.provider.info;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
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
 * Mar 1, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class Venue implements IVenue {

    private IVenueInfo info;

    private Map<String, UserId> participants;

    /**
     * 
     */
    public Venue() {
        participants = new HashMap<String, UserId>();
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue#getInfo()
     */
    @Override
    public IVenueInfo getInfo() {
        return info;
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue#setInfo(com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo)
     */
    @Override
    public void setInfo(IVenueInfo info) {
        this.info = info;
    }

    /**
     * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue#getParticipants()
     */
    @Override
    public Collection<UserId> getParticipants() {
        return participants.values();
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue#addParticipant(com.raytheon.uf.viz.collaboration.comm.identity.user.IVenueParticipant)
     */
    @Override
    public void addParticipant(UserId participant) {
        participants.put(participant.getName(), participant);
    }

    /**
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue#removeParticipant(com.raytheon.uf.viz.collaboration.comm.identity.user.IVenueParticipant)
     */
    @Override
    public void removeParticipant(UserId participant) {
        participants.remove(participant).getName();
    }

}
