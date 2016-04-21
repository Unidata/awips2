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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;

/**
 * An event that indicates that the leader of a shared display session changed
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 11, 2014            njensen     Initial creation
 * Feb 19, 2014 2751       bclement    added oldLeader field
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@DynamicSerialize
public class LeaderChangeEvent {

    @DynamicSerializeElement
    private VenueParticipant newLeader;

    @DynamicSerializeElement
    private VenueParticipant oldLeader;

    public LeaderChangeEvent() {

    }

    public LeaderChangeEvent(VenueParticipant newLeader,
            VenueParticipant oldLeader) {
        this.newLeader = newLeader;
        this.oldLeader = oldLeader;
    }

    public VenueParticipant getNewLeader() {
        return newLeader;
    }

    public void setNewLeader(VenueParticipant newLeader) {
        this.newLeader = newLeader;
    }

    /**
     * @return the oldLeader
     */
    public VenueParticipant getOldLeader() {
        return oldLeader;
    }

    /**
     * @param oldLeader
     *            the oldLeader to set
     */
    public void setOldLeader(VenueParticipant oldLeader) {
        this.oldLeader = oldLeader;
    }

}
