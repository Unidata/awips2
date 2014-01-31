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
package com.raytheon.uf.viz.collaboration.comm.identity.invite;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;

/**
 * Invitation to shared display venue
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2012            mnash     Initial creation
 * Jan 30, 2014 2698       bclement    changed UserId to VenueParticipant
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class SharedDisplayVenueInvite extends VenueInvite {

    @DynamicSerializeElement
    private VenueParticipant sessionLeader;

    @DynamicSerializeElement
    private VenueParticipant dataProvider;

    public VenueParticipant getSessionLeader() {
        return sessionLeader;
    }

    public void setSessionLeader(VenueParticipant sessionLeader) {
        this.sessionLeader = sessionLeader;
    }

    public VenueParticipant getDataProvider() {
        return dataProvider;
    }

    public void setDataProvider(VenueParticipant dataProvider) {
        this.dataProvider = dataProvider;
    }

}
