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

import com.raytheon.uf.viz.collaboration.comm.identity.invite.VenueInvite;
import com.raytheon.uf.viz.collaboration.comm.identity.user.IQualifiedID;

/**
 * Encapsulates a venue invitation to the user.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 21, 2012            jkorman     Initial creation
 * Dec 18, 2013 2562       bclement    removed subject getter (subject in invite)
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public interface IVenueInvitationEvent {

    /**
     * room id for venue
     * 
     * @return id in {room}@conference.{host} format
     */
    public IQualifiedID getRoomId();

    /**
     * 
     * @return id of user that sent invitation
     */
    public IQualifiedID getInviter();

    /**
     * Get detailed invitation which includes subject and message if provided
     * 
     * @return
     */
    public VenueInvite getInvite();
}
