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
package com.raytheon.uf.viz.collaboration.ui.data;

import java.util.Collection;
import java.util.Collections;

import com.raytheon.uf.viz.collaboration.comm.identity.CollaborationException;
import com.raytheon.uf.viz.collaboration.comm.provider.connection.CollaborationConnection;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueId;
import com.raytheon.uf.viz.collaboration.ui.Activator;

/**
 * Contributes bookmarked public rooms to the group view
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 8, 2014  3705      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class PublicRoomContainer extends TreeObjectContainer {

    public PublicRoomContainer() {
        super("Bookmarked Rooms", "session_group");
    }

    public Object[] getObjects() {
        CollaborationConnection connection = CollaborationConnection
                .getConnection();
        if (connection == null) {
            return new Object[0];
        }
        Collection<VenueId> bookmarkedRooms;
        try {
            bookmarkedRooms = connection.getBookmarkedRooms();
        } catch (CollaborationException e) {
            Activator.statusHandler.error(
                    "Unable to get bookmarked rooms from server", e);
            bookmarkedRooms = Collections.emptyList();
        }
        return bookmarkedRooms.toArray(new Object[0]);
    }
}
