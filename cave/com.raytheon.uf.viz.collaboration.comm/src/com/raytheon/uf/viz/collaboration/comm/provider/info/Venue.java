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
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.ecf.core.identity.ID;
import org.eclipse.ecf.core.user.IUser;
import org.eclipse.ecf.presence.IPresence;
import org.eclipse.ecf.presence.IPresence.Type;
import org.eclipse.ecf.presence.Presence;
import org.eclipse.ecf.presence.chatroom.IChatRoomContainer;
import org.eclipse.ecf.presence.chatroom.IChatRoomInfo;

import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
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

    private final IChatRoomContainer container;

    private final IVenueInfo info;

    private Map<String, IPresence> presenceMap = new HashMap<String, IPresence>();;

    public Venue(IChatRoomContainer container, IChatRoomInfo info) {
        this.container = container;
        this.info = new VenueInfo(info);
    }

    @Override
    public IVenueInfo getInfo() {
        return info;
    }

    @Override
    public Collection<UserId> getParticipants() {
        Set<UserId> participants = new HashSet<UserId>();
        ID[] ids = container.getChatRoomParticipants();
        for (ID id : ids) {
            participants.add(IDConverter.convertFrom(id));
        }
        return participants;
    }

    @Override
    public IPresence getPresence(IUser user) {
        IPresence presence = presenceMap.get(user.getID().getName());
        if (presence == null) {
            presence = new Presence(Type.UNAVAILABLE);
        }
        return presence;
    }

    public void handlePresenceUpdated(ID fromID, IPresence presence) {
        presenceMap.put(fromID.getName(), presence);
    }

}
