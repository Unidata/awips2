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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.Presence;
import org.jivesoftware.smack.packet.Presence.Mode;
import org.jivesoftware.smack.packet.Presence.Type;
import org.jivesoftware.smack.util.StringUtils;
import org.jivesoftware.smackx.muc.Affiliate;
import org.jivesoftware.smackx.muc.MultiUserChat;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
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
 * Mar 1, 2012            jkorman     Initial creation
 * Dec  6, 2013 2561       bclement    removed ECF
 * Jan 28, 2014 2698       bclement    removed getInfo, added methods to replace
 * Jan 30, 2014 2698       bclement    changed UserId to VenueParticipant, getSubject never returns null
 * Feb 13, 2014 2751       bclement    changed to use VenueParticipant handle instead of alias
 * Mar 05, 2014 2798       mpduff      Get Presence from MUC.
 * Mar 06, 2014 2751       bclement    added getParticipantUserid()
 * Mar 07, 2014 2848       bclement    added hasOtherParticipants()
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class Venue implements IVenue {

    private final static IUFStatusHandler log = UFStatus
            .getHandler(Venue.class);

    private final MultiUserChat muc;

    private final Map<String, UserId> participantIdCache = new ConcurrentHashMap<String, UserId>();

    public Venue(XMPPConnection conn, MultiUserChat muc) {
        this.muc = muc;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue#getParticipants
     * ()
     */
    @Override
    public Collection<VenueParticipant> getParticipants() {
        List<VenueParticipant> participants = new ArrayList<VenueParticipant>();
        Iterator<String> iter = muc.getOccupants();
        while (iter.hasNext()) {
            String id = iter.next();
            participants.add(IDConverter.convertFromRoom(muc, id));
        }
        return participants;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue#getPresence
     * (com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant)
     */
    @Override
    public Presence getPresence(VenueParticipant user) {
        Presence presence = muc.getOccupantPresence(user.getFQName());
        if (presence == null) {
            presence = new Presence(Type.unavailable);
            presence.setMode(Mode.away);
        }
        return presence;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue#getId()
     */
    @Override
    public String getName() {
        return StringUtils.parseName(getId());
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue#getId()
     */
    @Override
    public String getId() {
        return muc.getRoom();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue#
     * getParticipantCount()
     */
    @Override
    public int getParticipantCount() {
        return muc.getOccupantsCount();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue#getSubject()
     */
    @Override
    public String getSubject() {
        String rval = muc.getSubject();
        return rval != null ? rval : "";
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.comm.identity.info.IVenue#
     * getParticipantUserid
     * (com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant)
     */
    @Override
    public UserId getParticipantUserid(VenueParticipant participant) {
        if (participant.hasActualUserId()) {
            return participant.getUserid();
        }
        UserId rval = participantIdCache.get(participant.getHandle());
        if (rval == null) {
            try {
                Collection<Affiliate> members = muc.getMembers();
                for (Affiliate member : members) {
                    if (!org.apache.commons.lang.StringUtils.isBlank(member
                            .getJid())) {
                        UserId id = IDConverter.convertFrom(member.getJid());
                        participantIdCache.put(member.getNick(), id);
                    }
                }
            } catch (XMPPException e) {
                log.error("Unable to get room member list from " + getName(), e);
            }
            rval = participantIdCache.get(participant.getHandle());
        }
        return rval;
    }

    /**
     * @return false if current user is the only participant in the venue
     */
    public boolean hasOtherParticipants() {
        // current user is included in participant count
        return getParticipantCount() > 1;
    }

}
