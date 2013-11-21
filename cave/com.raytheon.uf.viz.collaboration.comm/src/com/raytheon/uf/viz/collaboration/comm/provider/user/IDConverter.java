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
package com.raytheon.uf.viz.collaboration.comm.provider.user;

import org.jivesoftware.smack.RosterEntry;
import org.jivesoftware.smack.util.StringUtils;
import org.jivesoftware.smackx.muc.MultiUserChat;
import org.jivesoftware.smackx.muc.Occupant;

/**
 * Utility to parse id strings
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012            jkorman     Initial creation
 * Dec  6, 2013 2561       bclement    removed ECF
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class IDConverter {

    private static final String CONF_ID = "conference.";

    public static UserId convertFrom(String id) {
        String name = StringUtils.parseName(id);
        String host = StringUtils.parseServer(id);
        String rsc = StringUtils.parseResource(id);
        UserId uid = new UserId(name, host, rsc);
        return uid;
    }

    public static UserId convertFrom(RosterEntry entry) {
        UserId rval = convertFrom(entry.getUser());
        rval.setAlias(entry.getName());
        return rval;
    }

    public static UserId convertFromRoom(MultiUserChat room, String id) {
        String nickname = StringUtils.parseResource(id);
        if (nickname == null || nickname.trim().isEmpty()) {
            // this message is from the room itself
            return convertFrom(id);
        }
        String host = StringUtils.parseServer(id);

        String name;
        Occupant occupant;
        if (room != null && (occupant = room.getOccupant(id)) != null) {
            // get actual user name
            name = StringUtils.parseName(occupant.getJid());
        } else {
            // fallback to using room nickname
            name = nickname;
        }

        return new UserId(name, host);
    }

    public static String normalizeHostname(String hostname) {
        if (hostname.startsWith(CONF_ID)) {
            return hostname.substring(CONF_ID.length());
        }
        return hostname;
    }

    public static boolean isFromRoom(String id) {
        String host = StringUtils.parseServer(id);
        return host.startsWith(CONF_ID);
    }

}
