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

import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;

/**
 * An event to put on a CollaborationConnection event bus when a users nickname
 * has changed to notify all views within this CAVE instance that they need to
 * refresh the displays for that user.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 25, 2012            bsteffen     Initial creation
 * Jan 30, 2014 2698       bclement    changed UserId to VenueParticipant
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class UserNicknameChangedEvent {

    public final VenueParticipant user;

    public final String nickname;

    public UserNicknameChangedEvent(VenueParticipant user, String nickname) {
        this.user = user;
        this.nickname = nickname;
    }

    public VenueParticipant getUser() {
        return user;
    }

    public String getNickname() {
        return nickname;
    }

}
