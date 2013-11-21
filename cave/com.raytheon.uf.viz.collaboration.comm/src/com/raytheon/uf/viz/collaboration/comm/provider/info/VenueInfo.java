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

import org.jivesoftware.smackx.muc.RoomInfo;

import com.raytheon.uf.viz.collaboration.comm.identity.info.IVenueInfo;
import com.raytheon.uf.viz.collaboration.comm.provider.Tools;

/**
 * 
 * Wrap a chatroom info in a VenueInfo
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class VenueInfo implements IVenueInfo {

    private RoomInfo info;

    public VenueInfo(RoomInfo info) {
        this.info = info;
    }

    @Override
    public String getVenueDescription() {
        return info.getDescription();
    }

    @Override
    public String getVenueName() {
        return Tools.parseName(info.getRoom());
    }

    @Override
    public String getVenueSubject() {
        return info.getSubject();
    }

    @Override
    public String getVenueID() {
        return info.getRoom();
    }

    @Override
    public int getParticipantCount() {
        return info.getOccupantsCount();
    }

    @Override
    public boolean isModerated() {
        return info.isModerated();
    }

    @Override
    public boolean isPersistent() {
        return info.isPersistent();
    }

    @Override
    public boolean requiresPassword() {
        return info.isPasswordProtected();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(String.format("[%s]:", getVenueName()));
        sb.append(String.format("[%s]:", getVenueID()));
        sb.append(String.format("mod[%s]:", (isModerated()) ? "T" : "F"));
        sb.append(String.format("pers[%s]:", (isPersistent()) ? "T" : "F"));
        sb.append(String.format("pass[%s]:", (requiresPassword()) ? "T" : "F"));
        sb.append(String.format("\n   subject     : %s", getVenueSubject()));
        sb.append(String.format("\n   description : %s", getVenueDescription()));

        return sb.toString();
    }
}
