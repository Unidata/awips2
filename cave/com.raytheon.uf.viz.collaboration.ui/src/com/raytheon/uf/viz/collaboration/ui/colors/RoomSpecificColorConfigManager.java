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
package com.raytheon.uf.viz.collaboration.ui.colors;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.raytheon.uf.viz.collaboration.ui.colors.ColorInfoMap.ColorInfo;

/**
 * Keeps track of custom user color configurations for users in a particular
 * chat room
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 08, 2015 3709       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class RoomSpecificColorConfigManager extends
        PersistentColorConfigManager {

    private static final String ROOM_CONFIG_DIR = CONFIG_DIR_NAME
            + IPathManager.SEPARATOR + "roomColors";

    private final String roomId;

    private final String configFilePath;

    /**
     * @param roomId
     * @return
     */
    public static RoomSpecificColorConfigManager getManagerForRoom(String roomId) {
        /*
         * if multiple managers are created for the same room, it could cause
         * concurrency issues with writing to localization. This could be solved
         * with a soft reference cache here. However, since there *should* only
         * be one of these per room id, it might be overkill
         */
        return new RoomSpecificColorConfigManager(roomId);
    }

    /**
     * @param roomId
     */
    protected RoomSpecificColorConfigManager(String roomId) {
        this.roomId = roomId;
        this.configFilePath = ROOM_CONFIG_DIR + IPathManager.SEPARATOR + roomId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.AbstractColorConfigManager#setColors
     * (java.lang.String, org.eclipse.swt.graphics.RGB,
     * org.eclipse.swt.graphics.RGB)
     */
    @Override
    public synchronized void setColors(String participant, RGB foreground,
            RGB background) {
        super.setColors(participant, foreground, background, configFilePath);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.AbstractColorConfigManager#getColor
     * (java.lang.String)
     */
    @Override
    public synchronized ColorInfo getColor(String participant) {
        return super.getColor(participant, configFilePath);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.collaboration.ui.colors.IColorConfigManager#
     * getDescription()
     */
    @Override
    public String getDescription(String key) {
        VenueParticipant id = IDConverter.convertFromRoom(null, key);
        VenueId venue = VenueId.fromString(roomId);
        return "Color changes will apply to the user " + id.getName()
                + " only in the " + venue.getName() + " room.";
    }

}
