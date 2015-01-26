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

import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.raytheon.uf.viz.collaboration.display.data.SessionColorManager;
import com.raytheon.uf.viz.collaboration.display.data.UserColorInfo;

/**
 * Session color manager that persists colors to localization
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2015 3709       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class PersistentSessionColorManager extends SessionColorManager {

    private static final String ROOM_CONFIG_DIR = PersistentColorConfigStorage.CONFIG_DIR_NAME
            + IPathManager.SEPARATOR + "roomColors";

    private final PersistentColorConfigStorage<VenueParticipant> storage = new PersistentColorConfigStorage<VenueParticipant>() {
        @Override
        protected VenueParticipant convert(String persisted) {
            return IDConverter.convertFromRoom(persisted);
        }
    };

    private final String configFilePath;

    /**
     * @param roomId
     * @return
     */
    public static PersistentSessionColorManager getManagerForRoom(String roomId) {
        /*
         * if multiple managers are created for the same room, it could cause
         * concurrency issues with writing to localization. This could be solved
         * with a soft reference cache here. However, since there *should* only
         * be one of these per room id, it might be overkill
         */
        return new PersistentSessionColorManager(roomId);
    }

    /**
     * @param roomId
     */
    protected PersistentSessionColorManager(String roomId) {
        this.configFilePath = ROOM_CONFIG_DIR + IPathManager.SEPARATOR + roomId;
        Map<VenueParticipant, UserColorInfo> persistedColors = storage
                .getColors(configFilePath);
        colors.putAll(persistedColors);
    }

    @Override
    public String getDescription(VenueParticipant participant) {
        return "Color changes will apply to the user " + participant.getName()
                + " only in the " + participant.getRoom() + " room.";
    }

    @Override
    protected void setColorInternal(VenueParticipant user, UserColorInfo color) {
        synchronized (storage) {
            super.setColorInternal(user, color);
            storage.persistColors(colors, configFilePath);
        }
    }

    @Override
    public void clearColors() {
        synchronized (storage) {
            super.clearColors();
            storage.persistColors(colors, configFilePath);
        }
    }

}
