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

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.collaboration.comm.provider.user.IDConverter;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueId;
import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.raytheon.uf.viz.collaboration.ui.colors.ColorInfoMap.ColorInfo;

/**
 * Non-persistent color configuration manager
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 08, 2015  3709       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class TemporaryColorConfigManager implements IColorConfigManager {

    private final Map<String, ColorInfo> map = new HashMap<String, ColorInfoMap.ColorInfo>();

    private final String roomId;

    /**
     * 
     */
    public TemporaryColorConfigManager(String roomId) {
        this.roomId = roomId;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.colors.IColorConfigManager#setColors
     * (java.lang.String, org.eclipse.swt.graphics.RGB,
     * org.eclipse.swt.graphics.RGB)
     */
    @Override
    public void setColors(String key, RGB foreground, RGB background) {
        ColorInfo info = new ColorInfo();
        info.setColors(foreground, background);
        map.put(key, info);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.collaboration.ui.colors.IColorConfigManager#getColor
     * (java.lang.String)
     */
    @Override
    public ColorInfo getColor(String key) {
        return map.get(key);
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
                + " only in the " + venue.getName() + " room. "
                + "\nColor changes will be discarded when you leave the room.";
    }

}
