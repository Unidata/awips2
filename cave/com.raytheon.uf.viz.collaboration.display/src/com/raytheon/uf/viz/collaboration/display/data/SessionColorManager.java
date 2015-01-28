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
package com.raytheon.uf.viz.collaboration.display.data;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
import com.raytheon.uf.viz.collaboration.display.Activator;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.viz.core.ColorUtil;

/**
 * 
 * Manages colors of different users for a session. Participants have different
 * colors for text and telestration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 03, 2012            mnash       Initial creation
 * Jan 30, 2014 2698       bclement    changed UserId to VenueParticipant
 * Mar 06, 2014 2848       bclement    synchronized color access
 * Jul 02, 2014 1255       bclement    collaboration specific RGB presets
 *                                      falls back to ColorUtil resource color presets
 * Jan 13, 2015 3709       bclement    implements IColorManager, uses UserColorInfo
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class SessionColorManager implements IColorManager<VenueParticipant> {

    public static final String SESSION_COLOR_PREFERENCE_KEY = "collaborationParticipantColor";

    protected final Map<VenueParticipant, UserColorInfo> colors = new HashMap<>();

    private static final RGB[] foregroundPresets;

    static {
        HierarchicalPreferenceStore prefs = (HierarchicalPreferenceStore) Activator
                .getDefault().getPreferenceStore();
        String[] names = prefs.getStringArray(SESSION_COLOR_PREFERENCE_KEY);
        if (names.length > 0) {
            foregroundPresets = new RGB[names.length];
            int i = 0;
            for (String name : names) {
                foregroundPresets[i++] = RGBColors.getRGBColor(name);
            }
        } else {
            foregroundPresets = ColorUtil.getResourceColorPresets();
        }
    }

    /**
     * Get a map of venue participants to their assigned foreground colors
     * 
     * @return
     */
    public Map<VenueParticipant, RGB> getForegroundColors() {
        /*
         * TODO the foreground specific methods are required since the shared
         * display protocol doesn't support changing the background color as
         * this would cause compatibility issues.
         */
        Map<VenueParticipant, RGB> rval;
        synchronized (colors) {
            rval = new HashMap<VenueParticipant, RGB>(colors.size());
            for (Entry<VenueParticipant, UserColorInfo> entry : colors
                    .entrySet()) {
                rval.put(entry.getKey(), entry.getValue().getForeground());
            }
        }
        return rval;
    }

    /**
     * Reassign foreground colors specified by map
     * 
     * @param map
     */
    public void setForegroundColors(Map<VenueParticipant, RGB> map) {
        synchronized (colors) {
            for (Entry<VenueParticipant, RGB> entry : map.entrySet()) {
                VenueParticipant participant = entry.getKey();
                UserColorInfo colorInfo = getColorInternal(participant);
                RGB foreground = entry.getValue();
                if (colorInfo != null) {
                    colorInfo.setForeground(foreground);
                } else {
                    setColorInternal(participant, new UserColorInfo(foreground));
                }
            }
        }
    }

    @Override
    public UserColorInfo getColorForUser(VenueParticipant user) {
        UserColorInfo rval;
        rval = getColorInternal(user);
        if (rval == null) {
            synchronized (colors) {
                int count = colors.size();
                if (foregroundPresets.length <= count) {
                    count = count % foregroundPresets.length;
                }
                rval = new UserColorInfo(foregroundPresets[count]);
                setColorInternal(user, rval);
            }
        }
        return rval;
    }

    /**
     * @param user
     * @return null if user isn't found
     */
    protected UserColorInfo getColorInternal(VenueParticipant user) {
        UserColorInfo rval = null;
        if (user != null) {
            synchronized (colors) {
                rval = colors.get(user);
            }
        }
        return rval;
    }

    /**
     * @param user
     * @param color
     */
    protected void setColorInternal(VenueParticipant user, UserColorInfo color) {
        synchronized (colors) {
            colors.put(user, color);
        }
    }

    @Override
    public void setColorForUser(VenueParticipant user, UserColorInfo color) {
        setColorInternal(user, color);
    }

    @Override
    public void clearColors() {
        synchronized (colors) {
            colors.clear();
        }
    }

    @Override
    public String getDescription(VenueParticipant user) {
        return "Color changes will apply to the user " + user.getName()
                + " only in the " + user.getRoom() + " room. "
                + "\nColor changes will be discarded when you leave the room.";
    }

}
