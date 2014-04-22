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

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.collaboration.comm.provider.user.VenueParticipant;
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
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class SessionColorManager {

    private final Map<VenueParticipant, RGB> colors = new HashMap<VenueParticipant, RGB>();

    private static final RGB[] rgbPresets = ColorUtil.getResourceColorPresets();

    /**
     * Get a map of venue participants to their assigned colors used for
     * 
     * @return
     */
    public Map<VenueParticipant, RGB> getColors() {
        Map<VenueParticipant, RGB> rval;
        synchronized (colors) {
            rval = new HashMap<VenueParticipant, RGB>(colors);
        }
        return rval;
    }

    /**
     * Clear color assignments and repopulate with supplied map
     * 
     * @param map
     */
    public void setColors(Map<VenueParticipant, RGB> map) {
        synchronized (colors) {
            colors.clear();
            colors.putAll(map);
        }
    }

    /**
     * Get participant's assigned color
     * 
     * @param user
     * @return
     */
    public RGB getColorForUser(VenueParticipant user) {
        RGB rval;
        synchronized (colors) {
            rval = colors.get(user);
            if (rval == null) {
                int count = colors.size();
                if (rgbPresets.length <= count) {
                    count = count % rgbPresets.length;
                }
                rval = rgbPresets[count];
                colors.put(user, rval);
            }
        }
        return rval;
    }

    /**
     * Assign color to participant
     * 
     * @param id
     * @param rgb
     */
    public void setColorForUser(VenueParticipant id, RGB rgb) {
        synchronized (colors) {
            colors.put(id, rgb);
        }
    }

    /**
     * Clear color assignments
     */
    public void clearColors() {
        synchronized (colors) {
            colors.clear();
        }
    }
}
