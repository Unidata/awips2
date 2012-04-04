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
package com.raytheon.uf.viz.collaboration.ui.telestrator;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.viz.core.ColorUtil;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2012            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class TelestratorColorManager {

    private Map<String, RGB> colors;

    private static TelestratorColorManager colorManager = null;

    private static RGB[] rgbPresets = null;

    public static TelestratorColorManager getColorManager() {
        if (colorManager == null) {
            colorManager = new TelestratorColorManager();
            rgbPresets = ColorUtil.getResourceColorPresets();
        }
        return colorManager;
    }

    /**
     * 
     */
    private TelestratorColorManager() {
        if (colors == null) {
            colors = new HashMap<String, RGB>();
        }
    }

    /**
     * @return the colors
     */
    public Map<String, RGB> getColors() {
        return colors;
    }

    /**
     * Add a user with a new color value
     * 
     * @param user
     */
    public void addUser(String user) {
        int count = colors.size();
        if (rgbPresets.length <= colors.size()) {
            count = rgbPresets.length % colors.size();
        }
        colors.put(user, rgbPresets[count]);
    }

    public RGB getColorFromUser(String user) {
        if (colors.get(user) == null) {
            addUser(user);
        }
        return colors.get(user);
    }

}
