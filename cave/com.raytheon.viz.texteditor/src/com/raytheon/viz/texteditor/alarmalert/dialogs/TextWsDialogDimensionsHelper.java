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
package com.raytheon.viz.texteditor.alarmalert.dialogs;

import java.io.IOException;

import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.ui.preferences.ScopedPreferenceStore;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.SystemUtil;
import com.raytheon.viz.texteditor.Activator;

/**
 * Saves the position and size of TextWS dialogs across CAVE sessions. The
 * dimensions are saved independently for each user/workstation combination.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2018 7624       tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

public class TextWsDialogDimensionsHelper {

    private static TextWsDialogDimensionsHelper instance = null;

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private final ScopedPreferenceStore prefs = new ScopedPreferenceStore(
            InstanceScope.INSTANCE, Activator.PLUGIN_ID);

    private TextWsDialogDimensionsHelper() {
        // singleton
    }

    public static synchronized TextWsDialogDimensionsHelper getInstance() {
        if (instance == null) {
            instance = new TextWsDialogDimensionsHelper();
        }
        return instance;
    }

    private static String getPreferencePrefix(String key) {
        return SystemUtil.getHostName() + "." + key + ".dimensions";
    }

    /**
     * @param key
     * @return (xPos, yPos, width, height) rectangle, or null if no saved
     *         dimensions exist
     */
    public Rectangle getSavedDimensions(String key) {
        String prefix = getPreferencePrefix(key);
        int locX = prefs.getInt(prefix + ".x");
        int locY = prefs.getInt(prefix + ".y");
        int width = prefs.getInt(prefix + ".width");
        int height = prefs.getInt(prefix + ".height");
        Rectangle savedDimensions = null;
        if (width != 0 || height != 0) {
            savedDimensions = new Rectangle(locX, locY, width, height);
        }
        return savedDimensions;
    }

    /**
     * Save location and size of shell under the specified key
     *
     * @param dimensions
     * @param key
     */
    public void saveDimensions(Rectangle dimensions, String key) {
        /*
         * store preferences with host name prefix so each user has different
         * preferences for each workstation to allow for different monitor
         * configurations
         */
        String prefix = getPreferencePrefix(key);
        prefs.setValue(prefix + ".x", dimensions.x);
        prefs.setValue(prefix + ".y", dimensions.y);
        prefs.setValue(prefix + ".width", dimensions.width);
        prefs.setValue(prefix + ".height", dimensions.height);
        try {
            prefs.save();
        } catch (IOException e) {
            statusHandler.handle(Priority.ERROR,
                    "Failed to save window dimensions for " + key, e);
        }
    }

}
