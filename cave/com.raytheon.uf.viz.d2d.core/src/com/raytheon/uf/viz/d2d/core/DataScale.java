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
package com.raytheon.uf.viz.d2d.core;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.viz.ui.VizWorkbenchManager;

/**
 * Class for holding whether data scale is selected for each window
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DataScale {

    private static Map<IWorkbenchWindow, Boolean> scaleMap = new HashMap<IWorkbenchWindow, Boolean>();

    /**
     * Set the data scale for the given workbench window
     * 
     * @param window
     * @param scale
     */
    public static void setDataScale(IWorkbenchWindow window, Boolean scale) {
        scaleMap.put(window, scale);
    }

    /**
     * Set the data scale for the active workbench window, used if you don't
     * know what window you are on
     * 
     * @param scale
     */
    public static void setDataScale(Boolean scale) {
        setDataScale(VizWorkbenchManager.getInstance().getCurrentWindow(),
                scale);
    }

    /**
     * Check if the data scale option is selected for the given window
     * 
     * @param window
     * @return
     */
    public static boolean isDataScale(IWorkbenchWindow window) {
        Boolean scaled = scaleMap.get(window);
        if (scaled == null) {
            scaled = false;
            scaleMap.put(window, scaled);
        }
        return scaled;
    }

    /**
     * Check if the data scaled option is selected for the active window
     * 
     * @return
     */
    public static boolean isDataScale() {
        return isDataScale(VizWorkbenchManager.getInstance().getCurrentWindow());
    }
}
