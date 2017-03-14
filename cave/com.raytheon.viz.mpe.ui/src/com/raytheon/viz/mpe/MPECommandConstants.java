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
package com.raytheon.viz.mpe;

import org.eclipse.ui.IWorkbenchCommandConstants;

/**
 * Command constants for MPE, based off of {@link IWorkbenchCommandConstants}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2012            mschenke    Initial creation
 * Mar 19, 2013  1457      mpduff      Added gage toggle commands.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface MPECommandConstants {
    /**
     * Font command id.
     */
    public static final String FONT = "com.raytheon.viz.mpe.ui.actions.setfont";

    /**
     * Gage color command id.
     */
    public static final String GAGE_COLOR = "com.raytheon.viz.mpe.ui.actions.toggleGageColor";

    /**
     * Gage missing command id.
     */
    public static final String GAGE_MISSING_OPTION = "com.raytheon.viz.mpe.ui.actions.toggleGageMissing";

    /**
     * Display mode command id.
     */
    public static final String DISPLAY_MODE = "com.raytheon.viz.mpe.ui.actions.toggleDisplayMode";

    /**
     * Toggle Gage Id Command ID.
     */
    public static final String TOGGLE_GAGEID_COMMAND_ID = "com.raytheon.viz.mpe.ui.actions.toggleGageIdDisplay";

    /**
     * Toggle gage triangle command ID.
     */
    public static final String TOGGLE_GAGE_TRIANGLE_COMMAND_ID = "com.raytheon.viz.mpe.ui.actions.ToggleGageTriangleDisplay";

    /**
     * Toggle gage value Command ID.
     */
    public static final String TOGGLE_GAGE_VALUE_COMMAND_ID = "com.raytheon.viz.mpe.ui.actions.toggleGageValueDisplay";
}
