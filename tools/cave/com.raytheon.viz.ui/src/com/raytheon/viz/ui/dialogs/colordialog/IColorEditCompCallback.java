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
package com.raytheon.viz.ui.dialogs.colordialog;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;

/**
 * Callback for classes using the ColorEditComposite
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 18, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public interface IColorEditCompCallback extends IColorBarAction,
        IColorWheelAction {

    /**
     * Notify callback when colormap updates
     * 
     * @param newColorMap
     */
    public void updateColorMap(ColorMap newColorMap);

    /**
     * Callback should provide access to parameters
     * 
     * @return
     */
    public ColorMapParameters getColorMapParameters();

}
