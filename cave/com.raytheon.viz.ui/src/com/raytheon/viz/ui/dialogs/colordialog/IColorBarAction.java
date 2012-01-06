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

/**
 * Interface use for callback action when the color bar is
 * queried for the color at the current mouse position.
 * @author lvenable
 *
 */
public interface IColorBarAction {
    /**
     * Method called to make any updates using the color and
     * alpha value from the color bar.
     * @param colorData Color data object containing the RGB color and
     *                  alpha value.
     * @param upperFlag True is the upper half of the color bar was queried,
     *                  false if the lower half was queried.
     */
    void updateColor(ColorData colorData, boolean upperFlag);
}
