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
 * Interface use for callback actions when the set or fill buttons are
 * pressed.
 * @author lvenable
 */
public interface IColorWheelAction {
    /**
     * Method called when the Set button is pressed.
     * @param colorData Color data containing the RGB and alpha values.
     * @param colorWheelTitle Title of the color wheel component.  This is to
     *                        determine which color wheel component the color
     *                        data is coming from.
     */
    void setColor(ColorData colorData, String colorWheelTitle);

    /**
     * Method called when the Fill button is pressed.
     * @param colorData Color data containing the RGB and alpha values.
     */
    void fillColor(ColorData colorData);
}
