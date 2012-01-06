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

import org.eclipse.swt.graphics.RGB;

/**
 * This is a convenience class that holds RGB and alpha values.
 * 
 * @author lvenable
 */
public class ColorData {
    /**
     * RGB color.
     */
    public RGB rgbColor;

    /**
     * Alpha value initialized to 255 (no transparency).
     */
    public int alphaValue = 255;

    /**
     * Constructor.
     * 
     * @param rgb
     *            Object with the RGB values.
     */
    public ColorData(RGB rgb) {
        rgbColor = rgb;
        alphaValue = 255;
    }

    /**
     * Constructor.
     * 
     * @param rgb
     *            Object with the RGB values.
     * @param alpha
     *            Alpha value.
     */
    public ColorData(RGB rgb, int alpha) {
        alphaValue = alpha;
        rgbColor = rgb;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ColorData other = (ColorData) obj;
        if (alphaValue != other.alphaValue)
            return false;
        if (rgbColor == null) {
            if (other.rgbColor != null)
                return false;
        } else if (!rgbColor.equals(other.rgbColor))
            return false;
        return true;
    }

}
