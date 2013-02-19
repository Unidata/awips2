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
package com.raytheon.uf.common.colormap;

import java.awt.image.IndexColorModel;
import java.util.List;

/**
 * Common utilities for colormaps
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 15, 2013 1638       mschenke     Initial creation
 *
 * </pre>
 *
 * @author mschenke
 * @version 1.0	
 */

public class ColorMapUtils {

    public static final int COLOR_MODEL_NUMBER_BITS = 8;

    public static final float MAX_VALUE = 255.0f;
    
    /**
     * Builds a color model from a color map
     * 
     * @param aColorMap
     * @return
     */
    public static IndexColorModel buildColorModel(IColorMap aColorMap) {
        int size = aColorMap.getSize();
        byte[] red = new byte[size];
        byte[] green = new byte[size];
        byte[] blue = new byte[size];
        byte[] alpha = new byte[size];

        List<Color> colors = aColorMap.getColors();
        for (int i = 0; i < size; ++i) {
            Color color = colors.get(i);
            red[i] = (byte) (color.getRed() * MAX_VALUE);
            green[i] = (byte) (color.getGreen() * MAX_VALUE);
            blue[i] = (byte) (color.getBlue() * MAX_VALUE);
            alpha[i] = (byte) (color.getAlpha() * MAX_VALUE);
        }

        return new IndexColorModel(COLOR_MODEL_NUMBER_BITS, size, red, green, blue, alpha);
    }
    
}
