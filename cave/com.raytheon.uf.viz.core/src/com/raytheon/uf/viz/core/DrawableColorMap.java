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
package com.raytheon.uf.viz.core;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;

/**
 * Object used to store information for drawing a color map to an
 * IGraphicsTarget
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 20, 2011            mschenke     Initial creation
 * Feb 14, 2013 1616       bsteffen    Add option for interpolation of colormap
 *                                     parameters, disable colormap interpolation
 *                                     by default.
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class DrawableColorMap {

    private ColorMapParameters colorMapParams;

    /** The extent the colormap should take up (in graphics world coordinates) */
    public IExtent extent;

    /** The alpha to draw at */
    public float alpha = 1.0f;

    /** The brightness to draw at */
    public float brightness = 1.0f;

    /** The contrast to draw at */
    public float contrast = 1.0f;

    /**
     * Uses the ColorMapParameters passed in for drawing color map
     * 
     * @param colorMapParams
     */
    public DrawableColorMap(ColorMapParameters colorMapParams) {
        this.colorMapParams = colorMapParams;
    }

    /**
     * Creates new ColorMapParameters for the IColorMap object
     * 
     * @param colorMap
     */
    public DrawableColorMap(IColorMap colorMap) {
        this.colorMapParams = new ColorMapParameters();
        this.colorMapParams.setColorMap(colorMap);
    }

    public ColorMapParameters getColorMapParams() {
        return colorMapParams;
    }

}
