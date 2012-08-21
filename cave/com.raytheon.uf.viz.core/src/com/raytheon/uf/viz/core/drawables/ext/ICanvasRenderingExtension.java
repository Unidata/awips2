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
package com.raytheon.uf.viz.core.drawables.ext;

import com.raytheon.uf.viz.core.DrawableColorMap;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension.IGraphicsExtensionInterface;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Contains methods for drawing on a target using canvas relative postions.
 * Useful for extra descriptions or information that are not drawn on a point of
 * the map but rather are on a specific portion of the display.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 26, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public interface ICanvasRenderingExtension extends IGraphicsExtensionInterface {

    /**
     * Draw the DrawableString object to the screen, the location of the
     * draqwable strings should be in canvas pixels which start at 0,0 in the
     * upper left.
     * 
     * @param parameters
     * @throws VizException
     */
    public abstract void drawStrings(PaintProperties paintProps,
            DrawableString... parameters) throws VizException;

    /**
     * Draw the DrawableLine object to the screen. The points in the line should
     * be in canvas pixels which start at 0,0 in the upper left.
     * 
     * @param parameters
     * @throws VizException
     */
    public abstract void drawLines(PaintProperties paintProps,
            DrawableLine... parameters) throws VizException;

    /**
     * Draw the drawable colormap to the screen, the extent of the colormap
     * should be in canvas pixels.
     * 
     * @param colorMap
     *            the colorMap to draw
     */
    public abstract void drawColorRamp(PaintProperties paintProps,
            DrawableColorMap colorMap) throws VizException;
}
