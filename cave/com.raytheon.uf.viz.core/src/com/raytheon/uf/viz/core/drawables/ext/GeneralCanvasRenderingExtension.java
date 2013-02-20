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

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.DrawableColorMap;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * A general implementation of the canvas rendering extension which converts
 * canvas relative coordinates into display coordinates.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 26, 2012            bsteffen     Initial creation
 * Feb 14, 2013 1616       bsteffen    Add option for interpolation of colormap
 *                                     parameters, disable colormap interpolation
 *                                     by default.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GeneralCanvasRenderingExtension extends
        GraphicsExtension<IGraphicsTarget> implements ICanvasRenderingExtension {

    @Override
    public void drawStrings(PaintProperties paintProps,
            DrawableString... parameters) throws VizException {
        IExtent extent = paintProps.getView().getExtent();
        double xRatio = extent.getWidth() / paintProps.getCanvasBounds().width;
        double yRatio = extent.getHeight()
                / paintProps.getCanvasBounds().height;

        List<DrawableString> mapStrings = new ArrayList<DrawableString>(
                parameters.length);
        for (DrawableString screenString : parameters) {
            DrawableString mapString = new DrawableString(
                    screenString.getText(), screenString.getColors());
            mapString.boxColor = screenString.boxColor;
            mapString.font = screenString.font;
            mapString.horizontalAlignment = screenString.horizontalAlignment;
            mapString.magnification = screenString.magnification;
            mapString.rotation = screenString.rotation;
            mapString.shadowColor = screenString.shadowColor;
            mapString.textStyle = screenString.textStyle;
            mapString.verticallAlignment = screenString.verticallAlignment;
            mapString.basics.alpha = screenString.basics.alpha;
            mapString.basics.xOrColors = screenString.basics.xOrColors;
            mapString.basics.color = screenString.basics.color;
            mapString.basics.x = extent.getMinX() + screenString.basics.x
                    * xRatio;
            mapString.basics.y = extent.getMinY() + screenString.basics.y
                    * yRatio;
            mapString.basics.z = screenString.basics.z;
            mapStrings.add(mapString);
        }
        target.clearClippingPlane();
        target.drawStrings(mapStrings);
        target.setupClippingPlane(paintProps.getClippingPane());
    }

    @Override
    public void drawLines(PaintProperties paintProps,
            DrawableLine... parameters) throws VizException {
        IExtent extent = paintProps.getView().getExtent();
        double xRatio = extent.getWidth() / paintProps.getCanvasBounds().width;
        double yRatio = extent.getHeight()
                / paintProps.getCanvasBounds().height;
        List<DrawableLine> mapLines = new ArrayList<DrawableLine>(
                parameters.length);
        for (DrawableLine screenLine : parameters) {
            DrawableLine mapLine = new DrawableLine();
            mapLine.lineStyle = screenLine.lineStyle;
            mapLine.width = screenLine.width;
            mapLine.basics.alpha = screenLine.basics.alpha;
            mapLine.basics.color = screenLine.basics.color;
            mapLine.basics.xOrColors = screenLine.basics.xOrColors;
            for (double[] point : screenLine.points) {
                double x = extent.getMinX() + point[0] * xRatio;
                double y = extent.getMinY() + point[1] * yRatio;
                mapLine.addPoint(x, y);
            }
            mapLines.add(mapLine);
        }
        target.clearClippingPlane();
        target.drawLine(mapLines.toArray(new DrawableLine[0]));
        target.setupClippingPlane(paintProps.getClippingPane());
    }

    @Override
    public void drawColorRamp(PaintProperties paintProps,
            DrawableColorMap colorMap) throws VizException {
        IExtent extent = paintProps.getView().getExtent();
        double xRatio = extent.getWidth() / paintProps.getCanvasBounds().width;
        double yRatio = extent.getHeight()
                / paintProps.getCanvasBounds().height;
        DrawableColorMap newColorMap = new DrawableColorMap(
                colorMap.getColorMapParams());
        newColorMap.alpha = colorMap.alpha;
        newColorMap.brightness = colorMap.brightness;
        newColorMap.contrast = colorMap.contrast;
        double x1 = colorMap.extent.getMinX();
        double y1 = colorMap.extent.getMinY();
        double x2 = colorMap.extent.getMaxX();
        double y2 = colorMap.extent.getMaxY();

        x1 = extent.getMinX() + x1 * xRatio;
        y1 = extent.getMinY() + y1 * yRatio;
        x2 = extent.getMinX() + x2 * xRatio;
        y2 = extent.getMinY() + y2 * yRatio;
        newColorMap.extent = new PixelExtent(x1, x2, y1, y2);
        target.clearClippingPlane();
        target.drawColorRamp(newColorMap);
        target.setupClippingPlane(paintProps.getClippingPane());
    }

    @Override
    public int getCompatibilityValue(IGraphicsTarget target) {
        return Compatibilty.GENERIC;
    }

}
