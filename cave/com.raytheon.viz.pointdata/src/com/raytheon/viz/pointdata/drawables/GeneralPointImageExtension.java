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
package com.raytheon.viz.pointdata.drawables;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 7, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class GeneralPointImageExtension extends
        GraphicsExtension<IGraphicsTarget> implements IPointImageExtension {

    @Override
    public void drawPointImages(PaintProperties paintProps,
            PointImage... images) throws VizException {
        drawPointImages(paintProps, Arrays.asList(images));
    }

    @Override
    public int getCompatibilityValue(IGraphicsTarget target) {
        return Compatibilty.GENERIC.value;
    }

    @Override
    public void drawPointImages(PaintProperties paintProps,
            Collection<PointImage> images) throws VizException {
        List<DrawableImage> drawableImages = new ArrayList<DrawableImage>(
                images.size());
        double xScale = paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;
        double yScale = paintProps.getView().getExtent().getHeight()
                / paintProps.getCanvasBounds().height;

        for (PointImage image : images) {
            Double width = image.getWidth();
            Double height = image.getHeight();
            if (width == null) {
                width = (double) image.getImage().getWidth();
            }
            if (height == null) {
                height = (double) image.getImage().getHeight();
            }
            width = width * xScale;
            height = height * yScale;
            Coordinate center = new Coordinate(image.getX(), image.getY());
            if (image.getHorizontalAlignment().equals(HorizontalAlignment.LEFT)) {
                center.x += width / 2;
            } else if (image.getHorizontalAlignment().equals(
                    HorizontalAlignment.RIGHT)) {
                center.x -= width / 2;
            }
            if (image.getVerticalAlignment().equals(VerticalAlignment.TOP)) {
                center.y += height / 2;
            } else if (image.getVerticalAlignment().equals(
                    VerticalAlignment.BOTTOM)) {
                center.y -= height / 2;
            }
            PixelCoverage coverage = new PixelCoverage(center, width, height);
            DrawableImage drawableImage = new DrawableImage(image.getImage(),
                    coverage);
            drawableImages.add(drawableImage);
        }

        target.drawRasters(paintProps,
                drawableImages.toArray(new DrawableImage[0]));
    }
}
