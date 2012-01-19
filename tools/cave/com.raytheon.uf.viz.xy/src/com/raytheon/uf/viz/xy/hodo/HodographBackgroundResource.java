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
package com.raytheon.uf.viz.xy.hodo;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.GenericResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.LineSegment;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 17, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class HodographBackgroundResource extends
        AbstractVizResource<GenericResourceData, HodographDescriptor> {

    /** The color of the map background */
    private static final RGB GREY = new RGB(155, 155, 155);

    private DrawableLine[] lines = null;

    private DrawableString[] labels = null;

    public HodographBackgroundResource(GenericResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    protected void disposeInternal() {

    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (lines != null) {
            target.drawLine(lines);
        }
        if (labels != null) {
            target.drawStrings(labels);
        }

        List<DrawableString> labels = new ArrayList<DrawableString>();
        for (int i = 90; i <= 360; i += 90) {
            DrawableString label = labelDirection(target, paintProps, i);
            if (label != null) {
                labels.add(label);
            }
        }
        if (!labels.isEmpty()) {
            target.drawStrings(labels);
        }
        // Paint the border
        IExtent extent = paintProps.getView().getExtent();
        target.drawRect(extent, GREY, 2.0f, 1.0);
    }

    private DrawableString labelDirection(IGraphicsTarget target,
            PaintProperties paintProps, int direction) {
        IExtent extent = paintProps.getView().getExtent();
        double padding = 3 * paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width;

        DrawableString label = new DrawableString(direction + "\u00B0", GREY);
        label.verticallAlignment = VerticalAlignment.MIDDLE;
        label.horizontalAlignment = HorizontalAlignment.CENTER;
        label.textStyle = TextStyle.BLANKED;
        double[] center = descriptor
                .worldToPixel(new double[] { 20, direction });
        double[] point = descriptor
                .worldToPixel(new double[] { 400, direction });
        LineSegment line = new LineSegment(center[0], center[1], point[0],
                point[1]);
        LineSegment bottom = new LineSegment(extent.getMinX(),
                extent.getMaxY(), extent.getMaxX(), extent.getMaxY());
        Coordinate intersection = line.intersection(bottom);
        if (intersection != null) {
            label.setCoordinates(intersection.x, intersection.y - padding);
            label.verticallAlignment = VerticalAlignment.BOTTOM;
            return label;
        }
        LineSegment top = new LineSegment(extent.getMinX(), extent.getMinY(),
                extent.getMaxX(), extent.getMinY());
        intersection = line.intersection(top);
        if (intersection != null) {
            label.setCoordinates(intersection.x, intersection.y + padding);
            label.verticallAlignment = VerticalAlignment.TOP;
            return label;
        }
        LineSegment left = new LineSegment(extent.getMinX(), extent.getMinY(),
                extent.getMinX(), extent.getMaxY());
        intersection = line.intersection(left);
        if (intersection != null) {
            label.setCoordinates(intersection.x + padding, intersection.y);
            label.horizontalAlignment = HorizontalAlignment.LEFT;
            return label;
        }
        LineSegment right = new LineSegment(extent.getMaxX(), extent.getMinY(),
                extent.getMaxX(), extent.getMaxY());
        intersection = line.intersection(right);
        if (intersection != null) {
            label.setCoordinates(intersection.x - padding, intersection.y);
            label.horizontalAlignment = HorizontalAlignment.RIGHT;
            return label;
        }
        return null;
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        if (lines == null) {
            List<DrawableLine> lineList = new ArrayList<DrawableLine>();
            List<DrawableString> labelList = new ArrayList<DrawableString>();
            for (int mag = 20; mag < 400; mag += 20) {
                DrawableLine circle = new DrawableLine();
                circle.basics.color = GREY;
                for (int dir = 0; dir <= 36; dir += 1) {
                    double[] screen = descriptor.worldToPixel(new double[] {
                            mag, dir * 10 });
                    circle.addPoint(screen[0], screen[1]);
                }
                lineList.add(circle);
                DrawableString label = new DrawableString(String.valueOf(mag),
                        GREY);
                double[] screen = descriptor.worldToPixel(new double[] { mag,
                        225 });
                label.setCoordinates(screen[0], screen[1]);
                label.textStyle = TextStyle.BLANKED;
                labelList.add(label);
            }
            // Add the lines
            for (int dir = 0; dir <= 8; dir += 1) {
                double[] screen1 = descriptor.worldToPixel(new double[] { 0,
                        dir * 45 });
                double[] screen2 = descriptor.worldToPixel(new double[] { 500,
                        dir * 45 });
                DrawableLine line = new DrawableLine();
                line.basics.color = GREY;
                line.addPoint(screen1[0], screen1[1]);
                line.addPoint(screen2[0], screen2[1]);
                lineList.add(line);

            }
            labels = labelList.toArray(new DrawableString[0]);
            lines = lineList.toArray(new DrawableLine[0]);
        }
    }

}
