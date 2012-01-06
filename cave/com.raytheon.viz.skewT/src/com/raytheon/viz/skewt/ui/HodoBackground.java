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
package com.raytheon.viz.skewt.ui;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.common.sounding.WxMath;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.viz.core.graphing.WGraphics;
import com.raytheon.viz.skewt.Activator;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Draw the hodograph.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28Sept2008  #1529       dhladky    separate and improve.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class HodoBackground extends AbstractSkewTBackground {

    private double uMin;

    private double uMax;

    private double vMin;

    private double vMax;

    private PixelExtent pixExt;

    /**
     * Public constructor
     * 
     * @param target
     * @param world
     * @param paintProps
     * @throws VizException
     */
    public HodoBackground() {
        super();

        rectangle = new Rectangle(10, 730, 560, 480);
    }

    @Override
    public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        float zoomLevel = paintProps.getZoomLevel();

        pixExt = new PixelExtent(rectangle);

        // rings are offset to left of center of hodograph.
        // in D2-D this is offset so the 50 m/s ring touches the
        // left side of the display and the 90 m/s touches the right side of
        // the display. We adjust our display to mimic this.

        target.setupClippingPlane(pixExt);

        // draw the spokes.
        Coordinate c = new Coordinate(getWorld().mapX(0), getWorld().mapY(0));
        for (double angle = 0; angle < 2 * Math.PI; angle += Math.PI / 6) {
            double x = 200 * Math.cos(angle);
            double y = 200 * Math.sin(angle);
            target.drawLine(c.x, c.y, 0.0, getWorld().mapX(x), getWorld().mapY(
                    y), 0.0, SkewTConstants.moistAdiabatColor, 1,
                    LineStyle.SOLID);
        }
        // label the spokes

        target.drawString(smallFont, "180" + SkewTConstants.DEGREE_SYMBOL, c.x,
                rectangle.y, 0.0, TextStyle.BLANKED, SkewTConstants.labelColor,
                HorizontalAlignment.CENTER, VerticalAlignment.TOP, null);

        target.drawString(smallFont, "360" + SkewTConstants.DEGREE_SYMBOL, c.x,
                rectangle.y + rectangle.height, 0.0, TextStyle.BLANKED,
                SkewTConstants.labelColor, HorizontalAlignment.CENTER,
                VerticalAlignment.BOTTOM, null);

        target.drawString(smallFont, "90" + SkewTConstants.DEGREE_SYMBOL,
                rectangle.x + (2 * zoomLevel), rectangle.y + rectangle.height
                        / 2, 0.0, TextStyle.BLANKED, SkewTConstants.labelColor,
                HorizontalAlignment.LEFT, VerticalAlignment.MIDDLE, null);

        target.drawString(smallFont, "270" + SkewTConstants.DEGREE_SYMBOL,
                rectangle.x + rectangle.width - 10 * zoomLevel, rectangle.y
                        + rectangle.height / 2, 0.0, TextStyle.BLANKED,
                SkewTConstants.labelColor, HorizontalAlignment.RIGHT,
                VerticalAlignment.MIDDLE, null);

        for (int spd = 10; spd <= 100; spd += 10) {

            Coordinate c0, c1;
            c0 = WxMath.uvComp(spd, 0);
            for (int dir = 1; dir <= 360; dir += 1) {
                c1 = WxMath.uvComp(spd, dir);
                target.drawLine(getWorld().mapX(c0.x), getWorld().mapY(c0.y),
                        0.0, getWorld().mapX(c1.x), getWorld().mapY(c1.y), 0.0,
                        SkewTConstants.backgroundColor, 1);
                c0 = c1;
            }

            Coordinate uv = WxMath.uvComp(spd, 240);
            if (spd != 0) {
                target.drawString(smallFont, "" + spd, getWorld().mapX(uv.x),
                        getWorld().mapY(uv.y), 0.0, TextStyle.NORMAL,
                        SkewTConstants.labelColor, HorizontalAlignment.CENTER,
                        VerticalAlignment.MIDDLE, null);
            }
        }

        target.drawRect(pixExt, SkewTConstants.backgroundColor, 1.0f, 1.0f);

        target.clearClippingPlane();
        String label = "HODOGRAPH SPEED IN m/s";
        target.drawString(smallFont, label, rectangle.x + rectangle.width / 2,
                rectangle.y, 0.0, TextStyle.BLANKED, SkewTConstants.labelColor,
                HorizontalAlignment.CENTER, VerticalAlignment.BOTTOM, null);

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.skewt.ui.AbstractSkewTBackground#computeWorld()
     */
    @Override
    protected WGraphics computeWorld() {

        HierarchicalPreferenceStore prefs = Activator.getDefault()
                .getPreferenceStore();

        double d;
        uMin = ((d = prefs.getDouble("windRange.umin")) == 0.0 ? -50.0 : d);
        uMax = ((d = prefs.getDouble("windRange.umax")) == 0.0 ? 50.0 : d);
        vMin = ((d = prefs.getDouble("windRange.vmin")) == 0.0 ? -50.0 : d);
        vMax = ((d = prefs.getDouble("windRange.vmax")) == 0.0 ? 50.0 : d);

        WGraphics world = new WGraphics(rectangle);
        world.setWorldCoordinates(uMin, vMax, uMax, vMin);
        return world;
    }

    /**
     * @return the uMin
     */
    public double getUMin() {
        return uMin;
    }

    /**
     * @return the uMax
     */
    public double getUMax() {
        return uMax;
    }

    /**
     * @return the vMin
     */
    public double getVMin() {
        return vMin;
    }

    /**
     * @return the vMax
     */
    public double getVMax() {
        return vMax;
    }

    /**
     * @return the pixExt
     */
    public PixelExtent getPixExt() {
        return pixExt;
    }

}
