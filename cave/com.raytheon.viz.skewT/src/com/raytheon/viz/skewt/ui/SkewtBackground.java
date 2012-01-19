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

import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.util.Iterator;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;

import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.edex.util.Equations;
import com.raytheon.edex.util.UAPoint;
import com.raytheon.uf.common.sounding.WxMath;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.graphing.WGraphics;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Used extensive work from SkewTGraph and associated codebase.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 14Jan2007   #682        ebabin     Update for sampling bug. 
 * 28Sept2008  #1529       dhladky    separate and improve.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class SkewtBackground extends AbstractSkewTBackground implements
        IRenderable {

    private static final UnitConverter kelvinToCelsius = SI.KELVIN
            .getConverterTo(SI.CELSIUS);

    public SkewtBackground() {
        super();

        this.rectangle = new Rectangle(10, 10, 690, 690);
    }

    /**
     * Draws the adiabat lines.
     * 
     * @throws VizException
     */
    private void drawAdiabats(IGraphicsTarget target) throws VizException {

        for (Iterator<List<UAPoint>> iterator = SkewTConstants.saturatedPoints
                .iterator(); iterator.hasNext();) {
            List<UAPoint> points = iterator.next();
            UAPoint firstPoint = points.get(0);
            Coordinate coor1 = WxMath.getSkewTXY(firstPoint.pressure,
                    kelvinToCelsius.convert(firstPoint.temperature));
            for (Iterator<UAPoint> iter = points.iterator(); iter.hasNext();) {
                UAPoint p = iter.next();
                Coordinate coor2 = WxMath.getSkewTXY(p.pressure,
                        kelvinToCelsius.convert(p.temperature));
                target.drawLine(getWorld().mapX(coor1.x), getWorld().mapY(
                        coor1.y), 0.0, getWorld().mapX(coor2.x), getWorld()
                        .mapY(coor2.y), 0.0, SkewTConstants.moistAdiabatColor,
                        1, IGraphicsTarget.LineStyle.DOTTED);
                coor1 = coor2;
            }
        }
        for (Iterator<List<UAPoint>> iterator = SkewTConstants.dryPoints
                .iterator(); iterator.hasNext();) {
            List<UAPoint> points = iterator.next();
            UAPoint firstPoint = points.get(0);
            Coordinate startCoor = WxMath.getSkewTXY(firstPoint.pressure,
                    kelvinToCelsius.convert(firstPoint.temperature));
            for (Iterator<UAPoint> iter = points.iterator(); iter.hasNext();) {
                UAPoint p = iter.next();
                Coordinate endCoor = WxMath.getSkewTXY(p.pressure,
                        kelvinToCelsius.convert(p.temperature));
                target.drawLine(getWorld().mapX(startCoor.x), getWorld().mapY(
                        startCoor.y), 0.0, getWorld().mapX(endCoor.x),
                        getWorld().mapY(endCoor.y), 0.0,
                        SkewTConstants.dryAdiabatColor, 1);
                startCoor = endCoor;
            }
        }
    }

    /**
     * Draws the temperature lines.
     * 
     * @throws VizException
     */
    private void drawTempLines(IGraphicsTarget target) throws VizException {

        Line2D.Double rightClipLine = new Line2D.Double(getWorld()
                .getViewXmax() - 20, getWorld().getViewYmin(), getWorld()
                .getViewXmax() - 20, getWorld().getViewYmax());

        Line2D.Double topClipLine = new Line2D.Double(getWorld().getViewXmin(),
                getWorld().getViewYmin() + 60, getWorld().getViewXmax(),
                getWorld().getViewYmin() + 60);

        for (int i = 70; i > -200; i -= 10) {
            Coordinate coorStart = WxMath.getSkewTXY(1050, i);
            Coordinate coorEnd = WxMath.getSkewTXY(100, i);

            target.drawLine(getWorld().mapX(coorStart.x), getWorld().mapY(
                    coorStart.y), 0.0, getWorld().mapX(coorEnd.x), getWorld()
                    .mapY(coorEnd.y), 0.0, SkewTConstants.temperatureColor, 1);

            double startX = getWorld().mapX(coorStart.x);
            double startY = getWorld().mapY(coorStart.y);

            double endX = getWorld().mapX(coorEnd.x);
            double endY = getWorld().mapY(coorEnd.y);

            Line2D.Double tempLine = new Line2D.Double(startX, startY, endX,
                    endY);

            drawLabelAtLineIntersections(target, tempLine, rightClipLine,
                    Integer.toString(i));
            drawLabelAtLineIntersections(target, tempLine, topClipLine, Integer
                    .toString(i));
        }
    }

    /**
     * Draws the presssure lines.
     * 
     * @throws VizException
     */
    private void drawPressureLines(IGraphicsTarget target) throws VizException {
        String s = null;

        for (int i = 0; i < SkewTConstants.MAN_LEVELS.length; i++) {
            Coordinate coor = WxMath.getSkewTXY(SkewTConstants.MAN_LEVELS[i],
                    -50);
            coor.x = getWorld().unMap(getWorld().getViewXmin(), 0).x;
            Coordinate coorEnd = WxMath.getSkewTXY(
                    SkewTConstants.MAN_LEVELS[i], 60);
            coorEnd.x = getWorld().unMap(740, 0).x;
            target.drawLine(getWorld().mapX(coor.x), getWorld().mapY(coor.y),
                    0.0, getWorld().mapX(coorEnd.x),
                    getWorld().mapY(coorEnd.y), 0.0,
                    SkewTConstants.pressureColor, 1);
            s = SkewTConstants.pressFormat.format(SkewTConstants.MAN_LEVELS[i]);
            if (i != 0 && (SkewTConstants.MAN_LEVELS[i] % 100 == 0)) {
                target.drawString(smallFont, s, getWorld().mapX(
                        SkewTConstants.center),
                        getWorld().mapY(
                                WxMath.getSkewTXY(SkewTConstants.MAN_LEVELS[i],
                                        -35).y), 0.0,
                        IGraphicsTarget.TextStyle.BLANKED,
                        SkewTConstants.pressureColor,
                        IGraphicsTarget.HorizontalAlignment.CENTER,
                        IGraphicsTarget.VerticalAlignment.MIDDLE, null);
            }

        }
    }

    /**
     * draw side bar lines
     */
    private void drawSideLines(IGraphicsTarget target) {

        try {
            target.drawLine(getWorld().mapX(SkewTConstants.left), getWorld()
                    .mapY(SkewTConstants.bottom), 0.0, getWorld().mapX(
                    SkewTConstants.left), getWorld().mapY(SkewTConstants.top),
                    0.0, SkewTConstants.pressureColor, 1);

            target.drawLine(getWorld().mapX(SkewTConstants.right), getWorld()
                    .mapY(SkewTConstants.bottom), 0.0, getWorld().mapX(
                    SkewTConstants.right), getWorld().mapY(SkewTConstants.top),
                    0.0, SkewTConstants.pressureColor, 1);

        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    /**
     * Draws a particular line at the intersection of two lines.
     * 
     * @param line1
     * @param line2
     * @param s
     * @throws VizException
     */
    private void drawLabelAtLineIntersections(IGraphicsTarget target,
            Line2D.Double line1, Line2D.Double line2, String s)
            throws VizException {
        Point2D.Double point = null;
        if ((point = getLineIntersection(line1, line2)) != null) {
            target.drawString(smallFont, s, point.x, point.y, 0.0,
                    IGraphicsTarget.TextStyle.BLANKED,
                    SkewTConstants.labelColor,
                    IGraphicsTarget.HorizontalAlignment.CENTER, null);
        }

    }

    /**
     * Returns the point that two lines instersect, or null if they do not.
     * 
     * @param l1
     * @param l2
     * @return
     */
    private Point2D.Double getLineIntersection(Line2D.Double l1,
            Line2D.Double l2) {
        if (!l1.intersectsLine(l2)) {
            return null;
        }

        Point2D.Double intersection = new Point2D.Double();
        double x1 = l1.getX1(), y1 = l1.getY1(), x2 = l1.getX2(), y2 = l1
                .getY2(), x3 = l2.getX1(), y3 = l2.getY1(), x4 = l2.getX2(), y4 = l2
                .getY2();

        intersection.x = det(det(x1, y1, x2, y2), x1 - x2, det(x3, y3, x4, y4),
                x3 - x4)
                / det(x1 - x2, y1 - y2, x3 - x4, y3 - y4);
        intersection.y = det(det(x1, y1, x2, y2), y1 - y2, det(x3, y3, x4, y4),
                y3 - y4)
                / det(x1 - x2, y1 - y2, x3 - x4, y3 - y4);

        return intersection;
    }

    private double det(double a, double b, double c, double d) {
        return a * d - b * c;
    }

    /**
     * Draws the mixing ratio lines.
     * 
     * @throws VizException
     */
    private void drawMixingRatios(IGraphicsTarget target,
            PaintProperties paintProperties) throws VizException {
        // get the location of the 825 pressure line...
        Coordinate coorStart = WxMath.getSkewTXY(850, -50);
        Coordinate coorEnd = WxMath.getSkewTXY(850, 50);

        double startX = getWorld().mapX(coorStart.x);
        double startY = getWorld().mapY(coorStart.y);

        double endX = getWorld().mapX(coorEnd.x);
        double endY = getWorld().mapY(coorEnd.y);

        Line2D.Double line = new Line2D.Double(startX, startY, endX, endY);

        double zoomLevel = paintProperties.getZoomLevel();
        double[] mixingRatios = getMixingRatios(zoomLevel);
        for (double ratio : mixingRatios) {
            UAPoint p1 = new UAPoint();
            p1.pressure = 1000;
            p1.temperature = Equations
                    .invMixingRatio(p1.pressure, ratio / 1000);

            UAPoint p2 = new UAPoint();
            p2.pressure = 400;
            p2.temperature = Equations
                    .invMixingRatio(p2.pressure, ratio / 1000);
            Coordinate coor1 = WxMath.getSkewTXY(p1.pressure,
                    p1.temperature - 273.15);
            Coordinate coor2 = WxMath.getSkewTXY(p2.pressure,
                    p2.temperature - 273.15);
            target.drawLine(getWorld().mapX(coor1.x), getWorld().mapY(coor1.y),
                    0.0, getWorld().mapX(coor2.x), getWorld().mapY(coor2.y),
                    0.0, SkewTConstants.mixingRatioColor, 1,
                    IGraphicsTarget.LineStyle.DASHED);

            drawLabelAtLineIntersections(target, line, new Line2D.Double(
                    getWorld().mapX(coor1.x), getWorld().mapY(coor1.y),
                    getWorld().mapX(coor2.x), getWorld().mapY(coor2.y)), Double
                    .toString((ratio)));
        }
    }

    private double[] getMixingRatios(double zoomLevel) {

        double[][] ratios = {
                { .5, 1, 2, 5, 10, 20, 50 },
                { .3, .5, 1, 2, 3, 5, 8, 12, 20, 30, 50 },

                { 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 7, 8, 9, 10, 11, 12, 14,
                        16, 18, 20, 24, 28, 32, 36, 40, 45, 50 },
                { 1.4, 1.6, 1.8, 2, 2.4, 2.8, 3.2, 3.6, 4, 4.5, 5, 5.5, 6, 7,
                        8, 9, 10, 11, 12, 14, 16, 18 },
                { .1, .2, .3, .4, .5, .6, .8, .7, .8, .9, 1, 1.1, 1.2, 1.4,
                        1.6, 1.8, 2, 2.4, 2.8, 3.2, 3.6, 4, 4.5, 5, 5.5, 6 },
                { .1, .2, .3, .4, .5, .6, .8, 1, 1.2, 1.6, 2, 2.5, 3, 4, 5, 6,
                        8, 10, 12, 16, 20, 25, 30, 40, 50 },
                { .1, .2, .3, .5, .7, 1, 1.5, 2, 3, 5, 7, 10, 15, 20, 30, 50 },

                { .3, 1, 3, 10, 30 }, { .1, 1, 10 } };

        if (zoomLevel == 1) {
            return ratios[0];
        }
        if (zoomLevel >= .70) {
            return ratios[1];
        }
        return ratios[0];
    }

    @Override
    public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        PixelExtent pe = new PixelExtent(this.rectangle);

        target.setupClippingPlane(pe);

        drawAdiabats(target);
        drawTempLines(target);
        drawPressureLines(target);
        drawMixingRatios(target, paintProps);
        drawSideLines(target);

        target.clearClippingPlane();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.skewt.ui.AbstractSkewTBackground#computeWorld()
     */
    @Override
    protected WGraphics computeWorld() {

        WGraphics world = new WGraphics(this.rectangle);

        world.setWorldCoordinates(SkewTConstants.left, SkewTConstants.top,
                SkewTConstants.right, SkewTConstants.bottom);

        return world;
    }

}
