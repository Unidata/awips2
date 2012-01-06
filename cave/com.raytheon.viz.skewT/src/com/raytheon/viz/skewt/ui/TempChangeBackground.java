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
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.viz.core.graphing.WGraphics;
import com.raytheon.viz.skewt.Activator;

/**
 * Used to draw the temp change background.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 28sep2008   #1529       dhladky     redone.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class TempChangeBackground extends AbstractSkewTBackground implements
        IRenderable {

    private double pMin;

    private double pMax;

    private double deltaT;

    /**
     * Public constructor
     * 
     * @param target
     * @param world
     * @param paintProps
     * @throws VizException
     */
    public TempChangeBackground() {
        super();

        this.rectangle = new Rectangle(600, 730, 250, 480);
    }

    @Override
    public void paintInternal(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        double left = getWorld().getViewXmin();
        double right = getWorld().getViewXmax();
        double top = getWorld().getViewYmin();
        double bottom = getWorld().getViewYmax();

        double d = Math.pow(10, Math.ceil(Math.log10(deltaT))) - 1;
        String s = "-  " + SkewTConstants.tempFormat.format(d);
        double labelWidth = target.getStringBounds(smallFont, s).getWidth()
                * paintProps.getZoomLevel();
        double dT = 1;
        while ((deltaT % dT != 0)
                || (labelWidth > ((right - left) / 2 / deltaT * dT))) {
            dT++;
        }

        for (double t = 0.0; t <= deltaT - dT; t += dT) {
            double x = getWorld().mapX(t);
            target.drawLine(x, bottom, 0.0, x, top, 0.0,
                    SkewTConstants.backgroundColor, 1, LineStyle.DOTTED);
            target.drawString(smallFont, SkewTConstants.tempFormat.format(t),
                    x, bottom, 0.0, TextStyle.NORMAL,
                    SkewTConstants.labelColor, HorizontalAlignment.CENTER,
                    VerticalAlignment.TOP, null);

            if (t == 0) {
                continue;
            }
            x = getWorld().mapX(-t);
            target.drawLine(x, bottom, 0.0, x, top, 0.0,
                    SkewTConstants.backgroundColor, 1, LineStyle.DOTTED);
            target.drawString(smallFont, SkewTConstants.tempFormat.format(-t),
                    x, bottom, 0.0, TextStyle.NORMAL,
                    SkewTConstants.labelColor, HorizontalAlignment.CENTER,
                    VerticalAlignment.TOP, null);
        }
        double x = getWorld().mapX(-deltaT);
        target.drawLine(x, bottom, 0.0, x, top, 0.0,
                SkewTConstants.backgroundColor, 1, LineStyle.SOLID);
        target.drawString(smallFont, SkewTConstants.tempFormat.format(-deltaT),
                x, bottom, 0.0, TextStyle.NORMAL, SkewTConstants.labelColor,
                HorizontalAlignment.CENTER, VerticalAlignment.TOP, null);

        x = getWorld().mapX(deltaT);
        target.drawLine(x, bottom, 0.0, x, top, 0.0,
                SkewTConstants.backgroundColor, 1, LineStyle.SOLID);
        target.drawString(smallFont, SkewTConstants.tempFormat.format(deltaT),
                x, bottom, 0.0, TextStyle.NORMAL, SkewTConstants.labelColor,
                HorizontalAlignment.CENTER, VerticalAlignment.TOP, null);

        // draw pressure lines.
        double centerX = (left + right) / 2;
        for (double pressure : SkewTConstants.MAN_LEVELS) {
            if (pressure >= pMin && pressure <= pMax) {
                double y = getWorld().mapY(WxMath.getSkewTXY(pressure, 0).y);
                target.drawLine(left, y, 0.0, right, y, 0.0,
                        SkewTConstants.pressureColor, 1);

                if (pressure % 100 == 0) {
                    target.drawString(smallFont, SkewTConstants.pressFormat
                            .format(pressure), centerX, getWorld().mapY(
                            WxMath.getSkewTXY(pressure, 0).y), 0.0,
                            TextStyle.BLANKED, SkewTConstants.labelColor,
                            HorizontalAlignment.CENTER,
                            VerticalAlignment.MIDDLE, null);
                }
            }
        }

        String label = "24 HR TEMP CHANGE";

        target.drawString(smallFont, label, centerX, top, 0.0,
                TextStyle.NORMAL, SkewTConstants.labelColor,
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
        pMin = ((d = prefs.getDouble("tempChange.pmin")) == 0.0 ? 300.0 : d);
        pMax = ((d = prefs.getDouble("tempChange.pmax")) == 0.0 ? 1050.0 : d);
        deltaT = ((d = prefs.getDouble("tempChange.deltaT")) == 0.0 ? 15.0 : d);

        WGraphics world = new WGraphics(this.rectangle);

        world.setWorldCoordinates(-deltaT, WxMath.getSkewTXY(pMin, 0).y,
                deltaT, WxMath.getSkewTXY(pMax, 0).y);

        return world;
    }

}
