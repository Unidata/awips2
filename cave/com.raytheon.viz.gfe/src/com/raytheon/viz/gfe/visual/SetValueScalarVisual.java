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
package com.raytheon.viz.gfe.visual;

import java.util.Arrays;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;

import com.raytheon.uf.common.colormap.Color;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.gridmanager.MouseHandler;
import com.raytheon.viz.gfe.rsc.GFEFonts;

/**
 * Visual for Scalar set value dialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 22, 2009  1318     randerso  Initial creation
 * Mar 10, 2016  5479     randerso  Use improved GFEFonts API
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 *
 * </pre>
 *
 * @author randerso
 */

public class SetValueScalarVisual {

    private Canvas canvas;

    private Parm parm;

    private ScaleVisual scaleVisual;

    private Font contLabelFont;

    private Font pickupFont;

    private boolean zoomedIn;

    private float zoomFactor;

    /**
     * Constructor
     *
     * @param canvas
     * @param parm
     */
    public SetValueScalarVisual(Canvas canvas, Parm parm) {
        this.canvas = canvas;
        this.parm = parm;
        scaleVisual = new ScaleVisual(210, 0.05f);
        scaleVisual.setParms(Arrays.asList(parm));
        scaleVisual.setCanvas(canvas);

        // Get the font for the scale
        contLabelFont = GFEFonts.makeGFEFont(canvas.getDisplay(),
                "SetValueContLabel_font", SWT.BOLD, 2);

        // Get the font for the pick up value
        pickupFont = GFEFonts.makeGFEFont(canvas.getDisplay(),
                "SetValuePickUp_font", SWT.BOLD, 3);

        zoomedIn = false;
        int zoom = GFEPreference.getInt("SetValue_zoom", 4);
        zoom = GFEPreference.getInt(
                parm.getParmID().compositeNameUI() + "_SetValue_zoom", zoom);
        zoomFactor = 1.0f / zoom;

        canvas.addMouseListener(new MouseHandler() {

            @Override
            public void dragMove(MouseEvent e) {
                super.dragMove(e);
                handleMouseDragEvent(e);
            }

            @Override
            public void mouseClick(MouseEvent e) {
                super.mouseClick(e);
                handleMouseClickEvent(e);
            }

        });
    }

    /**
     * Render this visual
     *
     * @param e
     *            the paint event
     */
    public void render(PaintEvent e) {
        GC gc = e.gc;

        // check for valid parm
        if (parm == null) {
            return;
        }

        canvas.setBackground(
                canvas.getDisplay().getSystemColor(SWT.COLOR_BLACK));

        // get the color table values, make a copy, then prepend notInTableEntry
        ColorMapParameters colorMapParameters = parm.getDataManager()
                .getSpatialDisplayManager().getResourcePair(parm).getResource()
                .getCapability(ColorMapCapability.class)
                .getColorMapParameters();

        if (colorMapParameters == null) {
            // cannot render without a color table.
            return;
        }

        // paint the color bar
        paintColorBar(e, colorMapParameters);

        gc.setFont(contLabelFont);
        scaleVisual.renderScale(e);

        gc.setFont(pickupFont);

        if (parm.isMutable()) {
            paintPickUpValue(e);
        }
    }

    private void paintPickUpValue(PaintEvent e) {
        WxValue wxValue = parm.getParmState().getPickUpValue();
        float pickupValue = ((ScalarWxValue) wxValue).getValue();
        Point p = scaleVisual.getPointForValue(pickupValue);

        GC gc = e.gc;
        gc.setForeground(gc.getDevice().getSystemColor(SWT.COLOR_WHITE));
        p.x += 10;
        gc.drawLine(p.x, p.y, p.x + 50, p.y);
        gc.drawLine(p.x, p.y, p.x + 10, p.y + 5);
        gc.drawLine(p.x, p.y, p.x + 10, p.y - 5);

        int yOffset = gc.textExtent("0").y / 2;
        gc.drawText(wxValue.toString(), p.x + 50, p.y - yOffset, true);

    }

    /**
     * paint the color bar
     *
     * @param e
     *            the paint event
     * @param colorMapParameters
     */
    public void paintColorBar(PaintEvent e,
            ColorMapParameters colorMapParameters) {
        GC gc = e.gc;
        Point minColor = scaleVisual
                .getPointForValue(colorMapParameters.getColorMapMin());
        Point maxColor = scaleVisual
                .getPointForValue(colorMapParameters.getColorMapMax());

        Point minParm = scaleVisual
                .getPointForValue(parm.getGridInfo().getMinValue());
        Point maxParm = scaleVisual
                .getPointForValue(parm.getGridInfo().getMaxValue());

        double height = (double) (maxColor.y - minColor.y)
                / colorMapParameters.getColorMap().getSize();
        int width = 50;
        Rectangle rect = new Rectangle(minColor.x, minColor.y, width, 0);

        int i = 0;
        int y0 = minColor.y;
        int y1 = minParm.y;
        int y2;
        for (Color colorEntry : colorMapParameters.getColorMap().getColors()) {
            if (i < (colorMapParameters.getColorMap().getSize() - 1)) {
                y2 = (int) (y0 + ((i + 1) * height));
            } else {
                y2 = maxParm.y;
            }
            rect.y = y1;
            rect.height = (y2 != y1 ? y2 - y1 : 1);
            org.eclipse.swt.graphics.Color color = new org.eclipse.swt.graphics.Color(
                    gc.getDevice(), (int) (colorEntry.getRed() * 255),
                    (int) (colorEntry.getGreen() * 255),
                    (int) (colorEntry.getBlue() * 255));
            gc.setBackground(color);
            gc.setAlpha((int) (colorEntry.getAlpha() * 255));
            gc.fillRectangle(rect);
            color.dispose();

            y1 = y2;
            i++;
        }

        gc.setAlpha(255);
    }

    /**
     * @param e
     */
    private void handleMouseClickEvent(MouseEvent e) {
        if (e.button == 1) {
            setPickupValue(e);
        } else if (e.button == 2) {
            click2Event(e);
        }
    }

    /**
     * @param e
     */
    private void setPickupValue(MouseEvent e) {
        float value = scaleVisual.getValueForHeight(e.y);
        value = Math.max(value, parm.getGridInfo().getMinValue());
        value = Math.min(value, parm.getGridInfo().getMaxValue());
        ScalarWxValue pickupValue = new ScalarWxValue(value, parm);
        parm.getParmState().setPickUpValue(pickupValue);
    }

    /**
     * @param e
     */
    private void click2Event(MouseEvent e) {
        if (zoomedIn) {
            scaleVisual.fullView();
        } else {
            scaleVisual.zoom(zoomFactor, e.y);
        }

        canvas.redraw();
        zoomedIn = !zoomedIn;
    }

    private void handleMouseDragEvent(MouseEvent e) {
        setPickupValue(e);
    }

    /**
     * Dispose resources used by this object
     */
    public void dispose() {
        if ((pickupFont != null) && !pickupFont.isDisposed()) {
            pickupFont.dispose();
            pickupFont = null;
        }
        if ((this.contLabelFont != null) && !contLabelFont.isDisposed()) {
            contLabelFont.dispose();
            contLabelFont = null;
        }
    }
}
