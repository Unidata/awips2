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

package com.raytheon.viz.gfe.temporaleditor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Pattern;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;

import com.raytheon.uf.common.dataplugin.gfe.discrete.DiscreteKey;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.FillPatterns;
import com.raytheon.viz.gfe.colortable.ColorTable.ImageAttr;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.gridmanager.MouseHandler;
import com.raytheon.viz.gfe.rsc.DiscreteDisplayUtil;
import com.raytheon.viz.gfe.sampler.HistPair;
import com.raytheon.viz.gfe.sampler.HistSample;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditorUtil.TextJustify;
import com.raytheon.viz.gfe.temporaleditor.mousehandler.EditorDiscreteMouseHandler;

/**
 * Displays the Temporal Editor for Discrete Data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 30, 2009 2159       rjpeter     Initial creation.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class TemporalEditorDiscreteBar extends AbstractTemporalEditorBar {
    private MouseHandler editorMouseHandler;

    private Map<Rectangle, DiscreteKey> rectToDiscreteKey = new HashMap<Rectangle, DiscreteKey>();

    private Parm displayedParm;

    private Font sampleFont;

    /**
     * @param parent
     * @param teUtil
     * @param parm
     * @param ts
     */
    public TemporalEditorDiscreteBar(TemporalEditor parent,
            TemporalEditorUtil teUtil, Parm parm, TimeSeries ts) {
        super(parent, teUtil, parm, ts);

        editorMouseHandler = new EditorDiscreteMouseHandler(this);
        editorMouseHandler.setDragButtons(1);
        editorMouseHandler.setDragTolerance(1);

        addParm(parm, ts);
    }

    /**
     * 
     */
    @Override
    protected void setupScaleCanvas() {
        int width = SCALE_WIDTH;

        if (scaleCanvas != null && !scaleCanvas.isDisposed()) {
            width = ((GridData) scaleCanvas.getLayoutData()).widthHint;
            scaleCanvas.dispose();
        }

        scaleCanvas = new Canvas(container, SWT.BORDER);
        GridData gridData = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        gridData.widthHint = width;
        scaleCanvas.setLayoutData(gridData);

        scaleCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                paintScaleCanvas(e);
            }
        });
    }

    /**
     * 
     */
    @Override
    protected void setupEditorCanvas() {
        if (editorCanvas != null && !editorCanvas.isDisposed()) {
            editorCanvas.removeMouseListener(editorMouseHandler);
            editorCanvas.dispose();
        }

        editorCanvas = new Canvas(container, SWT.BORDER);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        editorCanvas.setLayoutData(gridData);
        editorCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                paintEditorCanvas(e);
            }
        });
        editorCanvas.addMouseListener(editorMouseHandler);
    }

    @Override
    public void addParm(Parm parm, TimeSeries ts) {
        super.addParm(parm, ts);
        parmDisplayAttributesMap.get(parm).setDisplayedAsGraphic(false);
    }

    /**
     * 
     * @param event
     */
    @Override
    protected void paintEditorCanvas(PaintEvent event) {
        Rectangle clientArea = editorCanvas.getClientArea();
        teUtil.paintBackground(event, clientArea);

        if (DataManager.getCurrentInstance().getRefManager().getActiveRefSet() != null) {
            paintLocks(event);
            paintDiscrete(event);
        }
        if (showEditorTimeLines) {
            teUtil.paintSelected(event, clientArea);
        }
        teUtil.paintTimeScaleLines(event, clientArea);
    }

    /**
     * 
     * @param event
     */
    private void paintDiscrete(PaintEvent event) {
        GC gc = event.gc;
        rectToDiscreteKey.clear();
        displayedParm = null;

        Font oldFont = gc.getFont();
        Font labelFont = null;
        sampleFont = makeLabelFont(gc, "TESample_font", 1);
        if (sampleFont == null) {
            labelFont = oldFont;
        } else {
            labelFont = sampleFont;
        }

        try {
            paintDiscreteBody(event, gc, labelFont);
        } finally {
            gc.setFont(oldFont);
        }
    }

    /**
     * @param event
     * @param gc
     */
    private void paintDiscreteBody(PaintEvent event, GC gc, Font labelFont) {
        for (Parm parm : parmList) {
            TEParmDisplayAttributes parmDispAtt = parmDisplayAttributesMap
                    .get(parm);

            if (parmDispAtt.isDisplayed()) {
                displayedParm = parm;
                gc.setLineStyle(SWT.LINE_SOLID);
                TimeRange range = teUtil.getVisibleTimeRange();
                TimeSeries ts = parmToTimeSeries.get(parm);
                List<HistSample> samples = ts.getSamplesForTimeRange(range);
                Color baseColor = parmBaseColorMap.get(parm);
                int yMax = editorCanvas.getClientArea().height;

                for (HistSample sample : samples) {
                    if (sample.numOfPoints() > 0) {
                        float vScaling = (float) (sample.numOfPoints())
                                / (float) yMax;
                        float barMin = 0.0f;
                        TimeRange vt = sample.validTime();
                        int xStart = teUtil.dateToPixel(vt.getStart());
                        int xDur = teUtil.durationToPixels(vt.getDuration());

                        for (HistPair histPair : sample.histogram()) {
                            WxValue wxv = WxValue.getValue(histPair.value(),
                                    parm);
                            float barMax = barMin + histPair.count() / vScaling;
                            Rectangle rect = new Rectangle(xStart,
                                    Math.round(barMin), xDur, Math.round(barMax
                                            - barMin));

                            DiscreteKey discreteKey = ((DiscreteWxValue) wxv)
                                    .getDiscreteKey();
                            rectToDiscreteKey.put(rect, discreteKey);

                            paintBackgrounds(event, wxv, rect);

                            gc.setBackground(BACKGROUND_COLOR);
                            gc.setBackgroundPattern(null);
                            gc.setForeground(baseColor);
                            gc.drawRectangle(rect);
                            gc.setFont(labelFont);
                            paintLabel(gc, discreteKey.toString(), vt,
                                    Math.round((barMax + barMin) / 2.0f), 0,
                                    TextJustify.CENTER,
                                    Math.round(barMax - barMin));
                            barMin = barMax; // set for next loop iteration
                        }
                    }
                }

                // can only be one
                return;
            }
        }
    }

    /**
     * Using the PaintEvent supplied, draw all the background patterns for wxv
     * into the rectangle rect
     * 
     * @param event
     *            The paint event that triggered the call
     * @param wxv
     *            the WxValue whose background patterns are being painted
     * @param rect
     *            A rectangle describing where to paint
     */
    private void paintBackgrounds(PaintEvent event, WxValue wxv, Rectangle rect) {
        GC gc = event.gc;
        List<ImageAttr> fillAttrs = DiscreteDisplayUtil.getFillAttributes(wxv);
        for (ImageAttr attr : fillAttrs) {
            RGB rgb = RGBColors.getRGBColor(attr.getColorName());
            Color color = new Color(event.display, rgb);
            Pattern pattern = FillPatterns.getSWTPattern(rgb,
                    attr.getFillPatternName());

            gc.setBackground(color);
            gc.setBackgroundPattern(pattern);
            gc.fillRectangle(rect);

            if (pattern != null) {
                pattern.dispose();
            }

            color.dispose();
        }
    }

    @Override
    public void toggleParmDisplayedAsGraphic(Parm parm) {
        // discrete cannot be toggled
    }

    /**
     * 
     * @param pt
     * @return
     */
    public DiscreteKey getClickedKey(Point pt) {
        for (Rectangle rect : rectToDiscreteKey.keySet()) {
            if (rect.contains(pt)) {
                return rectToDiscreteKey.get(rect);
            }
        }

        return null;
    }

    /**
     * 
     * @return
     */
    public Parm getDisplayedParm() {
        return displayedParm;
    }

    @Override
    public void dispose() {
        if (sampleFont != null && !sampleFont.isDisposed()) {
            sampleFont.dispose();
            sampleFont = null;
        }
        super.dispose();
    }
}
