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

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.core.msgs.QuickViewModeChangedMsg;
import com.raytheon.viz.gfe.core.msgs.ShowQuickViewDataMsg;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.raytheon.viz.gfe.core.wxvalue.VectorWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.gfe.gridmanager.MouseHandler;
import com.raytheon.viz.gfe.sampler.HistSample;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditor.StatisticsMode;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditorUtil.TextJustify;
import com.raytheon.viz.gfe.temporaleditor.mousehandler.EditorNumericMouseHandler;
import com.raytheon.viz.gfe.temporaleditor.mousehandler.ScaleMouseHandler;
import com.raytheon.viz.gfe.visual.ScaleVisual;

/**
 * Displays the Temporal Editor Data
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
public class TemporalEditorNumericBar extends AbstractTemporalEditorBar
        implements IMessageClient {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TemporalEditorNumericBar.class);

    public static final int WIND_VISUAL_SIZE = 60;

    private static float radPerDeg = 0.0174533f;

    private static float fletchAngle = 60.0f; // in degrees from the barb shaft

    private static final double tipAngle = 150.0;

    private class QuickviewMouseListener extends MouseTrackAdapter implements
            MouseMoveListener {

        @Override
        public void mouseMove(MouseEvent e) {
            TemporalEditorUtil teUtil = getUtil();
            Date date = teUtil.pixelToDate(e.x);

            float val = getScale().getValueForHeight(e.y);
            Parm parm = getClosestParm(date, val);
            IGridData gridData = null;
            if (parm != null) {
                gridData = parm.overlappingGrid(date);
            }
            showQuickViewGrid(gridData);
        }

        @Override
        public void mouseExit(MouseEvent e) {
            if (quickviewMode && (quickviewGrid != null)) {
                showQuickViewGrid(null);
            }
        }

    }

    private MouseHandler scaleMouseHandler;

    private MouseHandler editorMouseHandler;

    private ScaleVisual scaleVisual;

    /**
     * 
     */
    private Map<Parm, List<Color>> parmColorMap = new HashMap<Parm, List<Color>>();

    private float min = Float.MAX_VALUE;

    private float max = -Float.MAX_VALUE;

    private boolean quickviewMode;

    private GridID quickviewGrid;

    private QuickviewMouseListener quickviewMouseListener;

    private Font sampleFont;

    /**
     * 
     * @param parent
     * @param gridManager
     * @param parm
     * @param ts
     */
    @SuppressWarnings("unchecked")
    public TemporalEditorNumericBar(TemporalEditor parent,
            TemporalEditorUtil teUtil, Parm parm, TimeSeries ts) {
        super(parent, teUtil, parm, ts);
        scaleMouseHandler = new ScaleMouseHandler(this);
        scaleMouseHandler.setDragButtons(1);
        scaleMouseHandler.setDragTolerance(2);
        editorMouseHandler = new EditorNumericMouseHandler(this);
        editorMouseHandler.setDragButtons(1);
        editorMouseHandler.setDragTolerance(1);
        quickviewMouseListener = new QuickviewMouseListener();
        scaleVisual = new ScaleVisual(5, 0.2f);
        addParm(parm, ts);

        Message.registerInterest(this, QuickViewModeChangedMsg.class);
        receiveMessage(Message
                .inquireLastMessage(QuickViewModeChangedMsg.class));
    }

    @SuppressWarnings("unchecked")
    @Override
    public void dispose() {
        if (sampleFont != null && !sampleFont.isDisposed()) {
            sampleFont.dispose();
            sampleFont = null;
        }
        Message.unregisterInterest(this, QuickViewModeChangedMsg.class);

        super.dispose();
    }

    /**
     * 
     * @param parm
     */
    @Override
    public void addParm(Parm parm, TimeSeries ts) {
        if (!parmList.contains(parm)) {
            super.addParm(parm, ts);

            IColorMap colorMap = null;
            ResourcePair rsc = DataManagerUIFactory.getCurrentInstance()
                    .getSpatialDisplayManager().getResourcePair(parm);
            if (rsc != null) {
                colorMap = rsc.getResource()
                        .getCapability(ColorMapCapability.class)
                        .getColorMapParameters().getColorMap();
            }
            if (colorMap != null) {
                List<Color> colorList = new ArrayList<Color>();
                Display display = Display.getCurrent();

                for (com.raytheon.uf.common.colormap.Color c : colorMap
                        .getColors()) {
                    RGB rgb = new RGB(Math.round(c.getRed() * 255.0f),
                            Math.round(c.getGreen() * 255.0f), Math.round(c
                                    .getBlue() * 255.0f));
                    colorList.add(new Color(display, rgb));
                }

                parmColorMap.put(parm, colorList);
            } else {
                statusHandler.handle(
                        Priority.PROBLEM,
                        "Could not determine colormap for Parm["
                                + parm.getFormattedString() + "]");
            }

            scaleVisual.setParms(parmList);
        }
    }

    /**
     * 
     * @param parm
     */
    @Override
    public void removeParm(Parm parm) {
        if (parmList.contains(parm)) {
            super.removeParm(parm);
            List<Color> colorList = parmColorMap.remove(parm);

            if (colorList != null) {
                for (Color color : colorList) {
                    color.dispose();
                }
            }

            scaleVisual.setParms(parmList);
        }
    }

    /**
     * 
     */
    @Override
    protected void setupScaleCanvas() {
        int width = SCALE_WIDTH;

        if (scaleCanvas != null && !scaleCanvas.isDisposed()) {
            width = ((GridData) scaleCanvas.getLayoutData()).widthHint;
            scaleCanvas.removeMouseListener(scaleMouseHandler);
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

        scaleVisual.setCanvas(scaleCanvas);
        scaleCanvas.addMouseListener(scaleMouseHandler);
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

        if (quickviewMode) {
            editorCanvas.addMouseMoveListener(quickviewMouseListener);
            editorCanvas.addMouseTrackListener(quickviewMouseListener);
        }
    }

    /**
     * 
     * @param event
     */
    @Override
    protected void paintScaleCanvas(PaintEvent event) {
        super.paintScaleCanvas(event);

        if (scaleVisual != null) {
            scaleVisual.renderScale(event);
        }
    }

    /**
     * 
     * @param event
     */
    @Override
    protected void paintEditorCanvas(PaintEvent event) {
        Rectangle clientArea = editorCanvas.getClientArea();
        teUtil.paintBackground(event, clientArea);
        teUtil.paintTimeScaleLines(event, clientArea);

        if (showEditorTimeLines) {
            teUtil.paintSelected(event, clientArea);
        }
        max = -Float.MAX_VALUE;
        min = Float.MAX_VALUE;

        if (DataManager.getCurrentInstance().getRefManager().getActiveRefSet() != null) {
            paintLocks(event);
            paintColorBar(event);
            paintNumericData(event);
        }
    }

    /**
     * 
     * @param event
     */
    protected void paintColorBar(PaintEvent event) {
        for (Parm parm : parmList) {
            GridType gridType = parm.getGridInfo().getGridType();

            if (GridType.SCALAR.equals(gridType)
                    || GridType.VECTOR.equals(gridType)) {
                TEParmDisplayAttributes parmDispAtt = parmDisplayAttributesMap
                        .get(parm);

                // print color bar
                if (parmDispAtt.isDisplayed()
                        && (parmDispAtt.hasColorBar() || parmDispAtt
                                .hasColorRangeBar())) {
                    paintEditorParm(event, parm, parmDispAtt, true);

                    // only 1 can be color bar
                    return;
                }
            }
        }
    }

    /**
     * 
     * @param event
     */
    protected void paintNumericData(PaintEvent event) {
        // print all other data, iterating in reverse order so that the first
        // parm is printed last causing its printing to be highest
        ListIterator<Parm> listIter = parmList.listIterator(parmList.size());
        while (listIter.hasPrevious()) {
            Parm parm = listIter.previous();
            TEParmDisplayAttributes parmDispAtt = parmDisplayAttributesMap
                    .get(parm);
            GridType gridType = parm.getGridInfo().getGridType();

            if (parmDispAtt.isDisplayed()
                    && (GridType.SCALAR.equals(gridType) || GridType.VECTOR
                            .equals(gridType))) {
                paintEditorParm(event, parm, parmDispAtt, false);
            }
        }
    }

    // -- public
    // -----------------------------------------------------------------
    // RangeBarVisual::render()
    // Command to render this visualization into the specified graphics over
    // the given domain.
    // -- implementation
    // ---------------------------------------------------------
    // Accumulates all of the components, and then paints them at the end.
    // ---------------------------------------------------------------------------
    private void paintEditorParm(PaintEvent event, Parm parm,
            TEParmDisplayAttributes parmDispAtt, boolean paintColorBar) {
        GC gc = event.gc;
        TimeRange range = teUtil.getVisibleTimeRange();
        TimeSeries ts = parmToTimeSeries.get(parm);
        List<HistSample> samples = ts.getSamplesForTimeRange(range);
        int yPixelMax = editorCanvas.getClientArea().height;
        GridType gridType = parm.getGridInfo().getGridType();

        Font oldFont = gc.getFont();
        if (sampleFont == null) {
            sampleFont = makeLabelFont(gc, "TESample_font", 1);
        }
        Font labelFont = (sampleFont == null) ? oldFont : sampleFont;
        try {

            if (samples.size() > 0) {
                Color baseColor = parmBaseColorMap.get(parm);
                List<Color> colorList = parmColorMap.get(parm);

                WxValue absAveWxValue, aveWxValue, maxWxValue, minWxValue;
                float absAveVal, aveVal, maxVal, minVal;
                int yAbsAve, yAve, yMax, yMin;
                int xStart, xMiddle, xEnd;
                StatisticsMode mode = temporalEditor.getMode();
                int modMin = temporalEditor.getModeratedMin();
                int modMax = temporalEditor.getModeratedMax();
                float stdMin = temporalEditor.getStdDevMin();
                float stdMax = temporalEditor.getStdDevMax();
                float parmMax = parm.getGridInfo().getMaxValue();
                float parmMin = parm.getGridInfo().getMinValue();
                int prevYAbsAve = 0;
                TimeRange prevVt = null;

                for (HistSample sample : samples) {
                    TimeRange vt = sample.validTime();
                    absAveWxValue = WxValue
                            .getValue(sample.average(true), parm);

                    switch (mode) {
                    case MODERATED:
                        aveWxValue = WxValue.getValue(
                                sample.moderatedAverage(modMin, modMax, true),
                                parm);
                        maxWxValue = WxValue.getValue(
                                sample.moderatedMax(modMax), parm);
                        minWxValue = WxValue.getValue(
                                sample.moderatedMin(modMin), parm);
                        break;
                    case STANDARD_DEVIATION:
                        aveWxValue = WxValue.getValue(
                                sample.stdDevAvg(stdMin, stdMax, true), parm);
                        maxWxValue = WxValue.getValue(sample.stdDevMax(stdMax),
                                parm);
                        minWxValue = WxValue.getValue(sample.stdDevMin(stdMin),
                                parm);
                        break;
                    case ABSOLUTE:
                    default:
                        aveWxValue = WxValue.getValue(sample.average(true),
                                parm);
                        maxWxValue = WxValue.getValue(sample.absoluteMax(),
                                parm);
                        minWxValue = WxValue.getValue(sample.absoluteMin(),
                                parm);
                        break;
                    }

                    // extract the data values
                    if (aveWxValue instanceof ScalarWxValue) {
                        absAveVal = ((ScalarWxValue) absAveWxValue).getValue();
                        aveVal = ((ScalarWxValue) aveWxValue).getValue();
                        maxVal = ((ScalarWxValue) maxWxValue).getValue();
                        minVal = ((ScalarWxValue) minWxValue).getValue();
                    } else {
                        absAveVal = ((VectorWxValue) absAveWxValue).getValue();
                        aveVal = ((VectorWxValue) aveWxValue).getValue();
                        maxVal = ((VectorWxValue) maxWxValue).getValue();
                        minVal = ((VectorWxValue) minWxValue).getValue();
                    }

                    max = Math.max(max, aveVal);
                    min = Math.min(min, aveVal);

                    yAbsAve = scaleVisual.getPointForValue(absAveVal).y;
                    yAve = scaleVisual.getPointForValue(aveVal).y;
                    yMax = scaleVisual.getPointForValue(maxVal).y;
                    yMin = scaleVisual.getPointForValue(minVal).y;
                    xStart = teUtil.dateToPixel(vt.getStart());
                    xMiddle = teUtil.dateToPixel(vt.getCenterTime());
                    xEnd = teUtil.dateToPixel(vt.getEnd());

                    if (paintColorBar) {
                        // render a box
                        if (parmDispAtt.hasColorBar()) {
                            Rectangle rect = new Rectangle(xStart, yAbsAve,
                                    xEnd - xStart,
                                    editorCanvas.getClientArea().height);
                            Color fillColor = TemporalEditorUtil
                                    .getColorForValue(colorList, parmMin,
                                            parmMax, absAveVal);
                            gc.setForeground(fillColor);
                            gc.setBackground(fillColor);
                            gc.fillRectangle(rect);
                        }

                        // render a range box
                        if (parmDispAtt.hasColorRangeBar()) {
                            Rectangle rect = new Rectangle(xStart, yMax, xEnd
                                    - xStart, yMin - yMax);
                            Color fillColor = TemporalEditorUtil
                                    .getColorForValue(colorList, parmMin,
                                            parmMax, aveVal);
                            gc.setForeground(fillColor);
                            gc.setBackground(fillColor);
                            gc.fillRectangle(rect);
                        }
                    } else {
                        // the time bar
                        if (parmDispAtt.hasTimeBar()) {
                            gc.setLineWidth(2);
                            gc.setLineStyle(SWT.LINE_SOLID);
                            gc.setForeground(baseColor);
                            gc.drawLine(xStart, yAbsAve, xEnd, yAbsAve);

                            // the horizontal line
                            gc.drawLine(xStart, yAbsAve, xEnd, yAbsAve);

                            // the tick marks
                            gc.setLineWidth(1);
                            gc.drawLine(xStart, yAbsAve - 4, xStart,
                                    yAbsAve + 4);
                            gc.drawLine(xEnd, yAbsAve - 4, xEnd, yAbsAve + 4);

                            if (prevVt != null && prevVt.isAdjacentTo(vt)
                                    && showSplitBoundaries) {
                                // paint split boundary
                                gc.setLineStyle(SWT.LINE_DASH);
                                gc.drawLine(xStart, prevYAbsAve, xStart,
                                        yAbsAve);
                            }
                        }

                        // render a range diamond line
                        if (parmDispAtt.hasRangeBar()) {
                            int[] pointArray = new int[12];
                            pointArray[0] = xStart;
                            pointArray[1] = yAve;
                            pointArray[2] = xMiddle;
                            pointArray[3] = yMax;
                            pointArray[4] = xEnd;
                            pointArray[5] = yAve;
                            pointArray[6] = xStart;
                            pointArray[7] = yAve;
                            pointArray[8] = xMiddle;
                            pointArray[9] = yMin;
                            pointArray[10] = xEnd;
                            pointArray[11] = yAve;
                            gc.setLineWidth(2);
                            gc.setLineStyle(SWT.LINE_SOLID);
                            gc.setForeground(baseColor);
                            gc.drawPolyline(pointArray);
                        }

                        // paint wind barb
                        if (GridType.VECTOR.equals(gridType)) {
                            VectorWxValue vectorValue = (VectorWxValue) absAveWxValue;
                            gc.setLineWidth(1);
                            gc.setLineStyle(SWT.LINE_SOLID);
                            gc.setForeground(baseColor);

                            if (parmDispAtt.hasWindBarb()) {
                                paintWindBarb(event, parm,
                                        vectorValue.getMag(),
                                        vectorValue.getDir(), WIND_VISUAL_SIZE,
                                        xMiddle);
                            }
                            if (parmDispAtt.hasWindArrow()) {
                                paintWindArrow(event, parm,
                                        vectorValue.getMag(),
                                        vectorValue.getDir(), WIND_VISUAL_SIZE,
                                        xMiddle);
                            }
                        }

                        if (parmDispAtt.hasRangeBar()
                                || parmDispAtt.hasColorRangeBar()) {
                            gc.setForeground(baseColor);
                            gc.setFont(labelFont);
                            paintLabel(gc, aveWxValue.toString(), vt, yAve, -2,
                                    TextJustify.TOP, yPixelMax);
                            paintLabel(gc, maxWxValue.toString(), vt, yMax, -2,
                                    TextJustify.TOP, yPixelMax);
                            paintLabel(gc, minWxValue.toString(), vt, yMin, 2,
                                    TextJustify.BOTTOM, yPixelMax);
                            max = Math.max(max, maxVal);
                            min = Math.min(min, minVal);
                        }
                    }

                    if (parmDispAtt.hasTimeBar() || parmDispAtt.hasRangeBar()
                            || parmDispAtt.hasColorBar()
                            || parmDispAtt.hasColorRangeBar()) {
                        gc.setForeground(baseColor);
                        gc.setFont(labelFont);
                        paintLabel(gc, absAveWxValue.toString(), vt, yAbsAve,
                                -2, TextJustify.TOP, yPixelMax);
                    }

                    prevVt = vt;
                    prevYAbsAve = yAbsAve;
                }
            }
        } finally {
            gc.setFont(oldFont);
        }

    }

    // -- public
    // -----------------------------------------------------------------
    // WindBarbVisual::render()
    //
    // Renders a wind barb using the current VectorVisual parameters into the
    // supplied Graphics.
    //
    // -- implementation
    // ---------------------------------------------------------
    // Begin by defining a couple of constants and checking the magnitude for
    // sanity. Next calculate the true size of the wind barb graphic based on
    // the barbSize parameter. Next calculate a bunch of sine and cosine terms
    // based on the direction and fletch direction. Then figure out how many
    // flags (= 50 knots), fletches (=10 knots), and half-fletches (= 5 knots)
    // based on the magnitude. Allocate the XSegment array that we fill along
    // the way and draw with a single XDrawSegments call. Calculate and store
    // the rectangle that surrounds the location. Calculate and store the main
    // staff. Then for each half-fletch, fletch, and flag, calculate and store
    // the coordinates for each sub-object. Finally do the X call to light up
    // the pixels. Note that for all calculations, 0.5 is added to every
    // coordinate so that they are properly rounded off.
    // ---------------------------------------------------------------------------
    public void paintWindBarb(PaintEvent event, Parm parm, float mag,
            float dir, float size, int xPixel) {
        GC gc = event.gc;
        float speed = mag + 2.5f; // round to the nearest 5 knots

        // calculate some stuff based on the barbSize
        // these can be fiddled with to modify the appearance of the barb
        double staffSize = size / 2.0; // base length of the staff
        double fletchSize = staffSize / 2.0; // length of each fletch
        double fletchDelta = size * 0.1; // distance between fletches

        double dirSin = Math.sin(dir * radPerDeg);
        double dirCos = Math.cos(dir * radPerDeg);
        double fletchSin = Math.sin((dir + fletchAngle) * radPerDeg);
        double fletchCos = Math.cos((dir + fletchAngle) * radPerDeg);

        // calculate some terms once and use them many times
        double xStaff = dirSin * staffSize;
        double yStaff = dirCos * staffSize;
        double xSpace = dirSin * fletchDelta;
        double ySpace = dirCos * fletchDelta;
        double xFletch = fletchSin * fletchSize;
        double yFletch = fletchCos * fletchSize;

        // Now figure out how many flags, fletches, and halfFletches we need.
        int flagCount = 0;
        int fletchCount = 0;
        int halfFletchCount = 0;

        // count the number of flags
        while (speed >= 50.0) {
            flagCount++;
            speed -= 50.0;
        }

        // count the number of fletches
        while (speed >= 10.0) {
            fletchCount++;
            speed -= 10.0;
        }

        // count the number of half fletches
        while (speed >= 5.0) {
            halfFletchCount++;
            speed -= 5.0;
        }

        // calculate the little rectangle
        int yPixel = scaleVisual.getPointForValue(mag).y;
        paintLittleRectangle(event, xPixel, yPixel);

        // if the speed is less than 2.5, we're done.
        // We could put the rest of the code in an IF block, too.
        if (mag >= 2.5) {

            // Figure out many fletch and flag units are required
            int fletchDeltaCount = fletchCount;
            int flagDeltaCount = 0;
            if (mag < 10.0) {
                fletchDeltaCount = 1;
            }
            if (flagCount > 0) {
                flagDeltaCount = flagCount + 1;
            }

            // calculate the staffSize length based on the number of units
            Point v1 = new Point(xPixel, yPixel);
            Point v2 = new Point(
                    xPixel
                            + (int) (0.5 + dirSin
                                    * (staffSize + fletchDelta
                                            * (flagDeltaCount
                                                    + fletchDeltaCount - 1))),
                    yPixel
                            - (int) ((-0.5 + dirCos
                                    * (staffSize + fletchDelta
                                            * (flagDeltaCount
                                                    + fletchDeltaCount - 1)))));
            gc.drawLine(v1.x, v1.y, v2.x, v2.y);

            // Now calculate the locations for the fletches and flags
            // Half fletches first
            if (halfFletchCount > 0) {
                v1.x = xPixel
                        + (int) (0.5 + dirSin * (staffSize - fletchDelta));
                v1.y = yPixel
                        + (int) (0.5 - dirCos * (staffSize - fletchDelta));
                v2.x = v1.x + (int) ((xFletch * 0.6) + 0.5);
                v2.y = v1.y - (int) ((yFletch * 0.6) + 0.5);

                gc.drawLine(v1.x, v1.y, v2.x, v2.y);
            }

            // On to the full-size fletches
            int elementCount = 0;
            for (int i = 0; i < fletchCount; i++, elementCount++) {
                v1.x = xPixel + (int) (xStaff + elementCount * xSpace + 0.5);
                v1.y = yPixel + (int) (-yStaff - elementCount * ySpace + 0.5);
                v2.x = v1.x + (int) (xFletch + 0.5);
                v2.y = v1.y - (int) (yFletch + 0.5);
                gc.drawLine(v1.x, v1.y, v2.x, v2.y);
            }

            // and finally the flags
            Point v3 = new Point(0, 0);
            for (int i = 0; i < flagCount; i++) {
                // the first side is just like a regular fletch
                v1.x = xPixel + (int) (xStaff + elementCount * xSpace + 0.5);
                v1.y = yPixel + (int) (-yStaff - elementCount * ySpace + 0.5);
                v2.x = v1.x + (int) (xFletch + 0.5);
                v2.y = v1.y - (int) (yFletch + 0.5);
                gc.drawLine(v1.x, v1.y, v2.x, v2.y);

                // the second side connects back to the staffSize
                elementCount++;
                v3.x = xPixel + (int) (xStaff + elementCount * xSpace + 0.5);
                v3.y = yPixel + (int) (-yStaff - elementCount * ySpace + 0.5);
                gc.drawLine(v2.x, v2.y, v3.x, v3.y);
            }
        }
    }

    // -- protected
    // --------------------------------------------------------------
    // VectorVisual::genLittleRectangle()
    //
    // This function inserts the coordinates for a rectangle located around this
    // VectorVisuals location.
    //
    // -- implementation
    // ---------------------------------------------------------
    // Inserts segments which make a 3x3 box.
    // ---------------------------------------------------------------------------
    private void paintLittleRectangle(PaintEvent event, int xLoc, int yLoc) {
        GC gc = event.gc;
        int[] box = { xLoc - 1, yLoc - 1, xLoc + 1, yLoc - 1, xLoc + 1,
                yLoc + 1, xLoc - 1, yLoc + 1 };
        gc.drawPolygon(box);
    }

    // -- public
    // -----------------------------------------------------------------
    // WindArrowVisual::render()
    //
    // Renders a wind arrow using the supplied Graphics and current values.
    //
    // -- implementation
    // ---------------------------------------------------------
    // Calculate the tip length and the shaft and each tip's x and y component
    // based on the direction and the tipAngle. Get the XSegementBuffer and
    // define the segments for the rectangle, the arrow shaft and the arrow
    // tips. Finally make the X call to draw the arrow.
    // ---------------------------------------------------------------------------
    public void paintWindArrow(PaintEvent event, Parm parm, float mag,
            float dir, float size, int xPixel) {
        final double tipLength = size / 5.0;
        GC gc = event.gc;
        double arrowDir = dir + 180; // arrows point down wind
        double dirSin = Math.sin(arrowDir * radPerDeg);
        double dirCos = Math.cos(arrowDir * radPerDeg);
        double tip1Sin = Math.sin((arrowDir + tipAngle) * radPerDeg);
        double tip1Cos = Math.cos((arrowDir + tipAngle) * radPerDeg);
        double tip2Sin = Math.sin((arrowDir - tipAngle) * radPerDeg);
        double tip2Cos = Math.cos((arrowDir - tipAngle) * radPerDeg);

        // Some handy vectors.
        Point v1 = new Point((int) (dirSin * size + 0.5),
                -(int) (dirCos * size + 0.5));
        Point v2 = new Point((int) (tip1Sin * tipLength + 0.5), -(int) (tip1Cos
                * tipLength + 0.5));
        Point v3 = new Point((int) (tip2Sin * tipLength + 0.5), -(int) (tip2Cos
                * tipLength + 0.5));

        // calculate the little rectangle
        int yPixel = scaleVisual.getPointForValue(mag).y;
        paintLittleRectangle(event, xPixel, yPixel);

        // Calculate the arrow shaft
        gc.drawLine(xPixel, yPixel, xPixel + v1.x, yPixel + v1.y);

        // And now the tips
        gc.drawLine(xPixel + v1.x, yPixel + v1.y, xPixel + v1.x + v2.x, yPixel
                + v1.y + v2.y);
        gc.drawLine(xPixel + v1.x, yPixel + v1.y, xPixel + v1.x + v3.x, yPixel
                + v1.y + v3.y);
    }

    /**
     * Zooms the scale in/out by the given factor and moves the pixel height to
     * the center of the scale.
     * 
     * @param zoomFactor
     * @param yZoomCenterPixel
     */
    public void zoom(float zoomFactor, int yZoomCenterPixel) {
        scaleVisual.zoom(zoomFactor, yZoomCenterPixel);
        scaleCanvas.redraw();
        editorCanvas.redraw();
    }

    /**
     * Scrolls the scale by the given number of pixels.
     * 
     * @param pixels
     */
    public void pan(int pixels) {
        scaleVisual.pan(pixels);
        scaleCanvas.redraw();
        editorCanvas.redraw();
    }

    /**
     * 
     */
    public void fitToData() {
        if (max != -Float.MAX_VALUE && min != Float.MAX_VALUE) {
            float spacing = (max - min) * 0.2f;
            scaleVisual.setVisibleRange(min - spacing, max + spacing);
            scaleCanvas.redraw();
            editorCanvas.redraw();
        }
    }

    /**
     * 
     */
    public void fullView() {
        scaleVisual.fullView();
        scaleCanvas.redraw();
        editorCanvas.redraw();
    }

    /**
     * 
     * @return
     */
    public ScaleVisual getScale() {
        return scaleVisual;
    }

    /**
     * 
     */
    @Override
    public Parm getClosestParm(Date date, float val) {
        Parm closestParm = null;
        TimeRange range = teUtil.dateToHour(date);
        float closestVal = 0;

        for (Parm parm : parmList) {
            TEParmDisplayAttributes parmDispAtt = parmDisplayAttributesMap
                    .get(parm);
            IGridData grid = parm.overlappingGrid(date);

            if (parmDispAtt.isDisplayed() && parm.isOkToEdit(range)
                    && grid != null) {
                float ave = getAverage(parm, date);
                if (closestParm == null
                        || (Math.abs(ave - val) < Math.abs(closestVal - val))) {
                    closestParm = parm;
                    closestVal = ave;
                }
            }
        }

        return closestParm;
    }

    /**
     * 
     */
    @Override
    public float getAverage(Parm parm, Date date) {
        StatisticsMode mode = temporalEditor.getMode();
        int modMin = temporalEditor.getModeratedMin();
        int modMax = temporalEditor.getModeratedMax();
        float stdMin = temporalEditor.getStdDevMin();
        float stdMax = temporalEditor.getStdDevMax();

        TimeSeries ts = parmToTimeSeries.get(parm);
        HistSample sample = ts.getSampleForDate(date);

        if (sample != null) {
            WxValue aveValue;
            float ave;
            TEParmDisplayAttributes dispAtt = parmDisplayAttributesMap
                    .get(parm);

            if (dispAtt.hasTimeBar()) {
                aveValue = WxValue.getValue(sample.average(true), parm);
            } else {
                switch (mode) {
                case MODERATED:
                    aveValue = WxValue
                            .getValue(sample.moderatedAverage(modMin, modMax,
                                    true), parm);
                    break;
                case STANDARD_DEVIATION:
                    aveValue = WxValue.getValue(
                            sample.stdDevAvg(stdMin, stdMax, true), parm);
                    break;
                case ABSOLUTE:
                default:
                    aveValue = WxValue.getValue(sample.average(true), parm);
                    break;
                }
            }

            // extract the data values
            if (aveValue instanceof ScalarWxValue) {
                ave = ((ScalarWxValue) aveValue).getValue();
            } else {
                ave = ((VectorWxValue) aveValue).getValue();
            }

            return ave;
        }

        // UFStatus.handle(Priority.PROBLEM, Activator.PLUGIN_ID,
        // StatusConstants.CATEGORY_GFE, null,
        // "Could not determine average for Parm["
        // + parm.getFormattedString() + "] date[" + date + "]");
        return -Float.MAX_VALUE;
    }

    /**
     * 
     */
    public void showQuickViewGrid(IGridData grid) {
        GridID gid = null;

        if (grid != null) {
            gid = new GridID(grid.getParm(), grid.getGridTime().getStart());
        }

        if (gid != null && gid.equals(quickviewGrid)) {
            return;
        } else if (gid == null && quickviewGrid == null) {
            return;
        }

        quickviewGrid = gid;

        new ShowQuickViewDataMsg(quickviewGrid).send();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.gfe.core.msgs.Message.IMessageClient#receiveMessage(
     * com.raytheon.viz.gfe.core.msgs.Message)
     */
    @Override
    public void receiveMessage(Message message) {
        if (message instanceof QuickViewModeChangedMsg) {
            boolean enabled = ((QuickViewModeChangedMsg) message).isEnabled();

            if (enabled && !quickviewMode) {
                quickviewMode = true;
                if (editorCanvas != null) {
                    editorCanvas.addMouseMoveListener(quickviewMouseListener);
                    editorCanvas.addMouseTrackListener(quickviewMouseListener);
                }
            } else if (!enabled && quickviewMode) {
                if (editorCanvas != null) {
                    editorCanvas
                            .removeMouseMoveListener(quickviewMouseListener);
                    editorCanvas
                            .removeMouseTrackListener(quickviewMouseListener);
                }
            }

            quickviewMode = enabled;

            if (!quickviewMode && (quickviewGrid != null)) {
                showQuickViewGrid(null);
            }
        }
    }
}
