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

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IDisposeListener;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.msgs.IParmInventoryChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.raytheon.viz.gfe.core.wxvalue.VectorWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.gridmanager.MouseHandler;
import com.raytheon.viz.gfe.rsc.GFEFonts;
import com.raytheon.viz.gfe.sampler.HistSample;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditor.StatisticsMode;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditorUtil.TextJustify;
import com.raytheon.viz.gfe.temporaleditor.mousehandler.LabelMouseHandler;
import com.raytheon.viz.gfe.temporaleditor.mousehandler.TitleBarMouseHandler;

/**
 * Displays the Temporal Editor Data
 *
 * Note: this class has a natural ordering that is inconsistent with equals.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 30, 2009  2159     rjpeter   Initial creation.
 * Oct 29, 2014  3776     randerso  Renamed static variables to match AWIPS
 *                                  standards
 * Mar 10, 2016  5479     randerso  Use improved GFEFonts API
 * Jan 23, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 * Mar 19, 2018  6859     dgilling  Listen for color changes from GFEResources.
 *
 * </pre>
 *
 * @author rjpeter
 */
public abstract class AbstractTemporalEditorBar
        implements Comparable<AbstractTemporalEditorBar> {

    private class TEParmResourceChangeListener
            implements IResourceDataChanged {

        private final Parm parm;

        private TEParmResourceChangeListener(Parm parm) {
            this.parm = parm;
        }

        @Override
        public void resourceChanged(ChangeType type, Object object) {
            if (handleResourceChangeEvent(parm, type, object)) {
                redraw();
            }
        }
    }

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractTemporalEditorBar.class);

    protected static final Color DEFAULT_COLOR = Display.getDefault()
            .getSystemColor(SWT.COLOR_GRAY);

    protected static final Color PARM_BORDER = Display.getDefault()
            .getSystemColor(SWT.COLOR_WHITE);

    protected static final Color PARM_DISPLAYED_TEXT_COLOR = Display
            .getDefault().getSystemColor(SWT.COLOR_BLACK);

    protected static final Color PARM_NOTDISPLAYED_TEXT_COLOR = Display
            .getDefault().getSystemColor(SWT.COLOR_WHITE);

    protected static final Color PARM_IMAGE_BACKGROUND_COLOR = Display
            .getDefault().getSystemColor(SWT.COLOR_GRAY);

    protected static final Color LABEL_COLOR = Display.getDefault()
            .getSystemColor(SWT.COLOR_GRAY);

    protected static final Color BACKGROUND_COLOR = Display.getDefault()
            .getSystemColor(SWT.COLOR_BLACK);

    protected static final int DRAGGABLE_LABEL_HEIGHT = 4;

    protected static final int MIN_BAR_HEIGHT = 65;

    protected static final int DEFAULT_BAR_HEIGHT = 130;

    private static final int TITLEBAR_HEIGHT = 20;

    private static final int TITLEBAR_MARGIN = 2;

    /**
     * Width of Scale in pixels
     */
    public static final int SCALE_WIDTH = 60;

    private static final int IMAGE_TOGGLE_BOX_HEIGHT = 5;

    private static final int IMAGE_TOGGLE_BOX_WIDTH = 10;

    private static final int TITLEBAR_PARM_SPACING = 8;

    protected static float radPerDeg = 0.0174533f;

    // in degrees from the barb shaft
    protected static float fletchAngle = 60.0f;

    protected final DataManager dataMgr;

    protected MouseHandler topLabelMouseHandler;

    protected MouseHandler titleBarMouseHandler;

    protected MouseHandler editorMouseHandler;

    protected MouseHandler bottomLabelMouseHandler;

    protected TimeSeriesChangedListener timesSeriesListener = new TimeSeriesChangedListener();

    protected TemporalEditor temporalEditor;

    protected Composite container;

    protected Label topLabel;

    protected Canvas titleBarCanvas;

    protected Canvas scaleCanvas;

    protected Canvas editorCanvas;

    protected Label bottomLabel;

    protected Cursor resizeCursor;

    /**
     * Parms in this bar.
     */
    protected List<Parm> parmList = new ArrayList<>();

    /**
     * Time Series for a given parm.
     */
    protected Map<Parm, TimeSeries> parmToTimeSeries = new HashMap<>();

    /**
     * Map of parm display attributes
     */
    protected Map<Parm, TEParmDisplayAttributes> parmDisplayAttributesMap = new HashMap<>();

    /**
     *
     */
    protected Map<Parm, Color> parmBaseColorMap = new HashMap<>();

    /**
     * Location of the display toggle box for the parm.
     */
    protected Map<Parm, Rectangle> parmDisplayToggleBoxMap = new HashMap<>();

    /**
     * Location of the graphic toggle box for the parm.
     */
    protected Map<Parm, Rectangle> parmGraphicToggleBoxMap = new HashMap<>();

    protected TemporalEditorUtil teUtil;

    protected IParmInventoryChangedListener parmChgListener;

    protected WeakReference<GC> lastLabelGc;

    protected Font labelFont;

    protected boolean showEditorTimeLines;

    protected boolean showSplitBoundaries;

    private final Map<Parm, IResourceDataChanged> listenerMap;

    /**
     * Factory method to create a Temporal Editor bar for a parm
     *
     * @param parent
     * @param teUtil
     * @param parm
     * @param ts
     * @return the temporal editor bar
     */
    public static AbstractTemporalEditorBar instanceFor(TemporalEditor parent,
            TemporalEditorUtil teUtil, Parm parm, TimeSeries ts,
            DataManager dataMgr) {
        GridType gridType = parm.getGridInfo().getGridType();

        switch (gridType) {
        case SCALAR:
        case VECTOR:
            return new TemporalEditorNumericBar(parent, teUtil, parm, ts,
                    dataMgr);
        case DISCRETE:
            return new TemporalEditorDiscreteBar(parent, teUtil, parm, ts,
                    dataMgr);
        case WEATHER:
            return new TemporalEditorWeatherBar(parent, teUtil, parm, ts,
                    dataMgr);
        default:
            throw new IllegalArgumentException("Unknown GridType: " + gridType);
        }
    }

    /**
     * @param parent
     * @param teUtil
     * @param parm
     * @param ts
     */
    protected AbstractTemporalEditorBar(TemporalEditor parent,
            TemporalEditorUtil teUtil, Parm parm, TimeSeries ts,
            DataManager dataMgr) {
        this.temporalEditor = parent;
        this.teUtil = teUtil;
        this.dataMgr = dataMgr;
        this.resizeCursor = new Cursor(parent.getDisplay(), SWT.CURSOR_SIZENS);
        parmChgListener = new IParmInventoryChangedListener() {
            @Override
            public void parmInventoryChanged(Parm parm, TimeRange timeRange) {
                VizApp.runAsync(new Runnable() {
                    @Override
                    public void run() {
                        redraw();
                    }
                });
            }
        };
        this.listenerMap = new HashMap<>();

        topLabelMouseHandler = new LabelMouseHandler(this,
                LabelMouseHandler.TOP_ORIENTATION);
        topLabelMouseHandler.setDragTolerance(1);
        titleBarMouseHandler = new TitleBarMouseHandler(this);
        bottomLabelMouseHandler = new LabelMouseHandler(this,
                LabelMouseHandler.BOTTOM_ORIENTATION);
        bottomLabelMouseHandler.setDragTolerance(1);
        showEditorTimeLines = true;
        if (GFEPreference.contains("EditorTimeLines")) {
            showEditorTimeLines = GFEPreference.getBoolean("EditorTimeLines");
        }
        showSplitBoundaries = true;
        if (GFEPreference.contains("SplitBoundaryDisplay")) {
            showSplitBoundaries = GFEPreference
                    .getBoolean("SplitBoundaryDisplay");
        }
    }

    /**
     *
     * @param parm
     * @param ts
     */
    public void addParm(Parm parm, TimeSeries ts) {
        if (!parmList.contains(parm)) {
            parmList.add(parm);

            // re-sort the parm list
            Collections.sort(parmList);
            parmToTimeSeries.put(parm, ts);

            if (container != null) {
                int height;
                String property = parm.getParmID().getParmName()
                        + "_temporalDataPaneSize";
                height = GFEPreference.getInt(property);
                height = (height == 0) ? DEFAULT_BAR_HEIGHT : height;
                height = Math.max(MIN_BAR_HEIGHT, height);
                height = Math.max(height,
                        ((GridData) container.getLayoutData()).heightHint);
                ((GridData) container.getLayoutData()).heightHint = height;
            }

            ResourcePair rscPair = dataMgr.getSpatialDisplayManager()
                    .getResourcePair(parm);
            if (rscPair != null) {
                IResourceDataChanged listener = new TEParmResourceChangeListener(
                        parm);
                listenerMap.put(parm, listener);
                rscPair.getResourceData().addChangeListener(listener);
                rscPair.getResource().registerListener(new IDisposeListener() {

                    @Override
                    public void disposed(AbstractVizResource<?, ?> rsc) {
                        rsc.getResourceData().removeChangeListener(listener);
                    }
                });
            }
            parmBaseColorMap.put(parm, new Color(Display.getCurrent(),
                    parm.getDisplayAttributes().getBaseColor()));

            ts.addTimeSeriesChangeListener(timesSeriesListener);
            parmDisplayAttributesMap.put(parm, new TEParmDisplayAttributes());

            parm.getListeners()
                    .addParmInventoryChangedListener(parmChgListener);
        }
    }

    /**
     *
     * @param parm
     */
    public void removeParm(Parm parm) {
        if (parmList.contains(parm)) {
            parmList.remove(parm);
            TimeSeries ts = parmToTimeSeries.remove(parm);
            ts.removeTimeSeriesChangeListener(timesSeriesListener);
            ts.checkRemoveListeners();
            parmDisplayAttributesMap.remove(parm);
            parmDisplayToggleBoxMap.remove(parm);
            parmGraphicToggleBoxMap.remove(parm);
            parmBaseColorMap.remove(parm).dispose();
            parm.getListeners()
                    .removeParmInventoryChangedListener(parmChgListener);

            IResourceDataChanged listener = listenerMap.remove(parm);
            ResourcePair rscPair = dataMgr.getSpatialDisplayManager()
                    .getResourcePair(parm);
            if ((rscPair != null) && (listener != null)) {
                rscPair.getResourceData().removeChangeListener(listener);
            }
        }
    }

    /**
     *
     */
    protected void setupComposite() {
        int height = MIN_BAR_HEIGHT;
        String property;

        int curParmHeight;
        for (Parm parm : parmList) {
            property = parm.getParmID().getParmName() + "_temporalDataPaneSize";
            curParmHeight = GFEPreference.getInt(property);
            curParmHeight = (curParmHeight == 0) ? DEFAULT_BAR_HEIGHT
                    : curParmHeight;
            height = Math.max(height, curParmHeight);
        }

        if ((container != null) && !container.isDisposed()) {
            height = ((GridData) container.getLayoutData()).heightHint;
            container.dispose();
        }

        container = new Composite(temporalEditor, SWT.NONE);
        GridLayout gridLayout = new GridLayout(2, false);
        gridLayout.marginHeight = 0;
        gridLayout.marginWidth = 0;
        gridLayout.verticalSpacing = 0;
        gridLayout.horizontalSpacing = 0;
        container.setLayout(gridLayout);
        GridData gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gridData.heightHint = height;
        container.setLayoutData(gridData);
    }

    /**
     *
     */
    protected void setupTopLabel() {
        if ((topLabel != null) && !topLabel.isDisposed()) {
            topLabel.dispose();
        }

        topLabel = new Label(temporalEditor, SWT.BORDER);
        GridData gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gridData.heightHint = DRAGGABLE_LABEL_HEIGHT;
        gridData.minimumHeight = DRAGGABLE_LABEL_HEIGHT;
        topLabel.setBackground(LABEL_COLOR);
        topLabel.setLayoutData(gridData);
        topLabel.addMouseListener(topLabelMouseHandler);
        topLabel.setCursor(resizeCursor);
    }

    /**
     *
     */
    protected void setupTitleBarCanvas() {
        int height = TITLEBAR_HEIGHT;
        if ((titleBarCanvas != null) && !titleBarCanvas.isDisposed()) {
            titleBarCanvas.removeMouseListener(titleBarMouseHandler);
            height = ((GridData) titleBarCanvas.getLayoutData()).heightHint;
            titleBarCanvas.dispose();
        }

        titleBarCanvas = new Canvas(container, SWT.BORDER);
        GridData gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gridData.horizontalSpan = 2;
        gridData.heightHint = height;
        gridData.minimumHeight = 0;
        titleBarCanvas.setLayoutData(gridData);
        titleBarCanvas.addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                paintTitleBar(e);
            }
        });
        titleBarCanvas.addMouseListener(titleBarMouseHandler);
    }

    /**
     *
     */
    protected abstract void setupScaleCanvas();

    /**
     *
     */
    protected abstract void setupEditorCanvas();

    /**
     *
     */
    protected void setupBottomLabel() {
        if ((bottomLabel != null) && !bottomLabel.isDisposed()) {
            bottomLabel.dispose();
        }

        bottomLabel = new Label(temporalEditor, SWT.BORDER);
        GridData gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gridData.heightHint = DRAGGABLE_LABEL_HEIGHT;
        gridData.minimumHeight = DRAGGABLE_LABEL_HEIGHT;
        bottomLabel.setBackground(LABEL_COLOR);
        bottomLabel.setLayoutData(gridData);
        bottomLabel.addMouseListener(bottomLabelMouseHandler);
        bottomLabel.setCursor(resizeCursor);
    }

    /**
     *
     * @param event
     */
    protected void paintTitleBar(PaintEvent event) {
        GC gc = event.gc;
        Font oldFont = gc.getFont();
        if (labelFont == null) {
            labelFont = GFEFonts.makeGFEFont(gc.getDevice(),
                    "TEDataSelector_font", SWT.NORMAL, 1);
        }

        try {
            if (labelFont != null) {
                gc.setFont(labelFont);
            }

            int fontHeight = gc.getFontMetrics().getAscent();

            Rectangle bounds = titleBarCanvas.getClientArea();
            gc.setBackground(BACKGROUND_COLOR);
            gc.fillRectangle(bounds);

            int offset = bounds.x;
            for (Parm parm : parmList) {
                Color color = parmBaseColorMap.get(parm);
                TEParmDisplayAttributes parmDispAtt = parmDisplayAttributesMap
                        .get(parm);
                String title = TemporalEditorUtil.getTitleBarText(parm);
                Point pt = gc.stringExtent(title);
                offset += TITLEBAR_PARM_SPACING;
                Rectangle textBorder = new Rectangle(offset,
                        bounds.y + TITLEBAR_MARGIN,
                        pt.x + (TITLEBAR_MARGIN * 2),
                        fontHeight + TITLEBAR_MARGIN);

                // draw text
                offset += TITLEBAR_MARGIN;

                // draw white border with colored rect
                if (parmDispAtt.isDisplayed()) {
                    // black text on color
                    paintBorder(event, textBorder, color, PARM_BORDER);
                    gc.setForeground(PARM_DISPLAYED_TEXT_COLOR);
                    gc.drawText(title, offset, textBorder.y - 1, true);
                } else {
                    // white text on black
                    paintBorder(event, textBorder, BACKGROUND_COLOR,
                            PARM_BORDER);
                    gc.setForeground(PARM_NOTDISPLAYED_TEXT_COLOR);
                    gc.drawText(title, offset, textBorder.y - 1, true);
                }

                // draw white border click box
                offset += textBorder.width + 1;
                Rectangle selectBoxBorder = new Rectangle(offset, textBorder.y
                        + ((textBorder.height - IMAGE_TOGGLE_BOX_HEIGHT) / 2),
                        IMAGE_TOGGLE_BOX_WIDTH, IMAGE_TOGGLE_BOX_HEIGHT);

                if (parmDispAtt.isDisplayedAsGraphic()) {
                    paintBorder(event, selectBoxBorder, BACKGROUND_COLOR,
                            PARM_BORDER);
                } else {
                    paintBorder(event, selectBoxBorder,
                            PARM_IMAGE_BACKGROUND_COLOR, PARM_BORDER);
                }

                // increase size of boxes to properly handle mouse events when
                // clicking on the border
                textBorder.width += 1;
                textBorder.height += 2;
                selectBoxBorder.width += 1;
                selectBoxBorder.height += 2;
                parmDisplayToggleBoxMap.put(parm, textBorder);
                parmGraphicToggleBoxMap.put(parm, selectBoxBorder);

                offset += selectBoxBorder.width;
            }
        } finally {
            gc.setFont(oldFont);
        }
    }

    /**
     *
     * @param event
     */
    protected void paintScaleCanvas(PaintEvent event) {
        GC gc = event.gc;
        Rectangle bounds = scaleCanvas.getClientArea();
        gc.setBackground(BACKGROUND_COLOR);
        gc.fillRectangle(bounds);
    }

    /**
     *
     * @param event
     */
    protected abstract void paintEditorCanvas(PaintEvent event);

    /**
     *
     * @param event
     * @param rect
     * @param backgroundColor
     * @param foregroundColor
     */
    protected void paintBorder(PaintEvent event, Rectangle rect,
            Color backgroundColor, Color foregroundColor) {
        GC gc = event.gc;
        gc.setLineStyle(SWT.LINE_SOLID);
        gc.setLineWidth(0);
        gc.setBackground(backgroundColor);
        gc.setForeground(foregroundColor);
        gc.fillRectangle(rect);
        gc.drawRectangle(rect);
    }

    /**
     * Paints all parm locks
     *
     * @param event
     */
    protected void paintLocks(PaintEvent event) {
        GC gc = event.gc;
        TimeRange range = teUtil.getVisibleTimeRange();
        int height = editorCanvas.getClientArea().height;

        for (Parm parm : parmList) {
            LockTable lockTable = parm.getLockTable();

            gc.setBackgroundPattern(
                    temporalEditor.getGridBarPrefs().getLockedByOther());
            for (TimeRange timeRange : lockTable.lockedByOther()) {
                if (timeRange.overlaps(range)) {
                    Rectangle rect = teUtil.timeRangeToPixels(timeRange);
                    rect.y = 0;
                    rect.height = height;
                    gc.fillRectangle(rect);
                }
            }
        }
    }

    /**
     * Routine to paint a sample label for the visualization
     *
     * Based on TEVisual::paintLabel.
     *
     * @param gc
     * @param txt
     *            The label to paint
     * @param tr
     *            the time period the label must fit in
     * @param yPos
     *            the y position
     * @param yOffset
     *            y offset in pixels
     * @param vTxtJust
     *            vertical text justification
     * @param yPixelsMax
     */
    protected void paintLabel(GC gc, String txt, TimeRange tr, int yPos,
            int yOffset, TextJustify vTxtJust, int yPixelsMax) {

        // is there enough room to plot labelText? Amount of
        // "time" in x direction is the duration of this
        // TimeRange.
        int xPixels = teUtil.durationToPixels(tr.getDuration());
        Point txtSize = gc.textExtent(txt);
        if ((xPixels >= txtSize.x) && (yPixelsMax >= txtSize.y)) {
            // paint the label
            int xLoc = teUtil.dateToPixel(tr.getCenterTime());
            int yLoc = yPos + yOffset;
            TemporalEditorUtil.drawJustifiedText(gc, txt, xLoc, yLoc, vTxtJust,
                    TextJustify.CENTER);
        }
    }

    protected boolean handleResourceChangeEvent(Parm p, ChangeType type,
            Object eventData) {
        if ((type == ChangeType.CAPABILITY)
                && (eventData instanceof ColorableCapability)) {
            Color newColor = new Color(Display.getCurrent(),
                    ((ColorableCapability) eventData).getColor());
            Color oldColor = parmBaseColorMap.put(p, newColor);
            if (oldColor != null) {
                oldColor.dispose();
            }

            return true;
        }

        return false;
    }

    /**
     * @return the height of the temporal editor bar
     */
    public int getHeight() {
        return container.getBounds().height + topLabel.getBounds().height
                + bottomLabel.getBounds().height;
    }

    /**
     * @return the parm
     */
    public List<Parm> getParms() {
        return parmList;
    }

    /**
     *
     */
    public void dispose() {
        List<Parm> parmsToRemove = new ArrayList<>(parmList);
        for (Parm parmToRemove : parmsToRemove) {
            removeParm(parmToRemove);
        }

        topLabel.dispose();
        container.dispose();
        bottomLabel.dispose();
        resizeCursor.dispose();
        if ((labelFont != null) && !labelFont.isDisposed()) {
            labelFont.dispose();
            labelFont = null;
        }
    }

    /**
     *
     */
    public void redraw() {
        if (!topLabel.isDisposed() && !container.isDisposed()
                && !bottomLabel.isDisposed()) {
            topLabel.redraw();
            titleBarCanvas.redraw();
            scaleCanvas.redraw();
            editorCanvas.redraw();
            bottomLabel.redraw();
        }
    }

    /**
     *
     */
    public void resetLocation() {
        setupTopLabel();
        setupComposite();
        setupTitleBarCanvas();
        setupScaleCanvas();
        setupEditorCanvas();
        setupBottomLabel();
        container.layout();
    }

    @Override
    public int compareTo(AbstractTemporalEditorBar o) {
        int compare = 0;
        if ((!this.parmList.isEmpty()) && (!o.parmList.isEmpty())) {
            List<Parm> list1 = new ArrayList<>(this.parmList);
            List<Parm> list2 = new ArrayList<>(o.parmList);
            Collections.sort(list1);
            Collections.sort(list2);
            Parm p1 = list1.get(0);
            Parm p2 = list2.get(0);
            compare = p1.compareTo(p2);
        } else if (!this.parmList.isEmpty()) {
            compare = 1;
        } else if (!o.parmList.isEmpty()) {
            compare = -1;
        }

        return compare;
    }

    /**
     * @return true if this temporal editor bar is disposed
     */
    public boolean isDisposed() {
        return topLabel.isDisposed() || container.isDisposed()
                || bottomLabel.isDisposed();
    }

    /**
     * @param parm
     * @return the time series for the parm
     */
    public TimeSeries getTimeSeriesForParm(Parm parm) {
        return parmToTimeSeries.get(parm);
    }

    /**
     * @param parm
     * @return the display attributes for the parm
     */
    public TEParmDisplayAttributes getParmDisplayAttributes(Parm parm) {
        return parmDisplayAttributesMap.get(parm);
    }

    /**
     * Set the display attributes for the parm
     *
     * @param parm
     * @param dispAtt
     */
    public void setParmDisplayAttributes(Parm parm,
            TEParmDisplayAttributes dispAtt) {
        parmDisplayAttributesMap.put(parm, dispAtt);
    }

    /**
     * @return the composite containing this temporal editor bar
     */
    public Composite getContainer() {
        return container;
    }

    /**
     * toggle display of parm
     *
     * @param parm
     */
    public void toggleParmDisplayed(Parm parm) {
        TEParmDisplayAttributes parmDispAtt = parmDisplayAttributesMap
                .get(parm);
        parmDispAtt.setDisplayed(!parmDispAtt.isDisplayed());
        redraw();
    }

    /**
     * toggle display as graphic for parm
     *
     * @param parm
     */
    public void toggleParmDisplayedAsGraphic(Parm parm) {
        TEParmDisplayAttributes parmDispAtt = parmDisplayAttributesMap
                .get(parm);

        if (parmDispAtt.isDisplayedAsGraphic()) {
            for (TEParmDisplayAttributes parmToGraphic : parmDisplayAttributesMap
                    .values()) {
                parmToGraphic.setDisplayedAsGraphic(true);
            }
        }

        parmDispAtt.setDisplayedAsGraphic(!parmDispAtt.isDisplayedAsGraphic());
        parmDispAtt.setDisplayed(true);
        redraw();
    }

    /**
     * @param pt
     * @return the parm whose title bar contains pt
     */
    public Parm getClickedTitleBarParm(Point pt) {
        return getClickedTitleBarParm(pt, true, true);
    }

    /**
     * @param pt
     * @param clickedOnDisplayedRect
     * @param clickedOnGraphicRect
     * @return the parm whose title bar contains pt
     */
    public Parm getClickedTitleBarParm(Point pt, boolean clickedOnDisplayedRect,
            boolean clickedOnGraphicRect) {
        int titleBarHeight = titleBarCanvas.getClientArea().height;
        for (Parm parm : parmList) {
            Rectangle parmDispRect = parmDisplayToggleBoxMap.get(parm);
            Rectangle parmGraphicRect = parmGraphicToggleBoxMap.get(parm);

            if (clickedOnDisplayedRect && clickedOnGraphicRect) {
                Rectangle rect = new Rectangle(parmDispRect.x, parmDispRect.y,
                        (parmGraphicRect.x - parmDispRect.x)
                                + parmGraphicRect.width,
                        titleBarHeight);
                if (rect.contains(pt)) {
                    return parm;
                }
            } else if (clickedOnDisplayedRect && parmDispRect.contains(pt)) {
                return parm;
            } else if (clickedOnGraphicRect && parmGraphicRect.contains(pt)) {
                return parm;
            }
        }

        return null;
    }

    /**
     * @param date
     * @param val
     * @return the closest parm for the date and value
     */
    public Parm getClosestParm(Date date, float val) {
        Parm closestParm = null;
        TimeRange range = teUtil.dateToHour(date);
        float closestVal = 0;

        for (Parm parm : parmList) {
            TEParmDisplayAttributes parmDispAtt = parmDisplayAttributesMap
                    .get(parm);
            IGridData grid = parm.overlappingGrid(date);

            if (parmDispAtt.isDisplayed() && parm.isOkToEdit(range)
                    && (grid != null)) {
                float ave = getAverage(parm, date);
                if ((closestParm == null)
                        || (Math.abs(ave - val) < Math.abs(closestVal - val))) {
                    closestParm = parm;
                    closestVal = ave;
                }
            }
        }

        return closestParm;
    }

    /**
     * @return the TemporalEditorUtil
     */
    public TemporalEditorUtil getUtil() {
        return teUtil;
    }

    /**
     * @return the temporalEditor
     */
    public TemporalEditor getTemporalEditor() {
        return temporalEditor;
    }

    /**
     * @param parm
     * @param date
     * @return the average value for parm on date
     */
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

            switch (mode) {
            case MODERATED:
                aveValue = WxValue.getValue(
                        sample.moderatedAverage(modMin, modMax, true), parm);
                break;
            case STANDARD_DEVIATION:
                aveValue = WxValue
                        .getValue(sample.stdDevAvg(stdMin, stdMax, true), parm);
                break;
            case ABSOLUTE:
            default:
                aveValue = WxValue.getValue(sample.average(true), parm);
                break;
            }

            // extract the data values
            if (aveValue instanceof ScalarWxValue) {
                ave = ((ScalarWxValue) aveValue).getValue();
            } else {
                ave = ((VectorWxValue) aveValue).getValue();
            }

            return ave;
        }

        statusHandler.handle(Priority.PROBLEM,
                "Could not determine average for Parm["
                        + parm.getFormattedString() + "] date[" + date + "]");
        return -Float.MAX_VALUE;
    }

    private class TimeSeriesChangedListener
            implements ITimeSeriesChangedListener {
        @Override
        public void timeSeriesChanged(TimeSeries ts) {
            editorCanvas.redraw();
        }
    }
}
