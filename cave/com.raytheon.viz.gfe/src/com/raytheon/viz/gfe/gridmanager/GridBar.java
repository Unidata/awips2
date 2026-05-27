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

package com.raytheon.viz.gfe.gridmanager;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Pattern;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.dataplugin.gfe.server.lock.LockTable;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.GridManagerView;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.msgs.ClearHighlightsMsg;
import com.raytheon.viz.gfe.core.msgs.GMDisplayModeMsg;
import com.raytheon.viz.gfe.core.msgs.HighlightMsg;
import com.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener;
import com.raytheon.viz.gfe.core.msgs.IGridDataChangedListener;
import com.raytheon.viz.gfe.core.msgs.IGridHistoryUpdatedListener;
import com.raytheon.viz.gfe.core.msgs.IGridVisibilityChangedListener;
import com.raytheon.viz.gfe.core.msgs.ILockTableChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParameterSelectionChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmIDChangedListener;
import com.raytheon.viz.gfe.core.msgs.IParmInventoryChangedListener;
import com.raytheon.viz.gfe.core.msgs.ISelectionTimeRangeChangedListener;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmState;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.gfe.rsc.GFEFonts;

/**
 * Displays the Grid Manager Data.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Feb 19, 2008           dfitch    Initial creation.
 * Apr 07, 2009  2212     randerso  Reimplemented
 * Jun 23, 2011  9897     ryu       Update static variables on new GFE config
 * Oct 29, 2014  3776     randerso  Cached fill patterns used in history mode
 *                                  Renamed static variables to match AWIPS
 *                                  standards
 * Mar 10, 2016  5479     randerso  Use improved GFEFonts API
 * Jan 23, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 * Feb 19, 2018  7117     randerso  Fixed grid display for satellite grids
 * Jan 29, 2019  7749     randerso  Notify the GridManagerView of changes in
 *                                  displayed parm lock status.
 *
 * </pre>
 *
 * @author dfitch
 */

public class GridBar implements IMessageClient, IParmInventoryChangedListener,
        IGridVisibilityChangedListener, IActivatedParmChangedListener,
        IGridDataChangedListener, ISelectionTimeRangeChangedListener,
        IParameterSelectionChangedListener, ILockTableChangedListener,
        IParmIDChangedListener, IGridHistoryUpdatedListener {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridBar.class);

    private static final Date BASE_DATE = new Date(0);

    private static final Color DEFAULT_COLOR = new Color(null,
            RGBColors.getRGBColor("gray75"));

    private static final int HEIGHT = 29;

    private static final int MARGIN = 3;

    protected static final int DATA_BLOCK_HORIZONTAL_MARGIN = 2;

    private static final int DATA_BLOCK_HEIGHT = 20;

    private static final int DATA_BLOCK_LINE_STYLE = SWT.LINE_DOT;

    private static final int HALF_INTENSITY;

    static {
        Color white = Display.getDefault().getSystemColor(SWT.COLOR_WHITE);
        HALF_INTENSITY = (white.getRed() + white.getGreen() + white.getBlue())
                / 2;
    }

    private GridCanvas canvas;

    private Rectangle bounds;

    private Parm parm;

    private boolean parmActive;

    private boolean parmVisible;

    private List<Highlight> highlights;

    private Map<String, Color> highlightColors;

    private GridManager gridManager;

    private GridBarPreferences prefs;

    private GridMode gridMode;

    private Font timeBlockSourceFont;

    private Font timeBlockLabelFont;

    private Rectangle selectionBox;

    private int descrHeight;

    private int verticalPosition;

    /**
     * @param canvas
     * @param parm
     * @param gridManager
     */
    public GridBar(final GridCanvas canvas, final Parm parm,
            final GridManager gridManager) {
        this.canvas = canvas;
        this.parm = parm;
        this.gridManager = gridManager;
        this.prefs = gridManager.getGridBarPrefs();

        highlights = new ArrayList<>();
        highlightColors = new HashMap<>();

        gridMode = Message.inquireLastMessage(GMDisplayModeMsg.class)
                .getGridMode();

        Message.registerInterest(this, HighlightMsg.class,
                ClearHighlightsMsg.class, GMDisplayModeMsg.class);

        this.parm.getListeners().addParmInventoryChangedListener(this);
        this.parm.getListeners().addGridChangedListener(this);
        this.parm.getListeners().addGridHistoryUpdatedListener(this);
        this.parm.getListeners().addSelectionTimeRangeChangedListener(this);
        this.parm.getListeners().addParameterSelectionChangedListener(this);
        this.parm.getListeners().addLockTableChangedListener(this);
        this.parm.getListeners().addParmIDChangedListener(this);

        gridManager.getDataManager().getSpatialDisplayManager()
                .addActivatedParmChangedListener(this);

        gridManager.getDataManager().getSpatialDisplayManager()
                .addGridVisibilityChangedListener(this);

        timeBlockSourceFont = GFEFonts.makeGFEFont(canvas.getDisplay(),
                "TimeBlockSource_font", SWT.NORMAL, 1);
        timeBlockLabelFont = GFEFonts.makeGFEFont(canvas.getDisplay(),
                "TimeBlockLabel_font", SWT.BOLD, 3);

        GC gc = new GC(canvas);
        gc.setFont(timeBlockLabelFont);
        descrHeight = gc.getFontMetrics().getAscent();
        gc.dispose();
    }

    /**
     * Clean up resources and listeners
     */
    public void dispose() {
        Message.unregisterInterest(this, HighlightMsg.class,
                ClearHighlightsMsg.class, GMDisplayModeMsg.class);

        parm.getListeners().removeParmInventoryChangedListener(this);
        parm.getListeners().removeGridChangedListener(this);
        parm.getListeners().removeGridHistoryUpdatedListener(this);
        parm.getListeners().removeSelectionTimeRangeChangedListener(this);
        parm.getListeners().removeParameterSelectionChangedListener(this);
        parm.getListeners().removeLockTableChangedListener(this);
        parm.getListeners().removeParmIDChangedListener(this);

        gridManager.getDataManager().getSpatialDisplayManager()
                .removeActivatedParmChangedListener(this);

        gridManager.getDataManager().getSpatialDisplayManager()
                .removeGridVisibilityChangedListener(this);

        if ((timeBlockSourceFont != null)
                && !timeBlockSourceFont.isDisposed()) {
            timeBlockSourceFont.dispose();
            timeBlockSourceFont = null;
        }

        if ((timeBlockLabelFont != null) && !timeBlockLabelFont.isDisposed()) {
            timeBlockLabelFont.dispose();
            timeBlockLabelFont = null;
        }
    }

    @Override
    public void receiveMessage(Message message) {
        if (message instanceof HighlightMsg) {
            highlightMsg((HighlightMsg) message);

        } else if (message instanceof ClearHighlightsMsg) {
            clearHighlightMsg((ClearHighlightsMsg) message);

        } else if (message instanceof GMDisplayModeMsg) {
            gridMode = ((GMDisplayModeMsg) message).getGridMode();
            redraw();
        } else {
            statusHandler.handle(Priority.PROBLEM,
                    "Unexpected message type recieved: "
                            + message.getClass().getName());
        }
    }

    /**
     * @param y
     *            the verticalPosition
     */
    public void setVerticalPosition(int y) {
        this.verticalPosition = y;
        computeBounds();

    }

    /**
     * @return the parm
     */
    public Parm getParm() {
        return parm;
    }

    /**
     * @return the parmVisible
     */
    public boolean isParmVisible() {
        return parmVisible;
    }

    protected void paint(PaintEvent event) {
        gridManager.getUtil().paintBackground(event, getBounds());
        paintLocks(event);
        gridManager.getUtil().paintTimeScaleLines(event, getBounds());
        paintSelectionTimeRange(event);
        // Retrieves the Grid Inventory from the parm and paints the blocks.
        IGridData data[] = parm.getGridInventory();
        TimeRange gridsTimeRange[] = new TimeRange[data.length];
        for (int i = 0; i < data.length; i++) {
            gridsTimeRange[i] = data[i].getGridTime();
        }
        if (prefs.isShowSplitBoundaries()) {
            paintEmptyBlocks(event, parm.getGridInfo().getTimeConstraints(),
                    gridsTimeRange);
        }
        if (prefs.isShowEditorTimeLines()) {
            gridManager.getUtil().paintSelected(event, getBounds());
        }
        paintDescription(event, getBounds());
        paintDataBlocks(event, gridsTimeRange);
    }

    private void paintSelectionTimeRange(PaintEvent event) {
        if (!parm.getParmState().isSelected()) {
            return;
        }
        TimeRange timeRange = parm.getParmState().getSelectedTimeRange();
        if ((timeRange != null) && timeRange.isValid()) {
            Rectangle selection = gridManager.getUtil()
                    .timeRangeToPixels(timeRange).intersection(getBounds());

            gridManager.getUtil().paintSelectionTimeRange(event, selection);
        }
    }

    /**
     * Paints the selection box and the Description of the Parm
     *
     * @param gc
     */
    private void paintDescription(PaintEvent event, Rectangle rect) {
        GC gc = event.gc;

        Font origFont = gc.getFont();

        gc.setFont(timeBlockLabelFont);

        gc.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_BLACK));
        gc.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_WHITE));
        if (parm.getParmState().isSelected()) {
            gridManager.getUtil().paintSelectionTimeRange(event, selectionBox);
        } else {
            gc.fillRectangle(selectionBox);
        }
        gc.setLineStyle(SWT.LINE_SOLID);
        gc.setLineWidth(0);
        gc.drawRectangle(selectionBox);

        StringBuilder sb = new StringBuilder();

        // get the parm name
        String parmText = parm.getParmID().getParmName();
        sb.append(parmText + " ");

        String levelText = parm.getParmID().getParmLevel();
        sb.append(levelText + "  ");

        // get the model name
        String modelText = parm.getParmID().getDbId().getShortModelId();
        sb.append(modelText);

        gc.drawText(sb.toString(), selectionBox.x + selectionBox.width + MARGIN,
                selectionBox.y, true);
        gc.setFont(origFont);
    }

    /**
     * Paints the parm's locks
     *
     * @param parm
     *            The parm
     */
    private void paintLocks(PaintEvent event) {
        GC gc = event.gc;

        LockTable lockTable = parm.getLockTable();

        gc.setBackgroundPattern(prefs.getLockedByMe());
        for (TimeRange timeRange : lockTable.lockedByMe()) {
            if (timeRange.overlaps(this.gridManager.getVisibleTimeRange())) {
                Rectangle rect = computeLockRect(timeRange);
                gc.fillRectangle(rect);
            }

        }

        gc.setBackgroundPattern(prefs.getLockedByOther());
        for (TimeRange timeRange : lockTable.lockedByOther()) {
            if (timeRange.overlaps(this.gridManager.getVisibleTimeRange())) {
                Rectangle rect = computeLockRect(timeRange);
                gc.fillRectangle(rect);
            }
        }

    }

    private void paintEmptyBlocks(PaintEvent event, TimeConstraints constraint,
            TimeRange dataTR[]) {
        GC gc = event.gc;
        TimeRange visibleRange = gridManager.getVisibleTimeRange();
        gc.setForeground(prefs.getTimeBlockInvisibleColor());

        gc.setLineStyle(DATA_BLOCK_LINE_STYLE);
        gc.setLineWidth(0);
        for (TimeRange timeRange : constraint.constraintTimes(visibleRange)) {
            paintShadowBox(gc, timeRange);
        }
    }

    private void paintShadowBox(GC gc, TimeRange dataTR) {
        Rectangle rect = computeRect(dataTR);
        if (rect.width < 4) {
            return;
        }
        rect.width -= 1;
        rect.height -= 1;
        gc.drawRectangle(rect);
    }

    /**
     * Paints the actual data blocks
     *
     * @param dataTR
     */
    private void paintDataBlocks(PaintEvent event, TimeRange dataTR[]) {
        Date spatialEditorTime = gridManager.getDataManager()
                .getSpatialDisplayManager().getSpatialEditorTime();
        if (spatialEditorTime == null) {
            return;
        }
        GridID syncGridID = new GridID(parm, spatialEditorTime);

        GC gc = event.gc;

        for (TimeRange dataTimeRange : dataTR) {

            GridID gridInvenID = new GridID(parm, dataTimeRange.getStart());

            switch (gridMode) {
            case NORMAL:
                String hlColor = highlightColor(dataTimeRange);
                if (gridInvenID.equals(syncGridID)) {
                    paintTimeBlock(gc, syncGridID, parmVisible, parmActive,
                            hlColor, GridMode.NORMAL);
                } else {
                    paintTimeBlock(gc, gridInvenID, false, false, hlColor,
                            GridMode.NORMAL);
                }
                break;

            case HISTORY:
                paintTimeBlock(gc, gridInvenID, false, false, "",
                        GridMode.HISTORY);
                break;
            case LASTSAVED:
            case LASTMODIFIED:
            case LASTPUBLISHED:
            case LASTSENT:
                paintTimeBlock(gc, gridInvenID, false, false, "", gridMode);
                break;

            default:
                break;
            }

        }
    }

    private void paintTimeBlock(GC gc, GridID gridId, boolean visible,
            boolean active, String highlight, GridMode mode) {

        if (gridId.grid() == null) {
            return;
        }

        TimeRange gridTR = gridId.grid().getGridTime();

        String srcStr = "";
        switch (mode) {
        case NORMAL:
            paintActualBlock(gc, gridTR, visible, active, highlight);
            srcStr = sourceString(gridId.grid().getHistory());
            break;

        case HISTORY:
            paintHistoryBlock(gc, gridTR, gridId);
            srcStr = sourceString(gridId.grid().getHistory());
            break;

        case LASTSAVED:
        case LASTMODIFIED:
        case LASTPUBLISHED:
        case LASTSENT: {
            Date[] time = paintSaveTimeBlock(gc, gridTR, gridId, mode);
            if (!time[0].equals(BASE_DATE)) {
                srcStr = formatUpdateTime(time[0], time[1]);
            } else {
                srcStr = new String();
            }
            break;
        }
        }

        paintBlockText(gc, gridTR, srcStr);

    }

    /**
     * Paints the actual data blocks
     *
     * @param dataTR
     */
    private void paintBlockText(GC gc, TimeRange dataTR, String text) {
        Color bkgColor = gc.getBackground();
        int intensity = bkgColor.getRed() + bkgColor.getGreen()
                + bkgColor.getBlue();

        if (intensity < HALF_INTENSITY) {
            gc.setForeground(
                    Display.getCurrent().getSystemColor(SWT.COLOR_WHITE));
        } else {
            gc.setForeground(
                    Display.getCurrent().getSystemColor(SWT.COLOR_BLACK));
        }

        Rectangle rect = computeRect(dataTR);

        gc.setFont(timeBlockSourceFont);
        gc.setTextAntialias(SWT.OFF);
        String s = truncateLabelToFit(gc, text, rect.width);
        Point textSize = gc.stringExtent(s);

        if (textSize.x < (rect.width + DATA_BLOCK_HORIZONTAL_MARGIN)) {
            int xOffset = (rect.width - textSize.x) / 2;
            int yOffset = (rect.height - textSize.y) / 2;
            gc.drawString(s, rect.x + xOffset, rect.y + yOffset, true);
        }
    }

    /**
     * Paints the actual data blocks
     *
     * @param dataTR
     */
    private void paintActualBlock(GC gc, TimeRange dataTR, boolean visible,
            boolean active, String highlight) {

        // if dataTR overlaps the parmVisible time range
        if (!gridManager.getVisibleTimeRange().overlaps(dataTR)) {
            return;
        }

        Rectangle rect = computeRect(dataTR);

        Color hlColor = highlightColors.get(highlight);
        if (hlColor != null) {
            gc.setBackground(hlColor);
            gc.fillRectangle(rect);
            return;
        }

        if (!active && !visible) {
            gc.setBackground(prefs.getTimeBlockInvisibleColor());
            gc.fillRectangle(rect);
        } else if (!active && visible) {
            gc.setBackground(prefs.getTimeBlockVisibleColor());
            gc.fillRectangle(rect);
        } else if (active && visible) {
            gc.setBackground(prefs.getTimeBlockActiveColor());
            gc.fillRectangle(rect);
        } else {
            gc.setBackground(prefs.getTimeBlockInvisibleColor());
            gc.fillRectangle(rect);
            gc.setForeground(prefs.getTimeBlockActiveColor());
            gc.drawRectangle(rect);
        }
    }

    private void paintHistoryBlock(GC gc, TimeRange dataTR, GridID gridID) {

        GridDataHistory emptyHistory = new GridDataHistory();
        GridDataHistory history;
        if (gridID.grid().getHistory().length > 0) {
            history = gridID.grid().getHistory()[0];
        } else {
            history = emptyHistory;
        }

        Color color = null;

        // if initialized, then look up the model color
        switch (history.getOrigin()) {
        case INITIALIZED: {
            String key = "HistoryModelColor_"
                    + history.getOriginParm().getDbId().getModelName();
            color = prefs.getHistoryColor(key);
            if (color == null) {
                // model not defined, try the default
                color = prefs.getHistoryColor("HistoryOriginColor_Populated");
            }
        }
            break;

        case TIME_INTERPOLATED:
            color = prefs.getHistoryColor("HistoryOriginColor_Interpolated");
            break;

        case SCRATCH:
            color = prefs.getHistoryColor("HistoryOriginColor_Scratch");
            break;

        case CALCULATED:
            color = prefs.getHistoryColor("HistoryOriginColor_Calculated");
            break;

        case OTHER:
            color = prefs.getHistoryColor("HistoryOriginColor_Other");
            break;

        default:
            color = prefs.getHistoryColor("HistoryOriginColor_Other");
            break;
        }

        if (color == null) {
            color = DEFAULT_COLOR;
        }

        // determine the fill pattern
        Pattern fp = null;
        if (history.getTimeModified() != null) {
            if (history.getWhoModified()
                    .equals(parm.getDataManager().getWsId())) {
                fp = prefs.getModifiedByMe(color);
            } else {
                fp = prefs.getModifiedByOther(color);
            }
        }

        // now do the drawing
        Rectangle rect = computeRect(dataTR);
        gc.setBackground(color);
        gc.setBackgroundPattern(fp);
        gc.fillRectangle(rect);

        // if a pattern was used, then return a black for the interior color
        if (fp != null) {
            gc.setBackground(
                    Display.getCurrent().getSystemColor(SWT.COLOR_BLACK));
        }
    }

    /**
     * Formats the a string using the grid data history.
     *
     * @param histories
     *            The grid data history.
     * @return The properly formated string.
     */
    private String sourceString(GridDataHistory histories[]) {
        String ts = "";

        if (histories.length == 0) {
            // empty string if no histories
            return ts;
        }
        GridDataHistory history = histories[0];

        // if modified, then display
        switch (history.getOrigin()) {
        case INITIALIZED:
            ts = GFEPreference.getString("HistoryOriginText_Populated", "P");
            ts = GFEPreference.getString(
                    "HistoryModelText_"
                            + history.getOriginParm().getDbId().getModelName(),
                    ts);
            break;

        case TIME_INTERPOLATED:
            ts = GFEPreference.getString("HistoryOriginText_Interpolated", "I");
            break;

        case SCRATCH:
            ts = GFEPreference.getString("HistoryOriginText_Scratch", "S");
            break;

        case CALCULATED:
            ts = GFEPreference.getString("HistoryOriginText_Calculated", "C");
            break;

        case OTHER:
            ts = GFEPreference.getString("HistoryOriginText_Other", "?");
            break;

        default:
            ts = "?";
        }

        if (history.getTimeModified() != null) {
            // modified by you or someone else
            if (history.getWhoModified()
                    .equals(gridManager.getDataManager().getWsId())) {
                ts = GFEPreference.getString("HistoryUserModText_Me", "m");
            } else if (!history.getWhoModified()
                    .equals(gridManager.getDataManager().getWsId())) {
                ts = GFEPreference.getString("HistoryUserModText_Other", "o");
            }
        }

        return ts;
    }

    private Date[] paintSaveTimeBlock(GC gc, TimeRange dataTR, GridID gridID,
            GridMode mode) {
        GridDataHistory history[] = gridID.grid().getHistory();

        // determine color to use, based on the history number.
        Date[] time = new Date[] { BASE_DATE, BASE_DATE };
        switch (mode) {
        case LASTSAVED:
            time = getUpdateTime(history);
            break;

        case LASTMODIFIED:
            time = getTimeModified(history);
            break;

        case LASTPUBLISHED:
            time = getPublishTime(history);
            break;

        case LASTSENT:
            time = getLastSentTime(history);
            break;

        default:
        }

        Color color = prefs.getTimeBasedColor(mode, time[1]);

        Rectangle rect = computeRect(dataTR);
        gc.setBackground(color);
        gc.fillRectangle(rect);

        return time;
    }

    /**
     * Calculates the udpate time.
     *
     * @param his
     *            The gird data's history
     * @return the times [0] = early, [1] = late
     */
    private Date[] getUpdateTime(GridDataHistory his[]) {
        Date earlyTime, lateTime;
        earlyTime = lateTime = BASE_DATE;
        for (int i = 0; i < his.length; i++) {
            if (his[i].getUpdateTime() != null) {
                if (his[i].getUpdateTime().after(lateTime)) {
                    lateTime = his[i].getUpdateTime();
                }
                if (earlyTime.equals(BASE_DATE)
                        || his[i].getUpdateTime().before(earlyTime)) {
                    earlyTime = his[i].getUpdateTime();
                }
            }
        }
        return new Date[] { earlyTime, lateTime };
    }

    /**
     * Calculates the modified time.
     *
     * @param his
     *            The gird data's history
     * @param earlyTime
     *            The early time
     * @param lateTime
     *            The late time
     */
    private Date[] getTimeModified(GridDataHistory his[]) {
        Date earlyTime, lateTime;
        earlyTime = lateTime = BASE_DATE;
        for (int i = 0; i < his.length; i++) {
            if (his[i].getTimeModified() != null) {
                if (his[i].getTimeModified().after(lateTime)) {
                    lateTime = his[i].getTimeModified();
                }
                if (earlyTime.equals(BASE_DATE)
                        || his[i].getTimeModified().before(earlyTime)) {
                    earlyTime = his[i].getTimeModified();
                }
            }
        }

        return new Date[] { earlyTime, lateTime };
    }

    /**
     * Calculates the published time.
     *
     * @param his
     *            The gird data's history
     * @param earlyTime
     *            The early time
     * @param lateTime
     *            The late time
     */
    private Date[] getPublishTime(GridDataHistory his[]) {
        Date earlyTime, lateTime;
        earlyTime = lateTime = BASE_DATE;
        for (int i = 0; i < his.length; i++) {
            if (his[i].getPublishTime() != null) {
                if (his[i].getPublishTime().after(lateTime)) {
                    lateTime = his[i].getPublishTime();
                }
                if (earlyTime.equals(BASE_DATE)
                        || his[i].getPublishTime().before(earlyTime)) {
                    earlyTime = his[i].getPublishTime();
                }
            }
        }

        return new Date[] { earlyTime, lateTime };
    }

    /**
     * Calculates the last sent time.
     *
     * @param his
     *            The gird data's history
     * @param earlyTime
     *            The early time
     * @param lateTime
     *            The late time
     */
    private Date[] getLastSentTime(GridDataHistory his[]) {
        Date earlyTime, lateTime;
        earlyTime = lateTime = BASE_DATE;
        for (int i = 0; i < his.length; i++) {
            if (his[i].getLastSentTime() != null) {
                if (his[i].getLastSentTime().after(lateTime)) {
                    lateTime = his[i].getLastSentTime();
                }
                if (earlyTime.equals(BASE_DATE)
                        || his[i].getLastSentTime().before(earlyTime)) {
                    earlyTime = his[i].getLastSentTime();
                }
            }
        }

        return new Date[] { earlyTime, lateTime };
    }

    /**
     * Formats the updated time
     *
     * @param updateTime
     *            The updated time.
     * @return The formatted string.
     */
    private String formatUpdateTime(Date updateTime) {
        if (updateTime.equals(BASE_DATE)) {
            return "";
        }

        long ago = (SimulatedTime.getSystemTime().getTime().getTime()
                - updateTime.getTime()) / TimeUtil.MILLIS_PER_SECOND;

        if (ago < 0) {
            return "0";
        }

        String o = "";
        if (ago < TimeUtil.SECONDS_PER_HOUR) {
            o = o.concat(String.valueOf(ago / TimeUtil.SECONDS_PER_MINUTE));
            o = o.concat("m");
        } else if (ago < (TimeUtil.SECONDS_PER_HOUR * 6)) {
            int hrs = (int) ago / TimeUtil.SECONDS_PER_HOUR;
            int min = (int) (ago % TimeUtil.SECONDS_PER_HOUR)
                    / TimeUtil.SECONDS_PER_MINUTE;
            o = o.concat(String.valueOf(hrs));
            o = o.concat("h");
            o = o.concat(String.valueOf(min));
            o = o.concat("m");
        } else if (ago < TimeUtil.SECONDS_PER_DAY) {
            o = o.concat(String.valueOf((int) ago / TimeUtil.SECONDS_PER_HOUR));
            o = o.concat("h");
        } else {
            o = o.concat(">1d");
        }
        return o;
    }

    /**
     * Formats the date with the proper text.
     *
     * @param earlyTime
     *            The early Time.
     * @param lateTime
     *            The late Time.
     * @return The formatted string.
     */
    String formatUpdateTime(Date earlyTime, Date lateTime) {
        if (earlyTime.equals(lateTime)) {
            return formatUpdateTime(earlyTime);
        }

        String s = formatUpdateTime(lateTime) + "->"
                + formatUpdateTime(earlyTime);

        return s;
    }

    /**
     * Paints the stretch blocks
     *
     * @param event
     * @param dataTR
     * @param isStretch
     */
    public void paintStretchBlocks(PaintEvent event, TimeRange dataTR,
            boolean isStretch) {
        GC gc = event.gc;
        int stretched = -2;
        if (isStretch) {
            gc.setForeground(
                    Display.getCurrent().getSystemColor(SWT.COLOR_RED));

        } else {
            gc.setForeground(
                    Display.getCurrent().getSystemColor(SWT.COLOR_GREEN));
            stretched = -1;
        }

        Rectangle rect = computeRect(dataTR);

        int x1 = rect.x + stretched;
        int x2 = (rect.x + rect.width) - stretched;
        int y1 = rect.y + stretched;
        int y2 = rect.y + rect.height;
        gc.drawPolyline(new int[] { x1, y2, x1, y1, x2, y1, x2, y2 });
    }

    @Override
    public String toString() {
        return parm.getParmID().compositeNameUI();
    }

    /**
     * Truncates the label so it fits within the box.
     *
     * @param label
     *            The text to be truncated.
     * @param maxLengthPixels
     *            The max length that the text can be.
     * @return The truncated string if necessary.
     */
    private String truncateLabelToFit(GC gc, String label,
            int maxLengthPixels) {
        if (maxLengthPixels == 0) {
            return "";
        }

        String string = label;
        while ((gc.stringExtent(string).x > maxLengthPixels)
                && (string.length() > 1)) {
            // remove last character
            string = string.substring(0, string.length() - 1);
        }

        return string;

    }

    /**
     * Compute data block rectangle from timeRange
     *
     * @param tr
     *            desired TimeRange
     * @return rectangle to be drawn
     */
    private Rectangle computeRect(TimeRange tr) {
        Rectangle rect = gridManager.getUtil().timeRangeToPixels(tr);

        rect.x += DATA_BLOCK_HORIZONTAL_MARGIN;
        rect.y = (getBounds().y + getBounds().height) - DATA_BLOCK_HEIGHT - 1;
        rect.width -= (DATA_BLOCK_HORIZONTAL_MARGIN * 2) - 1;
        if (rect.width < 1) {
            rect.width = 1;
        }
        rect.height = DATA_BLOCK_HEIGHT + 1;
        rect.intersect(getBounds());
        return rect;
    }

    /**
     * @param tr
     * @return
     */
    private Rectangle computeLockRect(TimeRange tr) {
        Rectangle rect = gridManager.getUtil().timeRangeToPixels(tr)
                .intersection(getBounds());
        return rect;
    }

    /**
     * @return the bounds of this GridBar
     */
    public Rectangle getBounds() {
        return bounds;
    }

    private void computeBounds() {
        Rectangle rect = canvas.getBounds();
        int height = HEIGHT + descrHeight;
        bounds = new Rectangle(rect.x, verticalPosition, rect.width, height);
        selectionBox = new Rectangle(bounds.x + MARGIN, bounds.y + MARGIN,
                descrHeight, descrHeight);

    }

    /**
     * Causes this GridBar to recompute its bounds
     */
    public void resize() {
        computeBounds();
    }

    protected boolean inSelectionBox(int x, int y) {
        return (selectionBox != null) && selectionBox.contains(x, y);
    }

    /*
     * Highlight timeRange
     */
    private static class Highlight {
        private TimeRange timeRange;

        private String color;

        public Highlight(final TimeRange tr, final String color) {
            timeRange = tr;
            this.color = color;
        }

        public TimeRange getTimeRange() {
            return timeRange;
        }

        public String getColor() {
            return color;
        }
    }

    /**
     * Set the associated parm's selected state
     *
     * @param selected
     */
    public void setSelection(boolean selected) {
        parm.getParmState().setSelected(selected);
    }

    /**
     * Toggle the associated parm's selected state
     */
    public void toggleSelected() {
        ParmState ps = parm.getParmState();
        ps.setSelected(!ps.isSelected());
    }

    /**
     * Mark this GridBar as needing redrawn
     */
    public void redraw() {
        canvas.markDirty(getBounds());
    }

    /**
     * Highlights the weather element blocks according to given time range.
     *
     * @param msg
     */
    private void highlightMsg(final HighlightMsg msg) {
        if (!msg.getParm().equals(parm)) {
            // nothing to do -- not this parm
            return;
        }

        // extract info from message
        TimeRange[] trs = msg.getTimeRanges();
        boolean onFlag = msg.isOn();
        String color = msg.getColor();

        boolean changed = false;
        for (TimeRange tr : trs) {
            changed = changed || mergeHighlights(tr, onFlag, color);
        }

        if (changed) {
            redraw();
        }
        return;
    }

    /**
     * Clears the specified highlights.
     *
     * @param msg
     */
    private void clearHighlightMsg(final ClearHighlightsMsg msg) {
        // extract info from message
        boolean selectedOnly = msg.selectedOnly();

        // all highlights
        if (!selectedOnly) {
            if (!highlights.isEmpty()) {
                highlights.clear();
                disposeHighlightColors();
                redraw();
            }
            return;
        }

        // selected highlights
        if (parm.getParmState().isSelected()) {
            TimeRange selTR = parm.getParmState().getSelectedTimeRange();
            boolean changed = mergeHighlights(selTR, false, "");
            if (changed) {
                redraw();
            }
        }
    }

    /**
     *
     */
    private void disposeHighlightColors() {
        for (Color color : highlightColors.values()) {
            color.dispose();
        }
        highlightColors.clear();
    }

    /**
     * Merges new highlights with old highlights.
     *
     * @param tr
     * @param onFlag
     * @param highlight
     * @return true if any changes made
     */
    private boolean mergeHighlights(final TimeRange tr, boolean onFlag,
            final String highlight) {
        if (onFlag) {
            highlights.add(new Highlight(tr, highlight));
            Color color = highlightColors.get(highlight);
            if (color == null) {
                highlightColors.put(highlight, new Color(Display.getDefault(),
                        RGBColors.getRGBColor(highlight)));
            }
            return true;
        }
        // find any overlapping
        else {
            boolean changed = false;
            for (int i = highlights.size() - 1; i >= 0; i--) {
                if ((highlights.get(i).getColor().equals(highlight)
                        || (highlight.length() == 0))
                        && highlights.get(i).getTimeRange().overlaps(tr)) {
                    highlights.remove(i);
                    changed = true;
                    if (highlights.isEmpty()) {
                        disposeHighlightColors();
                    }
                }
            }
            return changed;
        }
    }

    /**
     * Returns the highlight color
     *
     * @param tr
     * @return the highlight color or "" if not highlighted.
     */
    public String highlightColor(final TimeRange tr) {
        // search list backwards for last entry that matches the time range.
        for (int i = highlights.size() - 1; i >= 0; i--) {
            if (highlights.get(i).getTimeRange().overlaps(tr)) {
                return highlights.get(i).getColor();
            }
        }

        // no highlight
        return "";
    }

    @Override
    public void parmInventoryChanged(Parm parm, TimeRange timeRange) {
        if (this.parm.equals(parm)
                && this.gridManager.checkVisibility(timeRange)) {
            redraw();
        }
    }

    @Override
    public void gridHistoryUpdated(Parm parm, TimeRange timeRange) {
        if (this.parm.equals(parm)
                && this.gridManager.checkVisibility(timeRange)) {
            redraw();
        }
    }

    @Override
    public void gridVisibilityChanged(Parm aParm, boolean visible,
            boolean makeOnlyVisible) {

        // if want only one visible and this isn't the parm, make us
        // invisible
        boolean thisParm = parm.equals(aParm);
        boolean desiredVisiblity = parmVisible;
        if (thisParm) {
            // if we are this parm, use the visible flag
            desiredVisiblity = visible;
        } else if (makeOnlyVisible) {
            desiredVisiblity = false;
        }

        if (desiredVisiblity != parmVisible) {
            parmVisible = desiredVisiblity;
            redraw();
        }
    }

    @Override
    public void activatedParmChanged(Parm newParm) {
        boolean thisParm = parm.equals(newParm);
        if (!thisParm && !parmActive) {
            // nothing to do for this parm (no need to paint)
            return;
        }

        // set _parmActive appropriately
        parmActive = (thisParm ? true : false);

        redraw();
    }

    @Override
    public void gridDataChanged(ParmID parmId, TimeRange validTime) {
        if (this.parm.getParmID().equals(parmId)
                && this.gridManager.checkVisibility(validTime)) {
            redraw();
        }
    }

    @Override
    public void selectionTimeRangeChanged(Parm parm,
            TimeRange selectionTimeRange) {
        if (parm.getParmState().isSelected()) {
            redraw();
        }
    }

    @Override
    public void parameterSelectionChanged(Parm parm, boolean selected) {
        redraw();
    }

    @Override
    public void lockTableChanged(Parm parm, LockTable lockTable) {
        if (this.parm.equals(parm)) {
            // Update the GridManagerView dirty status
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    if (PlatformUI.isWorkbenchRunning()) {
                        IWorkbenchWindow window = PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow();
                        if (window != null) {
                            IWorkbenchPage page = window.getActivePage();
                            if (page != null) {
                                IViewReference viewReference = page
                                        .findViewReference(GridManagerView.ID,
                                                "GridManager");
                                if (viewReference != null) {
                                    ((GridManagerView) viewReference
                                            .getPart(false))
                                                    .updateDirtyStatus();
                                }
                            }
                        }
                    }
                }
            });
            redraw();
        }
    }

    @Override
    public void parmIDChanged(Parm parm, ParmID newParmID) {
        redraw();
    }
}
