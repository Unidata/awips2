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
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.progress.UIJob;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.TimeConstraints;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEOperationFailedException;
import com.raytheon.viz.gfe.GFEServerException;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.msgs.FindParameterMsg;
import com.raytheon.viz.gfe.core.msgs.IActivatedParmChangedListener;
import com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener;
import com.raytheon.viz.gfe.core.msgs.Message;
import com.raytheon.viz.gfe.core.msgs.Message.IMessageClient;
import com.raytheon.viz.gfe.core.msgs.QuickViewModeChangedMsg;
import com.raytheon.viz.gfe.core.msgs.SetImageOnActiveChangedMsg;
import com.raytheon.viz.gfe.core.msgs.ShowQuickViewDataMsg;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.gfe.gridmanager.action.AssignAction;
import com.raytheon.viz.gfe.gridmanager.action.ClearHighlightAction;
import com.raytheon.viz.gfe.gridmanager.action.CopyAction;
import com.raytheon.viz.gfe.gridmanager.action.CreateFromScratchAction;
import com.raytheon.viz.gfe.gridmanager.action.DeleteAction;
import com.raytheon.viz.gfe.gridmanager.action.DeselectAllAction;
import com.raytheon.viz.gfe.gridmanager.action.DisplayInfoAction;
import com.raytheon.viz.gfe.gridmanager.action.FragmentAction;
import com.raytheon.viz.gfe.gridmanager.action.PasteAction;
import com.raytheon.viz.gfe.gridmanager.action.SelectAllTimesAction;
import com.raytheon.viz.gfe.gridmanager.action.ShowISCArea;
import com.raytheon.viz.gfe.gridmanager.action.ShowISCGrid;
import com.raytheon.viz.gfe.gridmanager.action.ShowISCHighlights;
import com.raytheon.viz.gfe.gridmanager.action.ShowISCInfo;
import com.raytheon.viz.gfe.gridmanager.action.SplitAction;
import com.raytheon.viz.gfe.gridmanager.action.UndoAction;
import com.raytheon.viz.gfe.smarttool.SmartUtil;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;

/**
 * GridManager widget containing a GridBar for each displayed parm
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2009            randerso     Initial creation
 * Jun 3, 2011  8919      rferrel      Determine grid's VisMode based
 *                                     on imageOnEdit
 * 08/20/2012    #1082     randerso    Moved calcStepTimes to AbstractParmManager for
 *                                     use in PngWriter
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class GridCanvas extends Canvas implements IMessageClient {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridCanvas.class);

    public static final int GRIDBAR_SPACING = 3;

    public static final int SEPARATOR_HEIGHT = 10;

    private static Color SEPARATOR_COLOR = new Color(Display.getDefault(),
            RGBColors.getRGBColor("CornflowerBlue"));

    private class ScrollJob extends UIJob {
        private int increment = 0;

        /**
         * @param increment
         *            the increment to set
         */
        public void setIncrement(int increment) {
            this.increment = increment;
        }

        public ScrollJob() {
            super("GridCanvasScrollJob");
            setSystem(true);
        }

        @Override
        public IStatus runInUIThread(IProgressMonitor monitor) {
            Point p = scrolledComp.getOrigin();
            if ((increment < 0 && p.y > 0)
                    || (increment > 0 && p.y < (getSize().y - scrolledComp
                            .getClientArea().height))) {
                p.y += increment;
                scrolledComp.setOrigin(p);

                if (increment < 0) {
                    selection.y += increment;
                }
                selection.height += Math.abs(increment);
                setSelection(selection);

                schedule(100);
            } else {
                cancel();
            }
            return Status.OK_STATUS;
        }
    }

    private class RepaintJob extends UIJob {
        private Rectangle dirtyRect;

        public RepaintJob() {
            super("GridCanvasRepaintJob");
            setSystem(true);
        }

        public void markDirty(Rectangle rect) {
            synchronized (this) {
                if (this.dirtyRect != null) {
                    this.dirtyRect = this.dirtyRect.union(rect);
                } else {
                    this.dirtyRect = rect;
                }
            }
            this.schedule();
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * org.eclipse.ui.progress.UIJob#runInUIThread(org.eclipse.core.runtime
         * .IProgressMonitor)
         */
        @Override
        public IStatus runInUIThread(IProgressMonitor monitor) {

            if (!GridCanvas.this.isDisposed() && dirtyRect != null) {
                Rectangle rect;
                synchronized (this) {
                    rect = dirtyRect;
                    dirtyRect = null;
                }

                GridCanvas.this.redraw(rect.x, rect.y, rect.width, rect.height,
                        true);
            }

            return Status.OK_STATUS;
        }

    }

    private class QuickviewMouseListener extends MouseTrackAdapter implements
            MouseMoveListener {

        @Override
        public void mouseMove(MouseEvent e) {
            GridBar gridBar = findClickedBar(e.x, e.y);
            if (gridBar != null) {
                GridManager gm = gridManager;
                Date clickTime = gm.getUtil().pixelToDate(e.x);
                IGridData gridData = gridBar.getParm().overlappingGrid(
                        clickTime);

                showQuickViewGrid(gridData);
            }
        }

        @Override
        public void mouseExit(MouseEvent e) {
            if (quickviewMode && (quickviewGrid != null)) {
                showQuickViewGrid(null);
            }
        }

    }

    private ScrollJob scrollJob = new ScrollJob();

    private RepaintJob repaintJob = new RepaintJob();

    private ScrolledComposite scrolledComp;

    private Point stretchStart = new Point(0, 0);

    private boolean stretchingGrid = false;

    private GridID stretchGridID;

    private GridBar stretchGridBar;

    private TimeRange sourceTR;

    private TimeRange lastDestinationTR;

    private ArrayList<GridBar> gridBarList;

    private Map<Parm, GridBar> parmToGridBar;

    private Rectangle selection;

    private MenuManager menuMgr;

    private GridManager gridManager;

    private DataManager dataMgr;

    private IDisplayedParmListChangedListener displayedParmListChangedListener;

    private final IActivatedParmChangedListener activatedParmChangedListener;

    private int separatorPosition = 0;

    private boolean quickviewMode;

    private GridID quickviewGrid;

    private QuickviewMouseListener quickviewMouseListener;

    @SuppressWarnings("unchecked")
    public GridCanvas(final ScrolledComposite aParent,
            final GridManager aGridManager) {
        super(aParent, SWT.NONE);
        scrolledComp = aParent;
        scrolledComp.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                resize();
            }
        });

        gridManager = aGridManager;
        dataMgr = gridManager.getDataManager();

        gridBarList = new ArrayList<GridBar>();
        parmToGridBar = new HashMap<Parm, GridBar>();

        Parm[] displayedParms = gridManager.getDataManager().getParmManager()
                .getDisplayedParms();

        updateParmList(null, displayedParms);

        displayedParmListChangedListener = new IDisplayedParmListChangedListener() {

            @Override
            public void displayedParmListChanged(Parm[] parms,
                    final Parm[] deletions, final Parm[] additions) {
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        updateParmList(deletions, additions);
                        if (!isDisposed() && !scrolledComp.isDisposed()) {
                            Rectangle r = scrolledComp.getClientArea();
                            scrolledComp.setMinSize(computeSize(r.width,
                                    SWT.DEFAULT));
                            redraw();
                        }
                    }
                });
            }

        };

        gridManager
                .getDataManager()
                .getParmManager()
                .addDisplayedParmListChangedListener(
                        displayedParmListChangedListener);

        Message.registerInterest(this, FindParameterMsg.class,
                QuickViewModeChangedMsg.class);

        addPaintListener(new PaintListener() {
            @Override
            public void paintControl(PaintEvent e) {
                paint(e);
            }
        });

        MouseHandler mouseHandler = new MouseHandler() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * com.raytheon.viz.gfe.gridmanager.MouseHandler#dragEnd(org.eclipse
             * .swt.events.MouseEvent)
             */
            @Override
            public void dragEnd(MouseEvent e) {
                super.dragEnd(e);

                if (e.button == 1) {
                    scrollJob.setIncrement(0);
                }

                if (e.button == 2) {
                    if (stretchingGrid) {
                        inStretch(e.x);
                        endStretch();
                    }
                }
            }

            /*
             * (non-Javadoc)
             * 
             * @see
             * com.raytheon.viz.gfe.gridmanager.MouseHandler#dragMove(org.eclipse
             * .swt.events.MouseEvent)
             */
            @Override
            public void dragMove(MouseEvent e) {
                super.dragMove(e);

                if (isButtonDown(1)) {
                    // scroll GridCanvas vertically if necessary
                    Point p = scrolledComp.getOrigin();
                    Rectangle r = scrolledComp.getClientArea();

                    if (e.y < p.y) {
                        scrollJob.setIncrement(-30);
                        scrollJob.schedule();
                    } else if (e.y > p.y + r.height) {
                        scrollJob.setIncrement(30);
                        scrollJob.schedule();
                    } else {
                        scrollJob.setIncrement(0);
                    }

                    Point selectionStart = getDragAnchor();
                    setSelection(new Rectangle(Math.min(selectionStart.x, e.x),
                            Math.min(selectionStart.y, e.y), Math.abs(e.x
                                    - selectionStart.x), Math.abs(e.y
                                    - selectionStart.y)));
                }

                if (isButtonDown(2)) {
                    if (stretchingGrid) {
                        inStretch(e.x);
                    }
                }
            }

            /*
             * (non-Javadoc)
             * 
             * @see
             * com.raytheon.viz.gfe.gridmanager.MouseHandler#dragStart(org.eclipse
             * .swt.events.MouseEvent, org.eclipse.swt.graphics.Point)
             */
            @Override
            public void dragStart(MouseEvent e) {
                super.dragStart(e);

                if (isButtonDown(2)) {
                    startStretch(getDragAnchor());

                    if (stretchingGrid) {
                        inStretch(e.x);
                    }
                }
            }

            /*
             * (non-Javadoc)
             * 
             * @see
             * com.raytheon.viz.gfe.gridmanager.MouseHandler#mouseClick(org.
             * eclipse.swt.events.MouseEvent)
             */
            @Override
            public void mouseClick(MouseEvent e) {
                super.mouseClick(e);

                if (e.stateMask == SWT.BUTTON1 || e.stateMask == SWT.BUTTON2) {
                    processClickEvent(e);
                }
            }

            /*
             * (non-Javadoc)
             * 
             * @see
             * com.raytheon.viz.gfe.gridmanager.MouseHandler#mouseDown(org.eclipse
             * .swt.events.MouseEvent)
             */
            @Override
            public void mouseDown(MouseEvent e) {
                super.mouseDown(e);

                GridBar gridBar = findClickedBar(e.x, e.y);
                if (e.button == 1 && (e.stateMask & SWT.MODIFIER_MASK) == 0
                        && gridBar != null) {
                    if (!gridBar.inSelectionBox(e.x, e.y)) {
                        selectOnly(gridBar);
                        Date clickTime = gridManager.getUtil().pixelToDate(e.x);
                        TimeRange timeRange = gridManager.getUtil().dateToHour(
                                clickTime);
                        dataMgr.getParmOp().setSelectionTimeRange(timeRange);
                    }
                }
            }

            /*
             * (non-Javadoc)
             * 
             * @see
             * com.raytheon.viz.gfe.gridmanager.MouseHandler#displayContextMenu
             * ()
             */
            @Override
            public void displayContextMenu(MouseEvent e)
                    throws GFEServerException {
                super.displayContextMenu(e);

                GridBar gridBar = findClickedBar(e.x, e.y);
                if (gridBar != null) {
                    GridManager gm = gridManager;
                    Date clickTime = gm.getUtil().pixelToDate(e.x);
                    TimeRange timeRange = new TimeRange(clickTime,
                            GridManagerUtil.MILLIS_PER_SECOND);

                    Parm parm = gridBar.getParm();
                    IGridData overGrid = parm.overlappingGrid(clickTime);

                    boolean isEmpty = overGrid == null;
                    boolean pasteOk = gm.getDataManager().getParmOp()
                            .okToPasteGrid(parm, clickTime);
                    boolean okEdit = parm.isOkToEdit(timeRange);
                    boolean validConstraint = parm.getGridInfo()
                            .getTimeConstraints().constraintTime(clickTime)
                            .isValid();

                    if (menuMgr != null) {
                        menuMgr.dispose();
                    }
                    menuMgr = new MenuManager("#PopupMenu");

                    if (!isEmpty) {
                        menuMgr.add(new CopyAction(parm, clickTime));
                    }
                    if (okEdit && pasteOk) {
                        menuMgr.add(new PasteAction(parm, clickTime));
                    }
                    if (!isEmpty && okEdit) {
                        if (!overGrid.getGridTime().equals(
                                parm.getGridInfo()
                                        .getTimeConstraints()
                                        .constraintTime(
                                                overGrid.getGridTime()
                                                        .getStart()))) {

                            menuMgr.add(new FragmentAction(parm, clickTime));
                            menuMgr.add(new SplitAction(parm, clickTime));
                        }

                        WxValue defaultValue = WxValue.defaultValue(parm);
                        if (defaultValue != null
                                && !defaultValue.equals(parm.getParmState()
                                        .getPickUpValue())) {
                            menuMgr.add(new AssignAction(parm, overGrid
                                    .getGridTime(), parm.getParmState()
                                    .getPickUpValue()));
                        }

                        menuMgr.add(new AssignAction(parm, overGrid
                                .getGridTime(), WxValue.defaultValue(parm)));
                    }

                    if (okEdit && !isEmpty) {

                        menuMgr.add(new Separator());
                        menuMgr.add(new DeleteAction(parm, clickTime));
                        menuMgr.add(new Separator());
                    }

                    if (isEmpty && okEdit && validConstraint) {

                        menuMgr.add(new CreateFromScratchAction(parm, clickTime));
                    }
                    menuMgr.add(new UndoAction());
                    menuMgr.add(new SelectAllTimesAction(parm));
                    menuMgr.add(new DisplayInfoAction(parm, clickTime));
                    menuMgr.add(new DeselectAllAction());

                    String hlColor = gridBar.highlightColor(timeRange);
                    if (!hlColor.isEmpty() && !isEmpty) {
                        menuMgr.add(new ClearHighlightAction(
                                overGrid.getParm(), overGrid.getGridTime(),
                                hlColor));
                    }

                    // Add in any smart tools
                    if (parm.getParmID()
                            .getDbId()
                            .equals(parm.getDataManager().getParmManager()
                                    .getMutableDatabase())) {
                        String[] gmEditActions = Activator.getDefault()
                                .getPreferenceStore()
                                .getStringArray("GridManagerEditActions");
                        List<String> popUpActions = new ArrayList<String>(0);
                        if (gmEditActions.length > 0) {
                            // Only show tools this parm supports
                            String[] parmTools = DataManager
                                    .getCurrentInstance()
                                    .getSmartToolInterface().listTools(parm);
                            List<String> parmToolList = Arrays
                                    .asList(parmTools);
                            popUpActions = new ArrayList<String>(
                                    Arrays.asList(gmEditActions));
                            popUpActions.retainAll(parmToolList);
                        }

                        if ((popUpActions.size() > 0)
                                || (parm.getGridInfo().getGridType()
                                        .equals(GridType.SCALAR))
                                || (parm.getGridInfo().getGridType()
                                        .equals(GridType.VECTOR))) {
                            menuMgr.add(new Separator());
                        }

                        if ((parm.getGridInfo().getGridType()
                                .equals(GridType.SCALAR))
                                || (parm.getGridInfo().getGridType()
                                        .equals(GridType.VECTOR))) {
                            menuMgr.add(new ShowISCArea());
                            menuMgr.add(new ShowISCGrid());
                            menuMgr.add(new ShowISCHighlights());
                            menuMgr.add(new ShowISCInfo());
                        }

                        for (final String tool : popUpActions) {
                            AbstractRightClickAction action = new AbstractRightClickAction() {

                                @Override
                                public String getText() {
                                    return tool;
                                }

                                @Override
                                public void run() {
                                    SmartUtil.runTool(tool);
                                }
                            };
                            menuMgr.add(action);
                        }
                    }
                }

                Menu menu = menuMgr.createContextMenu(GridCanvas.this);
                menu.setVisible(true);
                setMenu(menu);
            }
        };

        addMouseListener(mouseHandler);

        quickviewMouseListener = new QuickviewMouseListener();

        receiveMessage(Message
                .inquireLastMessage(QuickViewModeChangedMsg.class));

        activatedParmChangedListener = new IActivatedParmChangedListener() {
            @Override
            public void activatedParmChanged(final Parm newParm) {
                VizApp.runAsync(new Runnable() {

                    @Override
                    public void run() {
                        makeVisible(newParm);
                    }

                });
            }
        };

        aGridManager.getDataManager().getSpatialDisplayManager()
                .addActivatedParmChangedListener(activatedParmChangedListener);

        addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                gridManager
                        .getDataManager()
                        .getParmManager()
                        .removeDisplayedParmListChangedListener(
                                displayedParmListChangedListener);

                gridManager
                        .getDataManager()
                        .getSpatialDisplayManager()
                        .removeActivatedParmChangedListener(
                                activatedParmChangedListener);

                Message.unregisterInterest(GridCanvas.this,
                        FindParameterMsg.class, QuickViewModeChangedMsg.class);
            }
        });
    }

    protected void showQuickViewGrid(IGridData grid) {
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

    protected void updateParmList(Parm[] deletions, Parm[] additions) {

        if (deletions != null) {
            for (Parm parm : deletions) {
                GridBar gridBar = parmToGridBar.remove(parm);
                if (gridBar != null) {
                    gridBarList.remove(gridBar);
                    gridBar.dispose();
                }
            }
        }

        if (additions != null) {
            for (Parm parm : additions) {
                if (!parm.getGridInfo().isTimeIndependentParm()) {
                    if (!parmToGridBar.containsKey(parm)) {
                        GridBar gridBar = new GridBar(this, parm, gridManager);
                        gridBarList.add(gridBar);
                        parmToGridBar.put(parm, gridBar);
                    }
                }
            }
        }

        Collections.sort(gridBarList, new Comparator<GridBar>() {

            @Override
            public int compare(GridBar o1, GridBar o2) {
                return o1.getParm().compareTo(o2.getParm());
            }

        });

        int vPos = 0;
        separatorPosition = -1;
        for (GridBar gridBar : gridBarList) {
            if (separatorPosition == -1 && !gridBar.getParm().isMutable()) {
                separatorPosition = vPos;
                vPos += SEPARATOR_HEIGHT + GRIDBAR_SPACING;
            }
            gridBar.setVerticalPosition(vPos);
            vPos += gridBar.getBounds().height + GRIDBAR_SPACING;
        }

        if (separatorPosition == -1) {
            Rectangle bounds;
            if (gridBarList.size() > 0) {
                bounds = gridBarList.get(gridBarList.size() - 1).getBounds();
                separatorPosition = bounds.y + bounds.height + GRIDBAR_SPACING;
            } else {
                separatorPosition = 0;
            }
        }

        resize();
    }

    protected void startStretch(Point startPoint) {
        stretchStart.x = startPoint.x;
        stretchStart.y = startPoint.y;

        stretchGridBar = findClickedBar(stretchStart.x, stretchStart.y);
        if (stretchGridBar == null) {
            return;
        }

        Date clickTime = gridManager.getUtil().pixelToDate(stretchStart.x);
        Parm parm = stretchGridBar.getParm();
        IGridData gridData = parm.overlappingGrid(clickTime);
        if (gridData == null) {
            return;
        }

        // See if it's okToEdit
        if (!parm.isOkToEdit(gridData.getGridTime())) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Editing not allowed on grids locked by others.");
            return;
        }
        // Set up the data that lives between events
        stretchingGrid = true;
        stretchGridID = new GridID(parm, clickTime);
        sourceTR = gridData.getGridTime();
        lastDestinationTR = null;
    }

    protected void inStretch(int x) {
        Date clickTime = gridManager.getUtil().pixelToDate(x);

        // Get the shadow block
        TimeRange shadowTR = stretchGridID.getParm().getGridInfo()
                .getTimeConstraints().constraintTime(clickTime);
        TimeRange newTR = stretchGridID.grid().getGridTime()
                .combineWith(shadowTR);

        if (!newTR.equals(lastDestinationTR)
                && stretchGridID.getParm().isOkToEdit(newTR)) {
            lastDestinationTR = newTR;
            stretchGridBar.redraw();
        }
    }

    protected void endStretch() {
        ArrayList<IGridData> newGrids = new ArrayList<IGridData>();
        // Make changes only when the new TimeRange is
        // different from the original and the TimeRange is
        // valid
        if (!stretchGridID.grid().getGridTime().equals(lastDestinationTR)
                && lastDestinationTR.isValid()) {

            // Checking for gaps in the new TimeRange
            TimeConstraints sb = stretchGridID.getParm().getGridInfo()
                    .getTimeConstraints();

            if (sb.getDuration() != sb.getRepeatInterval()) {
                TimeRange newTimes[] = sb.constraintTimes(lastDestinationTR);
                for (TimeRange timeRange : newTimes) {
                    IGridData tmpGrid;
                    try {
                        tmpGrid = stretchGridID.grid().clone();
                    } catch (CloneNotSupportedException e) {
                        tmpGrid = null;
                    }

                    tmpGrid.changeValidTime(timeRange, false);
                    newGrids.add(tmpGrid);
                }
            } else {

                IGridData grid;
                try {
                    grid = stretchGridID.grid().clone();
                } catch (CloneNotSupportedException e) {
                    grid = null;
                }
                grid.changeValidTime(lastDestinationTR, false);
                grid.updateHistoryToModified(DataManager.getCurrentInstance()
                        .getWsId());

                newGrids.add(grid);

            }

            // Replace data with the new grids - Note the grids become "owned"
            // by the parm and is no longer our responsibility
            if (!stretchGridID.getParm().replaceGriddedData(lastDestinationTR,
                    newGrids.toArray(new IGridData[newGrids.size()]))) {
                statusHandler.handle(Priority.PROBLEM,
                        "ReplaceGriddedData failed in endStretch.");
            }

        }
        stretchingGrid = false;
        stretchGridBar.redraw();
    }

    protected GridBar findClickedBar(int x, int y) {
        for (GridBar gridBar : gridBarList) {
            if (gridBar.getBounds().contains(x, y)) {
                return gridBar;
            }
        }
        return null;
    }

    protected void paint(PaintEvent event) {

        for (GridBar gridBar : gridBarList) {
            if (event.gc.getClipping().intersects(gridBar.getBounds())) {
                gridBar.paint(event);
            }
        }

        Rectangle separatorRect = getClientArea();
        separatorRect.y = separatorPosition;
        separatorRect.height = SEPARATOR_HEIGHT;

        event.gc.setBackground(SEPARATOR_COLOR);
        event.gc.fillRectangle(separatorRect);

        if (stretchingGrid) {
            stretchGridBar.paintStretchBlocks(event, sourceTR, false);
            stretchGridBar.paintStretchBlocks(event, lastDestinationTR, true);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.swt.widgets.Control#computeSize(int, int)
     */
    @Override
    public Point computeSize(int hint, int hint2) {
        Rectangle rect;
        if (gridBarList.size() > 0) {
            rect = gridBarList.get(gridBarList.size() - 1).getBounds();
            return new Point(hint, rect.y + rect.height + SEPARATOR_HEIGHT
                    + GRIDBAR_SPACING * 2);
        } else {
            return new Point(hint, SEPARATOR_HEIGHT + GRIDBAR_SPACING * 2);
        }
    }

    public int getGridBarSpacing() {
        if (gridBarList.size() == 0) {
            return 0;
        } else {
            return gridBarList.get(0).getBounds().height + GRIDBAR_SPACING;
        }
    }

    protected void setSelection(Rectangle selection) {
        this.selection = selection;
        for (GridBar gridBar : gridBarList) {
            gridBar.setSelection(selection.intersects(gridBar.getBounds()));
        }
    }

    protected void selectOnly(GridBar selectedBar) {
        for (GridBar gridBar : gridBarList) {
            gridBar.setSelection(gridBar == selectedBar);
        }
    }

    /**
     * @param e
     */
    private void processClickEvent(MouseEvent e) {
        GridBar gridBar = findClickedBar(e.x, e.y);
        if (gridBar == null) {
            return;
        }

        // First check if the click occurred in the Parm Selector box
        if (gridBar.inSelectionBox(e.x, e.y)) {
            gridBar.toggleSelected();
            return;
        }

        // Not the selector box, so toggle visibility and set SETime and make
        // active, maybe
        Parm parm = gridBar.getParm();
        Date clickTime = gridManager.getUtil().pixelToDate(e.x);
        GridID clickGridID = new GridID(parm, clickTime);

        gridManager.setSelectedTime(clickTime);

        // make it active, make it inactive depending upon okToEdit
        try {
            if (clickGridID.grid() != null && clickGridID.grid().isOkToEdit()) {
                gridManager.getDataManager().getSpatialDisplayManager()
                        .activateParm(parm);
            } else {
                gridManager.getDataManager().getSpatialDisplayManager()
                        .activateParm(null);

                // special case - simulate image on active, even though
                // we didn't really make the grid active
                boolean imageOnEdit = Message.inquireLastMessage(
                        SetImageOnActiveChangedMsg.class).isEnabled();
                gridManager
                        .getDataManager()
                        .getSpatialDisplayManager()
                        .setDisplayMode(parm,
                                imageOnEdit ? VisMode.IMAGE : VisMode.GRAPHIC);
            }

            // set the selection time range to that of the clicked grid
            IGridData grid = parm.overlappingGrid(clickTime);
            if (grid != null) {
                dataMgr.getParmOp().setSelectionTimeRange(grid.getGridTime());
            } else {
                dataMgr.getParmOp().setSelectionTimeRange(
                        parm.getGridInfo().getTimeConstraints()
                                .constraintTime(clickTime));
            }

            // Set the grid's visibility state to true
            boolean makeOnlyVisible = false;
            if (e.button == 1) {
                makeOnlyVisible = true;
            }
            gridManager.getDataManager().getSpatialDisplayManager()
                    .makeVisible(parm, true, makeOnlyVisible);
        } catch (GFEOperationFailedException e1) {
            statusHandler.handle(Priority.PROBLEM, "Error activating parm "
                    + parm.getParmID().compositeNameUI(), e1);
        }
    }

    private void resize() {
        Rectangle r = scrolledComp.getClientArea();
        scrolledComp.setMinSize(computeSize(r.width, SWT.DEFAULT));

        for (GridBar gridBar : gridBarList) {
            gridBar.resize();
        }

        updateScrollbar();
    }

    private void updateScrollbar() {
        int pageIncrement = getGridBarSpacing();
        if (pageIncrement > 0) {
            pageIncrement = (scrolledComp.getClientArea().height / getGridBarSpacing())
                    * getGridBarSpacing();
        }
        scrolledComp.getVerticalBar().setPageIncrement(pageIncrement);
    }

    /**
     * Scroll the grid manager to make the desired parm visible
     * 
     * @param parm
     */
    public void makeVisible(Parm parm) {
        for (GridBar gridBar : gridBarList) {
            if (gridBar.getParm().equals(parm)) {
                Rectangle rect = gridBar.getBounds();

                Point p = scrolledComp.getOrigin();
                if (rect.y < p.y) {
                    p.y = rect.y;
                    scrolledComp.setOrigin(p);
                } else {
                    Rectangle ca = scrolledComp.getClientArea();
                    if (rect.y + rect.height > p.y + ca.height) {
                        p.y = rect.y + rect.height - ca.height;
                        scrolledComp.setOrigin(p);
                    }
                }
                break;
            }
        }
    }

    @Override
    public void receiveMessage(Message message) {
        if (message instanceof FindParameterMsg) {
            makeVisible(((FindParameterMsg) message).getParm());

        } else if (message instanceof QuickViewModeChangedMsg) {
            boolean enabled = ((QuickViewModeChangedMsg) message).isEnabled();

            if (enabled && !quickviewMode) {
                quickviewMode = true;
                addMouseMoveListener(quickviewMouseListener);
                addMouseTrackListener(quickviewMouseListener);
            } else if (!enabled && quickviewMode) {
                removeMouseMoveListener(quickviewMouseListener);
                removeMouseTrackListener(quickviewMouseListener);
            }

            quickviewMode = enabled;

            if (!quickviewMode && (quickviewGrid != null)) {
                showQuickViewGrid(null);
            }
        }
    }

    public void markDirty(Rectangle rect) {
        this.repaintJob.markDirty(rect);
    }
}
