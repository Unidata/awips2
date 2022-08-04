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

import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.TimeZone;
import java.util.concurrent.ConcurrentSkipListSet;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.progress.UIJob;

import com.raytheon.uf.common.time.ISimulatedTimeChangeListener;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.griddata.IGridData;
import com.raytheon.viz.gfe.core.msgs.ISpatialEditorTimeChangedListener;
import com.raytheon.viz.gfe.core.msgs.ISystemTimeRangeChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.edittool.GridID;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditor;

/**
 * GFE Grid Manager
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 07, 2009           randerso  Redesigned
 * May 18, 2009  2159     rjpeter   Added temporal editor.
 * Jan 14, 2013  1442     rferrel   Add SimulatedTimeChangeListener.
 * Jan 15, 2014  16072    ryu       Modify syncSelectTR() to skip the original
 *                                  code when called back from
 *                                  setSelectedTime().
 * Aug 13, 2015  4749     njensen   Implemented dispose()
 * Mar 10, 2016  5479     randerso  Moved widthIncrement into GridManagerUtil
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 * Feb 28, 2018  7062     randerso  Synchronized TimeScale updates with
 *                                  SimulatedTime
 *
 * </pre>
 *
 * @author randerso
 */

public class GridManager
        implements IGridManager, ISpatialEditorTimeChangedListener {

    private Date selectedTime = null;

    private class RedrawRunnable implements Runnable {
        @Override
        public void run() {
            redraw();
        }
    }

    /**
     * Job to scroll the grid manager horizontally
     */
    private class ScrollJob extends Job {
        private int increment = 0;

        /**
         * @param increment
         *            the increment to set
         */
        public void setIncrement(int increment) {
            this.increment = increment;
        }

        public ScrollJob() {
            super("GridManagerScrollJob");
            this.setSystem(true);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            VizApp.runSync(new Runnable() {

                @Override
                public void run() {
                    GridManagerUtil util = getUtil();
                    int p = timeScroller.getSelection();
                    int inc = (int) (util.pixelsToDuration(increment) / 1000);
                    if (inc != 0) {
                        p += inc;
                        if (p < timeScroller.getMinimum()) {
                            timeScroller.setMinimum(p);
                        }
                        if ((p + timeScroller.getThumb()) > timeScroller
                                .getMaximum()) {
                            timeScroller
                                    .setMaximum(p + timeScroller.getThumb());
                        }
                        timeScroller.setSelection(p);
                        clearVisibleTimeRange();

                        if (selectionActive) {
                            selectionStart -= increment;
                            selectionEnd += increment;
                            TimeRange selection = getUtil().pixelsToSelection(
                                    selectionStart, selectionEnd);
                            dataManager.getParmOp()
                                    .setSelectionTimeRange(selection);
                        }

                        redraw();

                        schedule(100);
                    } else {
                        cancel();
                    }
                }

            });
            return Status.OK_STATUS;
        }
    }

    private ScrollJob scrollJob = new ScrollJob();

    /**
     * Job to update the time display
     */
    private static class UpdateJob extends UIJob
            implements ISimulatedTimeChangeListener {

        public UpdateJob() {
            super("GridManagerUpdate");
            this.setSystem(true);
            SimulatedTime.getSystemTime().addSimulatedTimeChangeListener(this);
        }

        @Override
        public IStatus runInUIThread(IProgressMonitor monitor) {
            // if any displays are active
            if (!activeList.isEmpty()) {
                // update the state of all active displays
                for (GridManager gm : activeList) {
                    gm.redraw();
                }
                long t = 0;
                if (!SimulatedTime.getSystemTime().isFrozen()) {
                    t = SimulatedTime.getSystemTime().getMillis()
                            % TimeUtil.MILLIS_PER_MINUTE;
                }
                this.schedule(TimeUtil.MILLIS_PER_MINUTE - t);
            }

            return Status.OK_STATUS;
        }

        @Override
        public void timechanged() {
            wakeUp();
        }
    }

    /**
     * Job to update the time display
     */
    private class TemporalEditorDeactiveJob extends Job {

        public TemporalEditorDeactiveJob() {
            super("TemporalEditorDeactive");
            this.setSystem(true);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            if ((temporalEditor != null) && temporalEditor.checkDeactive()) {
                Display.getDefault().syncExec(new Runnable() {
                    @Override
                    public void run() {
                        teScrolledComp.dispose();
                        teScrolledComp = null;
                        temporalEditor = null;
                    }
                });
            }

            if (!monitor.isCanceled()) {
                this.schedule(TimeUtil.MILLIS_PER_MINUTE);
            }
            return Status.OK_STATUS;
        }
    }

    private TemporalEditorDeactiveJob teDeactivateJob = new TemporalEditorDeactiveJob();

    private static ConcurrentSkipListSet<GridManager> activeList = new ConcurrentSkipListSet<>(
            new Comparator<GridManager>() {

                @Override
                public int compare(GridManager o1, GridManager o2) {
                    return o1.hashCode() - o2.hashCode();
                }

            });

    private static UpdateJob updateJob = new UpdateJob();

    private GridManagerState gmTEState = GridManagerState.GridManager;

    private Composite parent;

    private Slider timeScroller;

    private TimeScale timeScale;

    private GridBarPreferences gridBarPrefs;

    private GridCanvas gridCanvas;

    private TemporalEditor temporalEditor;

    private ScrolledComposite gridScrolledComp;

    private ScrolledComposite teScrolledComp;

    private DataManager dataManager;

    private int selectionStart;

    private int selectionEnd;

    private boolean selectionActive;

    private Runnable redraw = new RedrawRunnable();

    @Override
    public void dispose() {
        gridBarPrefs.dispose();
        teDeactivateJob.cancel();
        updateJob.cancel();
        scrollJob.cancel();
        activeList.remove(this);
    }

    /**
     * @return the selectionActive
     */
    public boolean isSelectionActive() {
        return selectionActive;
    }

    /**
     * @param selectionStart
     *            the selectionStart to set
     */
    protected void startSelection(int selectionStart) {
        this.selectionStart = selectionStart;
        this.selectionActive = true;
        extendSelection(selectionStart);
    }

    /**
     * @param selectionEnd
     *            the selectionEnd to set
     */
    protected void extendSelection(int selectionEnd) {
        if (!selectionActive) {
            return;
        }

        this.selectionEnd = selectionEnd;
        TimeRange selection = getUtil().pixelsToSelection(selectionStart,
                this.selectionEnd);

        dataManager.getParmOp().setSelectionTimeRange(selection);
    }

    protected void endSelection(int selectionEnd) {
        if (!this.selectionActive) {
            return;
        }
        extendSelection(selectionEnd);

        Date currentSETime = getSelectedTime();
        TimeRange selTR = dataManager.getParmOp().getSelectionTimeRange();

        if (selTR.contains(currentSETime)) {
            this.selectionActive = false;
            return;
        }

        Parm parm = dataManager.getSpatialDisplayManager().getActivatedParm();
        if (parm != null) {
            IGridData[] grids = parm.getGridInventory(selTR);
            if (grids.length > 0) {
                // return the latter start time of the grid's TR and the SelTR
                if (grids[0].getGridTime().compareTo(selTR) == 1) {
                    setSelectedTime(grids[0].getGridTime().getStart());
                    return;
                } else {
                    setSelectedTime(selTR.getStart());
                    return;
                }
            }
        }

        // All we can do now is return the selection Time Range start time.
        setSelectedTime(selTR.getStart());
    }

    private GridManagerUtil util;

    private TimeRange visibleTimeRange;

    private TimeRange teVisibleTimeRange;

    private boolean lockSelectionTRtoTimeStep;

    /**
     * @return the util
     */
    public GridManagerUtil getUtil() {
        return util;
    }

    @Override
    public void expandTimeScale() {
        if (this.util.expandTimeScale()) {
            adjustScroller();
            redraw();
        }
    }

    @Override
    public void contractTimeScale() {
        if (this.util.contractTimeScale()) {
            adjustScroller();
            redraw();
        }
    }

    /**
     * @return the dataManager
     */
    public DataManager getDataManager() {
        return dataManager;
    }

    /**
     * Constructor
     *
     * @param parent
     * @param dataManager
     */
    public GridManager(Composite parent, DataManager dataManager) {
        if (dataManager == null) {
            return;
        }

        this.parent = parent;
        this.dataManager = dataManager;
        this.gridBarPrefs = new GridBarPreferences();
        this.util = new GridManagerUtil(this);

        GridLayout gridLayout = new GridLayout(1, false);
        gridLayout.marginWidth = 0;
        gridLayout.marginHeight = 0;
        gridLayout.verticalSpacing = 3;
        parent.setLayout(gridLayout);

        GridData gridData = new GridData();

        timeScale = new TimeScale(parent, this);
        gridData = new GridData();
        gridData.heightHint = TimeScale.HEIGHT;
        gridData.horizontalAlignment = SWT.FILL;
        gridData.grabExcessHorizontalSpace = true;
        timeScale.setLayoutData(gridData);

        this.timeScale.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                adjustScroller();
            }
        });

        timeScroller = new Slider(parent, SWT.HORIZONTAL);
        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        timeScroller.setLayoutData(gridData);
        updateTimeRange(getDataManager().getParmManager().getSystemTimeRange());
        timeScroller.setSelection(
                (int) ((SimulatedTime.getSystemTime().getTime().getTime()
                        - TimeUtil.MILLIS_PER_HOUR)
                        / TimeUtil.MILLIS_PER_SECOND));
        timeScroller.setIncrement(TimeUtil.SECONDS_PER_HOUR);

        timeScroller.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                clearVisibleTimeRange();
                redraw();
            }
        });

        getDataManager().getParmManager().addSystemTimeRangeChangedListener(
                new ISystemTimeRangeChangedListener() {

                    @Override
                    public void systemTimeRangeChanged(
                            final TimeRange systemTimeRange) {
                        VizApp.runAsync(new Runnable() {

                            @Override
                            public void run() {
                                updateTimeRange(systemTimeRange);
                                redraw();
                            }

                        });
                    }

                });

        gridScrolledComp = new ScrolledComposite(parent, SWT.V_SCROLL);
        gridLayout = new GridLayout(1, false);
        gridScrolledComp.setLayout(gridLayout);
        gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridScrolledComp.setLayoutData(gridData);

        gridCanvas = new GridCanvas(gridScrolledComp, this);
        gridScrolledComp.setContent(gridCanvas);

        gridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gridCanvas.setLayoutData(gridData);

        gridCanvas.setSize(gridCanvas.computeSize(SWT.DEFAULT, SWT.DEFAULT));
        gridCanvas.layout();
        gridScrolledComp.setExpandHorizontal(true);
        gridScrolledComp.setExpandVertical(true);
        gridScrolledComp.getVerticalBar()
                .setIncrement(gridCanvas.getGridBarSpacing());

        lockSelectionTRtoTimeStep = GFEPreference
                .getBoolean("SelectGridsWhenStepping");
        setSelectedTime(SimulatedTime.getSystemTime().getTime());

        MouseHandler mouseHandler = new MouseHandler() {

            @Override
            public void dragEnd(MouseEvent e) {
                super.dragEnd(e);

                stopScrolling();

                if (e.button == 1) {
                    endSelection(e.x);
                }
            }

            @Override
            public void dragMove(MouseEvent e) {
                super.dragMove(e);

                Rectangle r = gridCanvas.getClientArea();
                if (e.x <= (r.x + 50)) {
                    int offset = e.x - (r.x + 50);
                    startScrolling(offset / 4);
                } else if (e.x >= (r.width - 50)) {
                    int offset = e.x - (r.width - 50);
                    startScrolling(offset / 4);
                } else {
                    stopScrolling();
                }

                if (isButtonDown(1)) {
                    extendSelection(e.x);
                }
            }

            @Override
            public void dragStart(MouseEvent e) {
                super.dragStart(e);

                if (isButtonDown(1)) {
                    startSelection(getDragAnchor().x);
                    extendSelection(e.x);
                }
            }

            @Override
            public void mouseClick(MouseEvent e) {
                if ((e.stateMask == (SWT.BUTTON1 | SWT.SHIFT))
                        || (e.stateMask == (SWT.BUTTON2 | SWT.SHIFT))) {

                    // Shift clicking to extend the highlighted area.
                    TimeRange timeRange = getDataManager()
                            .getSpatialDisplayManager().getGlobalTimeRange();

                    // setSelectedTimeRange(timeRange.span(getUtil().pixelToHour(
                    // e.x)));
                    getDataManager().getParmOp().setSelectionTimeRange(
                            timeRange.span(getUtil().pixelToHour(e.x)));
                }
            }

        };

        timeScale.addMouseListener(mouseHandler);
        gridCanvas.addMouseListener(mouseHandler);
        activeList.add(this);
        teDeactivateJob.schedule();

        if (updateJob.getState() == Job.NONE) {
            updateJob.schedule();
        }

        dataManager.getSpatialDisplayManager()
                .addSpatialEditorTimeChangedListener(this);
    }

    /**
     * @return the grid bar preferences
     */
    public GridBarPreferences getGridBarPrefs() {
        return gridBarPrefs;
    }

    /**
     * @return the visible time range
     */
    public TimeRange getVisibleTimeRange() {
        if (this.visibleTimeRange == null) {
            long duration = getUtil()
                    .pixelsToDuration(timeScale.getClientArea().width);
            this.visibleTimeRange = new TimeRange(new Date(
                    (timeScroller.getSelection() * TimeUtil.MILLIS_PER_SECOND)),
                    duration);
        }

        return this.visibleTimeRange;
    }

    /**
     * This function is used to checkVisibility for painting purposes. If the
     * visible range is null due to a recent change it will return true
     *
     * @param tr
     *            to check
     * @return if tr overlaps visible range or visible range needs to be
     *         computed
     */
    public boolean checkVisibility(TimeRange tr) {
        if ((this.visibleTimeRange != null) && !visibleTimeRange.overlaps(tr)) {
            return false;
        }
        return true;
    }

    /**
     * @return the visible time range in the temporal editor
     */
    public TimeRange getTemporalEditorVisibleTimeRange() {
        if ((this.teVisibleTimeRange == null) && (temporalEditor != null)) {
            TimeRange tr = getVisibleTimeRange().clone();
            int horizOffset = temporalEditor.getHorizOffset();
            long millis = getUtil().pixelsToDuration(horizOffset);
            Date start = tr.getStart();
            Calendar currentTickCal = Calendar
                    .getInstance(TimeZone.getTimeZone("GMT"));
            currentTickCal.setTimeInMillis(start.getTime() + millis);
            tr.setStart(currentTickCal.getTime());
            teVisibleTimeRange = tr;
        }

        return this.teVisibleTimeRange;
    }

    /**
     *
     */
    protected void adjustScroller() {
        clearVisibleTimeRange();
        int increment = (int) (getUtil()
                .pixelsToDuration(timeScale.getClientArea().width) / 1000);
        timeScroller.setThumb(increment);
        timeScroller.setPageIncrement(increment);
    }

    private void updateTimeRange(TimeRange timeRange) {
        long max = timeRange.getEnd().getTime() / TimeUtil.MILLIS_PER_SECOND;
        long min = timeRange.getStart().getTime() / TimeUtil.MILLIS_PER_SECOND;

        if (max > Integer.MAX_VALUE) {
            max = Integer.MAX_VALUE;
        }
        if (min < 0) {
            min = 0;
        }

        if (!timeScroller.isDisposed()) {
            timeScroller.setMaximum((int) max);
            timeScroller.setMinimum((int) min);
        }

        clearVisibleTimeRange();
    }

    @Override
    public void redraw() {
        if (!parent.isDisposed()) {
            timeScale.redraw();
            gridCanvas.redraw();

            if (temporalEditor != null) {
                temporalEditor.redraw();
            }
        } else {
            activeList.remove(this);
        }
    }

    protected void startScrolling(int i) {
        scrollJob.setIncrement(i);
        scrollJob.schedule();
    }

    protected void stopScrolling() {
        scrollJob.setIncrement(0);
    }

    @Override
    public void toggleTemporalEditor() {
        GridData gridData = (GridData) gridScrolledComp.getLayoutData();
        gridData.exclude = !gridData.exclude;
        gridScrolledComp.setVisible(!gridScrolledComp.getVisible());

        // if GM excluded then in TE mode
        if (gridData.exclude) {
            gmTEState = GridManagerState.TemporalEditor;
        } else {
            gmTEState = GridManagerState.GridManager;
        }

        if (temporalEditor == null) {
            createTemporalEditor();
        } else {
            gridData = (GridData) teScrolledComp.getLayoutData();
            boolean teActive = gridData.exclude;
            gridData.exclude = !teActive;
            teScrolledComp.setVisible(teActive);

            if (teActive) {
                temporalEditor.activate();
            } else {
                temporalEditor.deactivate();
            }
        }

        parent.layout();
    }

    @Override
    public GridManagerState getState() {
        return gmTEState;
    }

    private void createTemporalEditor() {
        teScrolledComp = new ScrolledComposite(parent, SWT.V_SCROLL);
        GridLayout gridLayout = new GridLayout(1, false);
        gridLayout.marginHeight = 0;
        gridLayout.marginWidth = 0;
        teScrolledComp.setLayout(gridLayout);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        teScrolledComp.setLayoutData(gridData);
        teScrolledComp.setAlwaysShowScrollBars(true);

        temporalEditor = new TemporalEditor(teScrolledComp, this);
        teScrolledComp.setContent(temporalEditor);
        teScrolledComp.setExpandHorizontal(true);
        teScrolledComp.setExpandVertical(true);
        teScrolledComp.getVerticalBar()
                .setIncrement(gridCanvas.getGridBarSpacing());
        teScrolledComp.getVerticalBar()
                .setPageIncrement(gridCanvas.getGridBarSpacing());

        teScrolledComp.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                temporalEditor.resize();
            }
        });
    }

    private void clearVisibleTimeRange() {
        this.visibleTimeRange = null;
        this.teVisibleTimeRange = null;
    }

    @Override
    public void setSelectedTime(Date selectedTime) {
        // set attribute to check if we are calling back to syncSelectTR()
        this.selectedTime = selectedTime;
        dataManager.getSpatialDisplayManager()
                .setSpatialEditorTime(selectedTime);
        redraw();
    }

    /**
     * @return the selectedTime
     */
    public Date getSelectedTime() {
        return dataManager.getSpatialDisplayManager().getSpatialEditorTime();
    }

    protected void syncSelectTR(Date t) {
        /*
         * setSelectedTime() will eventually call back to this method, in which
         * case skip the original code (within the if block immediately below).
         */
        if (!t.equals(selectedTime)) {
            // Use a selection tr of 1 hour duration if no active parm,
            // if active parm and no grid, use the parm's time constraint,
            // if active parm and grid, use the grid time.
            if (lockSelectionTRtoTimeStep) {
                TimeRange tr;
                Parm parm = dataManager.getSpatialDisplayManager()
                        .getActivatedParm();

                if (parm == null) {
                    Date tbase = new Date(
                            (t.getTime() / TimeUtil.MILLIS_PER_HOUR)
                                    * TimeUtil.MILLIS_PER_HOUR);
                    tr = new TimeRange(tbase, TimeUtil.MILLIS_PER_HOUR);
                } else {
                    GridID gridid = new GridID(parm, t);
                    if (gridid.grid() != null) {
                        tr = gridid.grid().getGridTime();
                    } else {
                        tr = parm.getGridInfo().getTimeConstraints()
                                .constraintTime(t);
                    }
                    dataManager.getParmOp().deselectAll();
                    parm.getParmState().setSelected(true);
                }

                dataManager.getParmOp().setSelectionTimeRange(tr);
            }
        } else {
            // reset
            selectedTime = null;
        }
    }

    /**
     * @return the lockSelectionTRtoTimeStep
     */
    @Override
    public boolean isLockSelectionTRtoTimeStep() {
        return lockSelectionTRtoTimeStep;
    }

    /**
     * @param lockSelectionTRtoTimeStep
     *            the lockSelectionTRtoTimeStep to set
     */
    @Override
    public void setLockSelectionTRtoTimeStep(
            boolean lockSelectionTRtoTimeStep) {
        this.lockSelectionTRtoTimeStep = lockSelectionTRtoTimeStep;

        ICommandService service = PlatformUI.getWorkbench()
                .getService(ICommandService.class);

        service.refreshElements(
                "com.raytheon.viz.gfe.actions.SelectGridsWhenStepping", null);
    }

    @Override
    public void spatialEditorTimeChanged(Date date) {
        syncSelectTR(date);
        VizApp.runAsync(redraw);
    }
}
