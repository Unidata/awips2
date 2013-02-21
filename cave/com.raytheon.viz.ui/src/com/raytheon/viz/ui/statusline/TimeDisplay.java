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

package com.raytheon.viz.ui.statusline;

import java.text.SimpleDateFormat;
import java.util.Comparator;
import java.util.Date;
import java.util.TimeZone;
import java.util.concurrent.ConcurrentSkipListSet;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.ContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.progress.UIJob;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.ISimulatedTimeChangeListener;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.ui.actions.ShowTimeDialog;

/**
 * 
 * Contribution item added to the status bar which displays the current
 * workstation time. The TimeJob class is responsible for updating this display
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30,2007  461        bphillip    Initial Creation
 * 09JUL2008    1234        ebabin      Updates for color, and display issues.
 * Jan 09, 2013 1442       rferrel     Added Simulated Time Change listener.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class TimeDisplay extends ContributionItem {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(TimeDisplay.class);

    /**
     * Job to update the time display
     */
    private static class TimeUpdateJob extends UIJob {

        /**
         * @param name
         */
        public TimeUpdateJob() {
            super("TimeUpdate");
            this.setSystem(true);
        }

        /*
         * (non-Javadoc)
         * 
         * @seeorg.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        public IStatus runInUIThread(IProgressMonitor monitor) {
            // if any displays are active
            if (!activeList.isEmpty()) {
                // update the state of all active displays
                for (TimeDisplay td : activeList) {
                    td.update();
                }

                long t = System.currentTimeMillis() % 60000;
                this.schedule(60000 - t);
            }

            return Status.OK_STATUS;
        }
    }

    private static ConcurrentSkipListSet<TimeDisplay> activeList = new ConcurrentSkipListSet<TimeDisplay>(
            new Comparator<TimeDisplay>() {

                @Override
                public int compare(TimeDisplay o1, TimeDisplay o2) {
                    return o1.hashCode() - o2.hashCode();
                }

            });

    private static TimeUpdateJob updateJob = new TimeUpdateJob();

    private static final TimeZone GMT = TimeZone.getTimeZone("GMT");

    private static final String gmtPattern = "HH:mm'Z' dd-MMM-yy";

    private static final String localPattern = "'Local Time' z:\nHH:mm dd-MMM-yy";

    private Composite comp;

    private Label timeLabel;

    /** Label used to display the current workstation time */
    private Label theTextArea;

    private final SimpleDateFormat gmtFormatter;

    private final SimpleDateFormat localFormatter;

    // workaround for the random tooltip following the cursor
    private boolean displayTooltip = true;

    private ISimulatedTimeChangeListener timeChangeListener;

    /**
     * Constructor
     */
    public TimeDisplay() {
        super("TimeDisplay");

        gmtFormatter = new SimpleDateFormat(gmtPattern);
        gmtFormatter.setTimeZone(GMT);

        localFormatter = new SimpleDateFormat(localPattern);
        timeChangeListener = new ISimulatedTimeChangeListener() {

            @Override
            public void timechanged() {
                update();
            }
        };
        SimulatedTime.getSystemTime().addSimulatedTimeChangeListener(
                timeChangeListener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.action.ContributionItem#dispose()
     */
    @Override
    public void dispose() {
        activeList.remove(this);
        SimulatedTime.getSystemTime().removeSimulatedTimeChangeListener(
                timeChangeListener);
        super.dispose();
    }

    /**
     * Populates the current time in the display pane
     */
    @Override
    public void fill(Composite parent) {
        comp = new Composite(parent, SWT.NONE);

        comp.setLayout(new GridLayout(2, false));

        timeLabel = new Label(comp, SWT.NONE);
        timeLabel.setText("Time:");

        theTextArea = new Label(comp, SWT.BORDER);
        theTextArea
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));
        theTextArea.addMouseListener(new MouseListener() {

            @Override
            public void mouseDoubleClick(MouseEvent e) {
                displayTooltip = false;
                ShowTimeDialog action = new ShowTimeDialog();
                try {
                    action.execute(null);
                } catch (ExecutionException e1) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error executing show time dialog action", e1);
                }
                displayTooltip = true;
            }

            @Override
            public void mouseDown(MouseEvent e) {

            }

            @Override
            public void mouseUp(MouseEvent e) {
            }

        });
        update();

        activeList.add(this);
        if (updateJob.getState() == Job.NONE) {
            updateJob.schedule();
        }

    }

    /**
     * Updates the displayed time
     */
    @Override
    public void update() {
        Display.getDefault().syncExec(new Runnable() {

            @Override
            public void run() {
                updateColors();
                updateText();
            }

        });
    }

    /**
     * Updates the text of the labels
     */
    private void updateText() {
        // System.out.println("updating time");
        if (theTextArea == null || theTextArea.isDisposed()) {
            return;
        }
        // Populates the current workstation time in the display
        Date date = SimulatedTime.getSystemTime().getTime();
        theTextArea.setText(gmtFormatter.format(date));

        // Sets the tool tip to contain the current workstation local time
        String localTime = localFormatter.format(date);

        // this workaround makes it so SWT does not display a tooltip while
        // certain dialogs are up which causes a bug making the tooltip follow
        // the cursor around the screen
        if (displayTooltip) {
            theTextArea.setToolTipText(localTime);
            timeLabel.setToolTipText(localTime);
        } else {
            theTextArea.setToolTipText(null);
            timeLabel.setToolTipText(null);
        }
        theTextArea.pack();
    }

    /**
     * Updates the colors according to the following scheme:
     * <p>
     * Black Font Grey Background: Using real time<br>
     * White Font Black Background: Modified time not frozen<br>
     * Yellow Font Black Background: Modified time frozen
     */
    private void updateColors() {
        if (theTextArea != null && !theTextArea.isDisposed()) {
            if (SimulatedTime.getSystemTime().isRealTime()) {
                theTextArea.setForeground(CAVEMode.getForegroundColor());
                theTextArea.setBackground(CAVEMode.getBackgroundColor());
            } else {
                theTextArea.setBackground(Display.getDefault().getSystemColor(
                        SWT.COLOR_BLACK));
                if (SimulatedTime.getSystemTime().isFrozen()) {
                    theTextArea.setForeground(Display.getDefault()
                            .getSystemColor(SWT.COLOR_YELLOW));
                } else {
                    theTextArea.setForeground(Display.getDefault()
                            .getSystemColor(SWT.COLOR_WHITE));
                }
            }
        }
    }
}
