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
package com.raytheon.viz.gfe.dialogs.sbu;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.progress.UIJob;

import com.raytheon.uf.common.dataplugin.gfe.svcbu.JobProgress;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.AbstractSbuTask;
import com.raytheon.viz.gfe.dialogs.sbu.jobs.ServiceBackupTaskExecutor;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Display Service Backup job status for a site
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 19, 2015  #4300     randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class ServiceBackupStatusDlg extends CaveJFACEDialog {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ServiceBackupStatusDlg.class);

    private static enum StatusIcons {
        NOT_STARTED("…", "Not started", SWT.COLOR_WIDGET_FOREGROUND), //
        IN_PROGRESS("", "In progress", SWT.COLOR_WIDGET_FOREGROUND), //
        SUCCESS("✔", "Successful", SWT.COLOR_GREEN), //
        FAILED("✖", "Failed", SWT.COLOR_RED); //

        private String symbol;

        private String toolTip;

        private int color;

        private int cycle = 0;

        private StatusIcons(String symbol, String toolTip, int color) {
            this.symbol = symbol;
            this.toolTip = toolTip;
            this.color = color;
        }

        public char getSymbol() {
            cycle = (cycle + 1) % this.symbol.length();
            return this.symbol.charAt(cycle);
        }

        public String getToolTip() {
            return this.toolTip;
        }

        public int getColor() {
            return this.color;
        }
    }

    private String site;

    private List<AbstractSbuTask> tasks;

    private ServiceBackupTaskExecutor executor;

    private Font bigFont;

    private Map<String, Label[]> controls;

    private Group statusGroup;

    private UIJob updateJob;

    /**
     * @param parentShell
     */
    public ServiceBackupStatusDlg(Shell parentShell, String site) {
        super(parentShell);
        this.site = site;
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS | SWT.MIN);

        updateJob = new UIJob("SvcbuUpdateJob") {
            @Override
            public IStatus runInUIThread(IProgressMonitor monitor) {
                if (!getShell().isDisposed()) {
                    doRefresh();

                    if (executor.isAlive()) {
                        this.schedule(1000);
                    } else {
                        getButton(IDialogConstants.CANCEL_ID).setEnabled(false);
                    }
                    return Status.OK_STATUS;
                } else {
                    return Status.CANCEL_STATUS;
                }
            }

        };
        updateJob.setSystem(true);
    }

    /**
     * @param tasks
     */
    public void setTasks(List<AbstractSbuTask> tasks) {
        if ((this.executor != null) && this.executor.isAlive()) {
            statusHandler.error("Service Backup job already in progress");
            return;
        }
        this.tasks = tasks;
        this.executor = new ServiceBackupTaskExecutor(tasks);

        if (this.isOpen()) {
            for (Control control : statusGroup.getChildren()) {
                control.dispose();
            }

            createStatusControls();
            statusGroup.pack();
            statusGroup.layout();
            Shell shell = getShell();
            shell.pack();
            shell.layout();

            startJob();
        }
    }

    /**
     * @return the site
     */
    public String getSite() {
        return site;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText(this.site + " Service Backup Status");
        newShell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                if (bigFont != null) {
                    bigFont.dispose();
                }

                if (executor != null) {
                    executor.cancel();
                }
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#getInitialLocation(org.eclipse.swt.graphics
     * .Point)
     */
    @Override
    protected Point getInitialLocation(Point initialSize) {
        Rectangle bounds = getParentShell().getBounds();
        int x = bounds.x + bounds.width;
        int y = bounds.y;

        Rectangle myBounds = new Rectangle(x, y, initialSize.x, initialSize.y);
        for (Shell child : getParentShell().getShells()) {
            if (!child.equals(getShell()) && !child.isDisposed()) {
                Rectangle childBounds = child.getBounds();
                if (myBounds.contains(childBounds.x, childBounds.y)) {
                    Rectangle clientArea = child.getClientArea();
                    int delta = childBounds.height - clientArea.height;
                    y += delta;
                }
            }
        }

        Point location = new Point(x, y);
        return location;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        final Composite comp = (Composite) super.createDialogArea(parent);

        final Button bigFontButton = new Button(comp, SWT.CHECK);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        bigFontButton.setLayoutData(layoutData);
        bigFontButton.setText("Use large font");
        bigFontButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (bigFontButton.getSelection()) {
                    FontData fontData = comp.getFont().getFontData()[0];
                    fontData.setHeight(18);
                    fontData.setStyle(SWT.BOLD);
                    bigFont = new Font(comp.getDisplay(), fontData);
                    setFont(bigFont);
                } else {
                    Font font = comp.getFont();
                    setFont(font);
                    if (bigFont != null) {
                        bigFont.dispose();
                        bigFont = null;
                    }
                }
            }
        });

        controls = new HashMap<>();

        statusGroup = new Group(comp, SWT.SHADOW_NONE);
        statusGroup.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
        statusGroup.setLayout(new GridLayout(3, false));
        statusGroup.setText("Job Status");
        createStatusControls();

        comp.pack();
        comp.layout();

        return comp;
    }

    /**
     * 
     */
    private void createStatusControls() {
        controls.clear();
        for (AbstractSbuTask task : tasks) {
            String status = task.getStatusFileName();
            String text = task.getGuiDescription();

            Label statusIcon = new Label(statusGroup, SWT.HORIZONTAL
                    | SWT.CENTER);
            statusIcon.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                    false));
            StatusIcons icon = StatusIcons.NOT_STARTED;
            statusIcon.setForeground(statusIcon.getDisplay().getSystemColor(
                    icon.getColor()));
            statusIcon.setText(Character.toString(icon.getSymbol()));
            statusIcon.setToolTipText(icon.getToolTip());

            Label time = new Label(statusGroup, SWT.HORIZONTAL);
            time.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
            time.setText("00:00:00 ");

            Label label = new Label(statusGroup, SWT.HORIZONTAL);
            label.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
            label.setText(String.format(text, site));

            if (bigFont != null) {
                statusIcon.setFont(bigFont);
                time.setFont(bigFont);
                label.setFont(bigFont);
            }

            controls.put(status, new Label[] { statusIcon, time, label });
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false).setEnabled(false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
     */
    @Override
    protected void cancelPressed() {
        cancelJob();
        getButton(IDialogConstants.CANCEL_ID).setEnabled(false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveJFACEDialog#open()
     */
    @Override
    public int open() {
        int status = super.open();

        startJob();

        return status;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#close()
     */
    @Override
    public boolean close() {
        if (executor.isAlive()) {
            MessageDialog
                    .openWarning(getShell(), this.site
                            + " Service Backup Status",
                            "You cannot close this dialog while tasks are in progress!");
            return false;
        }
        cancelJob();
        return super.close();
    }

    private void startJob() {
        this.executor.start();
        updateJob.schedule(1000);
        getButton(IDialogConstants.CANCEL_ID).setEnabled(true);
    }

    private void cancelJob() {
        this.executor.cancel();
    }

    private void setFont(Font font) {
        for (Label[] labels : controls.values()) {
            for (Label label : labels) {
                label.setFont(font);
            }
        }
        getShell().pack(true);
    }

    private void updateStatus(final AbstractSbuTask task) {
        String key = task.getStatusFileName();
        JobProgress jobProgress = task.getStatus();

        SimpleDateFormat sdf = new SimpleDateFormat("HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

        Date statusTime = task.getStatusTime();
        Label[] labels = controls.get(key);
        if (labels != null) {
            Label statusIcon = labels[0];
            Label timeLabel = labels[1];

            StatusIcons icon;
            switch (jobProgress) {
            case IN_PROGRESS:
                icon = StatusIcons.IN_PROGRESS;
                statusTime = new Date(System.currentTimeMillis()
                        - statusTime.getTime());
                break;
            case SUCCESS:
                icon = StatusIcons.SUCCESS;
                break;
            case FAILED:
                icon = StatusIcons.FAILED;
                break;

            case UNKNOWN:
                icon = StatusIcons.NOT_STARTED;
                break;

            default:
                statusHandler.error("Unknown JobProgress value received: "
                        + jobProgress.name());
                icon = StatusIcons.IN_PROGRESS;
            }
            if (!statusIcon.isDisposed()) {
                statusIcon.setForeground(statusIcon.getDisplay()
                        .getSystemColor(icon.getColor()));
                statusIcon.setText(Character.toString(icon.getSymbol()));
                statusIcon.setToolTipText(icon.getToolTip());
            }

            if (!timeLabel.isDisposed()) {
                timeLabel.setText(sdf.format(statusTime));
            }
        }
    }

    private void doRefresh() {
        for (AbstractSbuTask task : tasks) {
            updateStatus(task);
        }
    }

    public void reset() {
        StatusIcons icon = StatusIcons.NOT_STARTED;

        for (Label[] labels : controls.values()) {
            Label statusIcon = labels[0];
            Label timeLabel = labels[1];
            if (!statusIcon.isDisposed()) {
                statusIcon.setForeground(statusIcon.getDisplay()
                        .getSystemColor(icon.getColor()));
                statusIcon.setText(Character.toString(icon.getSymbol()));
                statusIcon.setToolTipText(icon.getToolTip());
            }

            if (!timeLabel.isDisposed()) {
                timeLabel.setText("00:00:00");
            }
        }
    }

}
