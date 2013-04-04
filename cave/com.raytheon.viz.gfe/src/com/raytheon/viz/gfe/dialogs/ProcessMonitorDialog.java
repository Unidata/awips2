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
package com.raytheon.viz.gfe.dialogs;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.tasks.AbstractGfeTask;
import com.raytheon.viz.gfe.tasks.AbstractGfeTask.TaskStatus;
import com.raytheon.viz.gfe.tasks.ITaskStatusChangedListener;
import com.raytheon.viz.gfe.tasks.TaskManager;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The dialog for process monitor.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 7, 2008			   Eric Babin  Initial Creation
 * 02/12/2013       #1597  randerso    Modified TaskOutputDialog to support GFE Performance metrics
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class ProcessMonitorDialog extends CaveJFACEDialog implements
        ITaskStatusChangedListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProcessMonitorDialog.class);

    private Composite top;

    private Composite pendingComp, finishedComp;

    private Image destroyImg, xtermImg, startedImg, pendingImg;

    public ProcessMonitorDialog(Shell parent) {
        super(parent);
        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);
        this.destroyImg = Activator.imageDescriptorFromPlugin(
                Activator.PLUGIN_ID, "icons/destroy.gif").createImage();
        this.xtermImg = Activator.imageDescriptorFromPlugin(
                Activator.PLUGIN_ID, "icons/xterm.gif").createImage();

        Rectangle bounds = this.xtermImg.getBounds();
        this.startedImg = new Image(parent.getDisplay(), bounds);
        GC gc = new GC(this.startedImg);
        gc.setBackground(parent.getDisplay().getSystemColor(SWT.COLOR_GREEN));
        gc.fillRectangle(bounds);
        gc.dispose();

        this.pendingImg = new Image(parent.getDisplay(), bounds);
        gc = new GC(this.pendingImg);
        gc.setBackground(parent.getDisplay().getSystemColor(
                SWT.COLOR_DARK_GREEN));
        gc.fillRectangle(bounds);
        gc.dispose();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);

        shell.setText("External Programs");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#close()
     */
    @Override
    public boolean close() {
        TaskManager.getInstance().removeTaskStatusChangedListener(this);
        return super.close();
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
        top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout(1, true);
        top.setLayout(layout);

        initializeComponents();

        refreshLists();
        TaskManager.getInstance().addTaskStatusChangedListener(this);

        return top;
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.CANCEL_ID, "Dismiss", false);
    }

    private void initializeComponents() {
        Composite comp = new Composite(top, SWT.BORDER);
        GridLayout layout = new GridLayout(1, false);
        comp.setLayout(layout);
        layout.marginWidth = 1;
        layout.marginHeight = 1;
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        comp.setLayoutData(layoutData);

        Label lab = new Label(comp, SWT.NONE);
        layoutData = new GridData(GridData.FILL_HORIZONTAL);
        layoutData.horizontalAlignment = SWT.CENTER;
        lab.setText("Local Scripts");
        lab.setLayoutData(layoutData);

        this.pendingComp = createTaskList(comp, "Pending");
        layout = (GridLayout) pendingComp.getLayout();
        this.finishedComp = createTaskList(comp, "Finished");
        layout = (GridLayout) finishedComp.getLayout();

    }

    private Composite createTaskList(Composite parent, String title) {
        Composite main = new Composite(parent, SWT.BORDER);
        GridLayout layout = new GridLayout(1, false);
        layout.marginWidth = 1;
        layout.marginHeight = 1;
        main.setLayout(layout);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        main.setLayoutData(layoutData);

        Label lab3 = new Label(main, SWT.NONE);
        layoutData = new GridData(GridData.FILL_HORIZONTAL);
        layoutData.horizontalAlignment = SWT.CENTER;
        lab3.setText(title);
        lab3.setLayoutData(layoutData);

        final ScrolledComposite scrolled = new ScrolledComposite(main,
                SWT.BORDER | SWT.V_SCROLL);
        layoutData = new GridData(300, 120);
        scrolled.setLayoutData(layoutData);
        layout = new GridLayout(1, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        layout.horizontalSpacing = 0;
        layout.verticalSpacing = 0;
        scrolled.setLayout(layout);
        scrolled.setAlwaysShowScrollBars(true);

        final Composite comp = new Composite(scrolled, SWT.NONE);

        layout = new GridLayout(1, false);
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        layout.horizontalSpacing = 0;
        layout.verticalSpacing = 0;
        comp.setLayout(layout);
        layoutData = new GridData(300, 200);
        comp.setLayoutData(layoutData);

        scrolled.setContent(comp);
        scrolled.setExpandVertical(true);
        scrolled.setMinSize(comp.getSize());

        scrolled.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                scrolled.setMinSize(comp.computeSize(SWT.DEFAULT, SWT.DEFAULT));
            }
        });

        scrolled.layout();
        return comp;
    }

    /**
     * 
     */
    private void refreshLists() {
        java.util.List<AbstractGfeTask> pending = new ArrayList<AbstractGfeTask>();
        java.util.List<AbstractGfeTask> finished = new ArrayList<AbstractGfeTask>();

        // get the task list
        java.util.List<AbstractGfeTask> taskList = TaskManager.getInstance()
                .getTaskList();
        for (AbstractGfeTask task : taskList) {
            if (task.getStatus() == TaskStatus.FINISHED
                    || task.getStatus() == TaskStatus.CANCELED) {
                finished.add(task);
            } else {
                pending.add(task);
            }
        }

        Collections.sort(pending);
        Collections.sort(finished);

        // dispose all the old pending controls
        for (Control c : pendingComp.getChildren()) {
            c.dispose();
        }

        // dispose all the old finished controls
        for (Control c : finishedComp.getChildren()) {
            c.dispose();
        }

        // create listeners
        SelectionAdapter displayLogListener = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                Button button = (Button) e.widget;
                displayLog((AbstractGfeTask) button.getData());
            }
        };

        SelectionAdapter runTaskListener = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                Button button = (Button) e.widget;
                TaskManager.getInstance().forceRunTask(
                        (AbstractGfeTask) button.getData());
            }
        };

        SelectionAdapter destroyTaskListener = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                Button button = (Button) e.widget;
                TaskManager.getInstance().cancelTask(
                        (AbstractGfeTask) button.getData());
            }
        };

        // create the pending controls
        for (AbstractGfeTask task : pending) {
            Composite comp = new Composite(pendingComp, SWT.NONE);
            comp.setLayoutData(new GridData(300, SWT.DEFAULT));
            GridLayout layout = new GridLayout(4, false);
            layout.marginHeight = 0;
            layout.marginWidth = 0;
            layout.horizontalSpacing = 0;
            layout.verticalSpacing = 0;
            comp.setLayout(layout);

            Button button = new Button(comp, SWT.PUSH);
            button.setData(task);
            GridData layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false,
                    false);
            button.setLayoutData(layoutData);
            if (task.getStatus() == TaskStatus.PENDING) {
                button.setImage(this.pendingImg);
                button.addSelectionListener(runTaskListener);
            } else {
                button.setImage(this.startedImg);
            }

            button = new Button(comp, SWT.PUSH);
            button.setData(task);
            button.setImage(this.destroyImg);
            layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
            button.setLayoutData(layoutData);
            button.addSelectionListener(destroyTaskListener);

            button = new Button(comp, SWT.PUSH);
            button.setData(task);
            button.setImage(this.xtermImg);
            layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
            button.setLayoutData(layoutData);
            button.addSelectionListener(displayLogListener);

            Text text = new Text(comp, SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE);
            layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
            text.setLayoutData(layoutData);
            text.setText(task.getDisplayName());

            comp.pack();
        }
        pendingComp.pack();
        pendingComp.layout();
        ScrolledComposite scrolled = (ScrolledComposite) pendingComp
                .getParent();
        scrolled.setMinSize(finishedComp.getSize());
        scrolled.layout();

        // create the finished controls
        for (AbstractGfeTask task : finished) {
            Composite comp = new Composite(finishedComp, SWT.NONE);
            comp.setLayoutData(new GridData(300, SWT.DEFAULT));
            GridLayout layout = new GridLayout(2, false);
            layout.marginHeight = 0;
            layout.marginWidth = 0;
            layout.horizontalSpacing = 0;
            layout.verticalSpacing = 0;
            comp.setLayout(layout);

            Button button = new Button(comp, SWT.PUSH);
            button.setData(task);
            button.setImage(this.xtermImg);
            GridData layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false,
                    false);
            button.setLayoutData(layoutData);
            button.addSelectionListener(displayLogListener);

            Text text = new Text(comp, SWT.BORDER | SWT.READ_ONLY | SWT.SINGLE);
            layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
            text.setLayoutData(layoutData);
            text.setText(task.getDisplayName());

            comp.pack();
        }
        finishedComp.pack();
        finishedComp.layout();
        scrolled = (ScrolledComposite) finishedComp.getParent();
        scrolled.setMinSize(finishedComp.getSize());
        scrolled.layout();
    }

    private void displayLog(AbstractGfeTask task) {
        TaskOutputDialog dlg = new TaskOutputDialog(getShell(), task);
        dlg.open();
    }

    protected static class TaskOutputDialog extends CaveJFACEDialog {

        private AbstractGfeTask task;

        private Text logText;

        /**
         * @param parentShell
         */
        protected TaskOutputDialog(Shell parentShell, AbstractGfeTask task) {
            super(parentShell);
            this.setShellStyle(SWT.RESIZE);
            this.task = task;
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
            newShell.setText(task.getDisplayName());
            super.configureShell(newShell);
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.
         * eclipse.swt.widgets.Composite)
         */
        @Override
        protected Control createDialogArea(Composite parent) {
            Composite comp = (Composite) super.createDialogArea(parent);
            Text cmdText = new Text(comp, SWT.BORDER | SWT.READ_ONLY
                    | SWT.MULTI | SWT.WRAP);
            GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
            layoutData.widthHint = this.convertWidthInCharsToPixels(120);
            cmdText.setLayoutData(layoutData);
            cmdText.setText(task.getCommand());

            logText = new Text(comp, SWT.MULTI | SWT.BORDER | SWT.READ_ONLY
                    | SWT.H_SCROLL | SWT.V_SCROLL);
            layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
            layoutData.widthHint = this.convertWidthInCharsToPixels(120);
            layoutData.heightHint = this.convertHeightInCharsToPixels(24);
            logText.setLayoutData(layoutData);
            logText.setBackground(logText.getDisplay().getSystemColor(
                    SWT.COLOR_BLACK));
            logText.setForeground(logText.getDisplay().getSystemColor(
                    SWT.COLOR_WHITE));

            logText.setText(getLogFileContents());

            Composite timeComp = new Composite(comp, SWT.NONE);
            GridLayout layout = new GridLayout(2, false);
            layout.marginWidth = 0;
            layout.verticalSpacing = 0;
            timeComp.setLayout(layout);
            layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
            timeComp.setLayoutData(layoutData);

            SimpleDateFormat sdf = new SimpleDateFormat(
                    "EEE MMM d HH:mm:ss yyyy");
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

            Composite startComp = new Composite(timeComp, SWT.NONE);
            layout = new GridLayout(2, false);
            layout.marginWidth = 0;
            layout.verticalSpacing = 0;
            startComp.setLayout(layout);
            layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
            startComp.setLayoutData(layoutData);

            Label label = new Label(startComp, SWT.NONE);
            Date date;
            if (task.getStartedTime() == null) {
                label.setText("Queue Time:");
                date = task.getQueuedTime();
            } else {
                label.setText("Start Time:");
                date = task.getStartedTime();
            }

            Text startText = new Text(startComp, SWT.BORDER | SWT.READ_ONLY);
            startText.setText(sdf.format(date));
            layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            startText.setLayoutData(layoutData);

            Composite elapsedComp = new Composite(timeComp, SWT.NONE);
            layout = new GridLayout(2, false);
            layout.marginWidth = 0;
            layout.verticalSpacing = 0;
            elapsedComp.setLayout(layout);
            layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
            elapsedComp.setLayoutData(layoutData);
            if (task.getFinishedTime() != null) {
                label = new Label(elapsedComp, SWT.RIGHT);
                layoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
                label.setText("Elapsed:");
                label.setLayoutData(layoutData);

                long delta = task.getFinishedTime().getTime()
                        - task.getStartedTime().getTime();
                Text elapsedText = new Text(elapsedComp, SWT.BORDER
                        | SWT.READ_ONLY);
                elapsedText.setText(delta + " ms");
                layoutData = new GridData(SWT.RIGHT, SWT.DEFAULT, false, false);
                elapsedText.setLayoutData(layoutData);

                label = new Label(startComp, SWT.NONE);
                if (task.getStatus() == TaskStatus.FINISHED) {
                    label.setText("End Time:");
                } else {
                    label.setText("Canceled Time:");
                }

                Text endText = new Text(startComp, SWT.BORDER | SWT.READ_ONLY);
                endText.setText(sdf.format(task.getFinishedTime()));
                layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
                endText.setLayoutData(layoutData);
            }

            return comp;
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
            createButton(parent, IDialogConstants.OK_ID, "Save As...", false);
            createButton(parent, IDialogConstants.CANCEL_ID, "Dismiss", true);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.dialogs.Dialog#okPressed()
         */
        @Override
        protected void okPressed() {
            FileDialog fd = new FileDialog(getShell(), SWT.SAVE);
            fd.setText("Save As");
            fd.setOverwrite(true);
            String fileName = fd.open();
            if (fileName != null) {
                OutputStreamWriter out = null;
                try {
                    out = new OutputStreamWriter(new FileOutputStream(new File(
                            fileName)));
                    out.write(logText.getText());
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);

                } finally {
                    if (out != null) {
                        try {
                            out.close();
                        } catch (IOException e) {
                            statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        }
                    }
                }
            }
            super.okPressed();
        }

        private String getLogFileContents() {
            String s = "";
            InputStreamReader in = null;
            try {
                File file = task.getLogFile();
                if (file != null && file.exists()) {
                    in = new InputStreamReader(new FileInputStream(file),
                            "UTF-8");
                    int n = (int) file.length();
                    char[] contents = new char[n];
                    in.read(contents);
                    s = String.valueOf(contents);
                }

            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);

            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);

            } finally {
                try {
                    if (in != null) {
                        in.close();
                    }
                } catch (IOException e) {
                    // do nothing
                }
            }
            return s;
        }
    }

    @Override
    public void taskStatusChanged(AbstractGfeTask task) {
        getShell().getDisplay().asyncExec(new Runnable() {

            @Override
            public void run() {
                refreshLists();
            }
        });
    }

}
