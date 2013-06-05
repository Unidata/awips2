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
package com.raytheon.uf.viz.archive.ui;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.ProgressBar;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.archive.config.ArchiveConfigManager;
import com.raytheon.uf.common.archive.config.ArchiveConfigManager.DisplayData;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Class to show progress of creating a case.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 3, 2013  1966       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrelGenerateJob
 * @version 1.0
 */

public class GenerateCaseDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GenerateCaseDlg.class);

    /** Use to display the current state of the case generation. */
    private Label stateLbl;

    /** Progress bar to indicate activity. */
    private ProgressBar progressBar;

    /** Stops the case generation leaving it in an unknown state. */
    private Button cancelBtn;

    /** Active have generation is finish or has an error. */
    private Button closeBtn;

    /** The main destination directory. */
    private final File targetDir;

    /**
     * The case's destination directory. Assumed to be a sub-directory of
     * targetDir.
     */
    private final File caseDir;

    /** Starting time for the case. */
    private final Calendar startCal;

    /** End time for the case. */
    private final Calendar endCal;

    /** Data list for the case. */
    private final List<DisplayData> sourceDataList;

    /** When true compress the case directory. */
    private final boolean doCompress;

    /** When true break the compress file into multliple files. */
    private final boolean doMultiFiles;

    /** The compress size for muliple files. */
    private final int compressSize;

    /** Assumed to be MG or GB to indicate scaling factor for compressSize. */
    private final String sizeType;

    /** Job to perform the case generation off of the UI thread. */
    private GenerateJob generateJob;

    /** Shorten case name to use in status message. */
    private final String caseName;

    /** Listeners to add to the job when scheduled. */
    private final List<IJobChangeListener> jobListeners = new ArrayList<IJobChangeListener>();

    /** Data manager. */
    private final ArchiveConfigManager archiveManager = ArchiveConfigManager
            .getInstance();

    /**
     * Constructor.
     * 
     * @param parentShell
     * @param targetDir
     * @param caseDir
     * @param startCal
     * @param endCal
     * @param sourceList
     * @param doCompress
     * @param doMultiFiles
     * @param compressSize
     * @param sizeType
     */
    protected GenerateCaseDlg(Shell parentShell, File targetDir, File caseDir,
            Calendar startCal, Calendar endCal, List<DisplayData> sourceList,
            boolean doCompress, boolean doMultiFiles, int compressSize,
            String sizeType) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL,
                CAVE.DO_NOT_BLOCK);
        this.targetDir = targetDir;
        this.caseDir = caseDir;
        this.startCal = startCal;
        this.endCal = endCal;
        this.sourceDataList = new ArrayList<DisplayData>(sourceList);
        this.doCompress = doCompress;
        this.doMultiFiles = doMultiFiles;
        this.compressSize = compressSize;
        this.sizeType = sizeType;
        this.caseName = caseDir.getAbsolutePath().substring(
                targetDir.getAbsolutePath().length() + 1);
        setText("Generating - " + caseName);
    }

    /**
     * Add a job listener.
     * 
     * @param listener
     */
    protected void addJobChangeListener(IJobChangeListener listener) {
        jobListeners.add(listener);
    }

    /**
     * Remve a job listener.
     * 
     * @param listener
     */
    protected void removeJobChangeListener(IJobChangeListener listener) {
        jobListeners.remove(listener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.horizontalSpacing = 0;
        mainComp.setLayout(gl);

        init();
    }

    /**
     * Set up main layout.
     */
    private void init() {
        createProgress();
        GuiUtil.addSeparator(shell, SWT.HORIZONTAL);
        createBottomActionButtons();
    }

    /**
     * The progress component.
     */
    private void createProgress() {
        Composite progComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        progComp.setLayout(gl);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.minimumWidth = 300;
        stateLbl = new Label(shell, SWT.BORDER | SWT.CENTER);
        stateLbl.setLayoutData(gd);
        stateLbl.setText("state of progress goes here.");

        progressBar = new ProgressBar(shell, SWT.DEFAULT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        progressBar.setLayoutData(gd);
    }

    /**
     * Component for the action buttons.
     */
    private void createBottomActionButtons() {
        Composite actionComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        actionComp.setLayout(gl);

        cancelBtn = new Button(actionComp, SWT.PUSH);
        cancelBtn.setText(" Cancel ");
        cancelBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                if (generateJob != null && generateJob.getState() != Job.NONE) {
                    generateJob.cancel();
                }

                close();
            }
        });

        closeBtn = new Button(actionComp, SWT.PUSH);
        closeBtn.setText(" Close ");
        closeBtn.setEnabled(false);
        closeBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialog#preOpened()
     */
    @Override
    protected void preOpened() {
        super.preOpened();

        generateJob = new GenerateJob();
        for (IJobChangeListener listener : jobListeners) {
            generateJob.addJobChangeListener(listener);
        }

        generateJob.schedule();
        shell.addShellListener(new ShellAdapter() {

            @Override
            public void shellClosed(ShellEvent e) {
                if (generateJob != null && generateJob.getState() != Job.NONE) {
                    e.doit = false;
                }
            }
        });
    }

    /**
     * Allow a non-UI thread to update the state label.
     * 
     * @param message
     * @param tooltip
     */
    private void setStateLbl(final String message, final String tooltip) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                if (!stateLbl.isDisposed()) {
                    stateLbl.setText(message);
                    stateLbl.setToolTipText(tooltip);
                }
            }
        });
    }

    /**
     * Allow a non-UI thread to update the progress bar and background of the
     * state label when there is an error.
     * 
     * @param value
     * @param state
     */
    private void setProgressBar(final int value, final int state) {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (!progressBar.isDisposed()) {
                    int val = value;
                    if (val < progressBar.getMinimum()) {
                        val = progressBar.getMinimum();
                    } else if (val >= progressBar.getMaximum()) {
                        val = progressBar.getMaximum();
                        closeBtn.setEnabled(true);
                        cancelBtn.setEnabled(false);
                    }
                    progressBar.setSelection(val);
                    progressBar.setState(state);
                    progressBar.setVisible(val != progressBar.getMaximum());
                    Color bgColor = null;
                    if (state == SWT.ERROR) {
                        bgColor = shell.getDisplay().getSystemColor(
                                SWT.COLOR_RED);
                    }
                    stateLbl.setBackground(bgColor);
                }
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        if (generateJob != null && generateJob.getState() != Job.NONE) {
            generateJob.cancel();
        }
    }

    /**
     * The performs the work of generating the case on a non-UI thread.
     */
    private class GenerateJob extends Job {
        boolean shutdown = false;

        public GenerateJob() {
            super("Generate Job");
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            if (monitor.isCanceled()) {
                return Status.OK_STATUS;
            }

            setStateLbl("Creating: " + caseDir.getName(),
                    caseDir.getAbsolutePath());
            if (caseDir.exists()) {
                setStateLbl("Case exists: " + caseName,
                        caseDir.getAbsolutePath());
                setProgressBar(100, SWT.ERROR);
                return Status.OK_STATUS;
            }

            if (!caseDir.mkdirs()) {
                setStateLbl("Unable to create case: " + caseName,
                        caseDir.getAbsolutePath());
                setProgressBar(100, SWT.ERROR);
                return Status.OK_STATUS;
            }

            if (shutdown) {
                return Status.OK_STATUS;
            }

            for (DisplayData displayData : sourceDataList) {
                if (shutdown) {
                    return Status.OK_STATUS;
                }

                setStateLbl(
                        "Copying \"" + displayData.getDisplayLabel() + "\"",
                        null);
                String rootDir = displayData.getRootDir();
                int beginIndex = rootDir.length();

                List<File> files = archiveManager.getDisplayFiles(displayData,
                        startCal, endCal);
                for (File source : files) {
                    if (shutdown) {
                        return Status.OK_STATUS;
                    }

                    String name = source.getAbsolutePath();
                    String relativePath = name.substring(beginIndex);

                    setStateLbl("Copy: " + relativePath, name);
                    File destination = new File(caseDir, relativePath);
                    try {
                        if (source.isDirectory()) {
                            destination.mkdirs();
                            FileUtil.copyDirectory(source, destination);
                        } else {
                            File destParent = caseDir.getParentFile();
                            destParent.mkdirs();
                            FileUtil.copyFile(source, destination);
                        }
                    } catch (IOException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
            }

            if (!doCompress) {
                setProgressBar(100, SWT.NORMAL);
                setStateLbl("Created: " + caseName, caseDir.getAbsolutePath());
                return Status.OK_STATUS;
            }

            if (shutdown) {
                return Status.OK_STATUS;
            }

            if (doMultiFiles) {
                setProgressBar(100, SWT.NORMAL);
                String message = "Compress into multifiles NYI.";
                setStateLbl(message, null);
            } else {
                setProgressBar(100, SWT.NORMAL);
                String message = "Compress into one file NYI";
                setStateLbl(message, null);
            }

            return Status.OK_STATUS;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.core.runtime.jobs.Job#canceling()
         */
        @Override
        protected void canceling() {
            shutdown = true;
            generateJob = null;
        }
    }
}
