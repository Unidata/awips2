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

import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.zip.GZIPOutputStream;

import org.apache.commons.compress.archivers.ArchiveException;
import org.apache.commons.compress.archivers.ArchiveOutputStream;
import org.apache.commons.compress.archivers.ArchiveStreamFactory;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.commons.io.FileUtils;
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
import com.raytheon.uf.common.archive.config.ArchiveConstants;
import com.raytheon.uf.common.archive.config.DisplayData;
import com.raytheon.uf.common.dataquery.requests.SharedLockRequest;
import com.raytheon.uf.common.dataquery.requests.SharedLockRequest.RequestType;
import com.raytheon.uf.common.dataquery.responses.SharedLockResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class performs the desired type of case creation and displays a
 * progress/status message dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 3, 2013  1966       rferrel     Initial creation
 * Aug 16, 2013 2225       rferrel     Change structure of copy to include
 *                                     archive and category directory and 
 *                                     implementation of compression.
 * Oct 08, 2013 2442       rferrel     Remove category directory.
 * Feb 04, 2014 2270       rferrel     Move HDF files to parent's directory.
 * Mar 26, 2014 2880       rferrel     Compress and split cases implemented.
 * Apr 03, 2014 2862       rferrel     Logic for shared locking of top level directories.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class GenerateCaseDlg extends CaveSWTDialog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GenerateCaseDlg.class);

    /** Extension for HDF files. */
    private static final String hdfExt = ".h5";

    /** Use to display the current state of the case generation. */
    private Label stateLbl;

    /** Progress bar to indicate activity. */
    private ProgressBar progressBar;

    /** Stops the case generation leaving it in an unknown state. */
    private Button cancelBtn;

    /** Active have generation is finish or has an error. */
    private Button closeBtn;

    /**
     * The case's destination directory. Assumed to be a sub-directory of
     * targetDir.
     */
    private final File caseDir;

    /** Starting time for the case. */
    private final Calendar startCal;

    /** End time for the case. */
    private final Calendar endCal;

    /** Data list for the case sorted by archive and category names. */
    private final DisplayData[] sourceDataList;

    /** When true compress the case directory. */
    private final boolean doCompress;

    /** When true break the compress file into multiple files. */
    private final boolean doMultiFiles;

    /** The compress size for multiple files. */
    private final long splitSize;

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
     * @param splitSize
     */
    protected GenerateCaseDlg(Shell parentShell, File targetDir, File caseDir,
            Calendar startCal, Calendar endCal, List<DisplayData> sourceList,
            boolean doCompress, boolean doMultiFiles, long splitSize) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK
                | CAVE.PERSPECTIVE_INDEPENDENT);
        this.caseDir = caseDir;
        this.startCal = startCal;
        this.endCal = endCal;
        this.sourceDataList = sourceList.toArray(new DisplayData[sourceList
                .size()]);
        Arrays.sort(this.sourceDataList, DisplayData.LABEL_ORDER);
        this.doCompress = doCompress;
        this.doMultiFiles = doMultiFiles;

        this.splitSize = splitSize;
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
     * Remove a job listener.
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
     * This performs the work of generating the case on a non-UI thread.
     */
    private class GenerateJob extends Job {
        /** Parent flag to shutdown the job. */
        private final AtomicBoolean shutdown = new AtomicBoolean(false);

        /** How long to wait before making another request for a plug-in lock. */
        private final long LOCK_RETRY_TIME = 2 * TimeUtil.MILLIS_PER_MINUTE;

        /** Files/directories needing plug-in locks in order to copy. */
        private final Map<CopyInfo, Map<String, List<File>>> caseCopyMap = new HashMap<CopyInfo, Map<String, List<File>>>();

        /** Timer to determine when to send another request for a plug-in lock. */
        private final ITimer retrytimer = TimeUtil.getTimer();

        /** Timer to update current lock's last execute time. */
        private Timer updateTimer = null;

        /**
         * Constructor.
         */
        public GenerateJob() {
            super("Generate Case");
        }

        /**
         * Add file to the caseCopyMap.
         * 
         * @param copyInfo
         * @param plugin
         * @param file
         * @return
         */
        private boolean putFile(CopyInfo copyInfo, String plugin, File file) {
            if (caseCopyMap.size() == 0) {
                retrytimer.start();
            }

            Map<String, List<File>> pluginMap = caseCopyMap.get(copyInfo);

            if (pluginMap == null) {
                pluginMap = new HashMap<String, List<File>>();
                caseCopyMap.put(copyInfo, pluginMap);
            }

            List<File> files = pluginMap.get(plugin);

            if (files == null) {
                files = new ArrayList<File>();
                pluginMap.put(plugin, files);
            }
            return files.add(file);
        }

        /**
         * @param copyInfo
         * @return true if locks needed to complete the copy.
         */
        private boolean keepCaseCopy(CopyInfo copyInfo) {
            return caseCopyMap.get(copyInfo) != null;
        }

        /**
         * @return true when valid case directory.
         */
        private boolean validateCaseDirectory() {
            setStateLbl("Creating: " + caseDir.getName(),
                    caseDir.getAbsolutePath());

            String errorMessage = null;
            if (caseDir.exists()) {
                errorMessage = "Case exists: " + caseDir.getName();
            } else if (!caseDir.mkdirs()) {
                errorMessage = "Unable to create case: " + caseDir.getName();
            }

            if (errorMessage != null) {
                setStateLbl(errorMessage, caseDir.getAbsolutePath());
                setProgressBar(100, SWT.ERROR);
                return false;
            }

            if (shutdown.get()) {
                return false;
            }
            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.
         * IProgressMonitor)
         */
        @Override
        protected IStatus run(IProgressMonitor monitor) {
            if (monitor.isCanceled()) {
                return Status.OK_STATUS;
            }

            if (!validateCaseDirectory()) {
                return Status.OK_STATUS;
            }

            ICaseCopy caseCopy = null;
            String currentArchive = null;
            String currentCategory = null;
            int rootDirLen = -1;
            File rootDir = null;
            String plugin = null;
            boolean allowCopy = true;
            CopyInfo copyInfo = null;

            ITimer timer = TimeUtil.getTimer();
            timer.start();

            try {
                /*
                 * The sourceDataList is sorted so all the displayDatas for a
                 * given archive/category are grouped together in the loop.
                 */
                for (DisplayData displayData : sourceDataList) {
                    if (shutdown.get()) {
                        return Status.OK_STATUS;
                    }

                    /*
                     * The current display data is for a different
                     * archive/category then the previous one.
                     */
                    if (!displayData.getArchiveName().equals(currentArchive)
                            || !displayData.getCategoryName().equals(
                                    currentCategory)) {

                        // Finish up previous archive/category.
                        if (caseCopy != null) {
                            if (allowCopy) {
                                releaseLock(plugin);
                            }

                            /*
                             * The copyInfo needs locks in order to finish.
                             * Force creation of a new caseCopy for the new
                             * category.
                             */
                            if (keepCaseCopy(copyInfo)) {
                                caseCopy = null;
                                copyInfo = null;
                            } else {
                                caseCopy.finishCase();
                            }
                            plugin = null;
                        }

                        // Set up for new category.
                        currentArchive = displayData.getArchiveName();
                        currentCategory = displayData.getCategoryName();
                        rootDir = new File(displayData.getRootDir());
                        rootDirLen = displayData.getRootDir().length();
                        allowCopy = true;

                        setStateLbl(currentArchive + " | " + currentCategory,
                                caseDir.getAbsolutePath() + "\n"
                                        + currentArchive + "\n"
                                        + currentCategory);

                        /*
                         * When caseCopy is not null it is safe to reuse it for
                         * the new category.
                         */
                        if (caseCopy == null) {
                            if (!doCompress) {
                                caseCopy = new CopyMove();
                            } else if (doMultiFiles) {
                                caseCopy = new CompressAndSplitCopy(splitSize);
                            } else {
                                caseCopy = new CompressCopy();
                            }
                            copyInfo = new CopyInfo(caseCopy, currentArchive,
                                    currentCategory, caseDir);
                        }

                        caseCopy.startCase(caseDir, displayData, shutdown);
                    }

                    List<File> files = archiveManager.getDisplayFiles(
                            displayData, startCal, endCal);

                    /*
                     * Check all files/directories in the displayData and
                     * attempt a recursive copy.
                     */
                    for (File source : files) {
                        if (shutdown.get()) {
                            return Status.OK_STATUS;
                        }

                        String dirName = source.getAbsolutePath()
                                .substring(rootDirLen).split(File.separator)[0];
                        String newPlugin = (new File(rootDir, dirName))
                                .getAbsolutePath();

                        // Have new plugin.
                        if (!newPlugin.equals(plugin)) {
                            // Release the current lock.
                            if (allowCopy && (plugin != null)) {
                                releaseLock(plugin);
                            }
                            allowCopy = requestLock(newPlugin);
                            plugin = newPlugin;
                        }

                        if (allowCopy) {
                            // Have lock safe to perform recursive copy.
                            caseCopy.copy(source);
                        } else {
                            // No lock add to Map of files needing locks.
                            putFile(copyInfo, plugin, source);
                        }
                    } // End of files loop

                    /*
                     * The copy may have taken some time see if any pending
                     * copies can be completed.
                     */
                    if (retrytimer.getElapsedTime() >= LOCK_RETRY_TIME) {
                        retryCasesCopy();
                    }
                } // End of sourceDataList loop

                // Finish up the loop's last plugin.
                if (plugin != null) {
                    if (allowCopy) {
                        releaseLock(plugin);
                    }

                    // Finish last case
                    if (!keepCaseCopy(copyInfo)) {
                        caseCopy.finishCase();
                    }
                    plugin = null;
                }
                caseCopy = null;

                // Finish pending copies needing locks.
                waitForLocks();

                if (shutdown.get()) {
                    return Status.OK_STATUS;
                }

                setStateLbl("Created: " + caseName, caseDir.getAbsolutePath());
                setProgressBar(100, SWT.NORMAL);

            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                setStateLbl(
                        "Failed to Create: " + caseName,
                        caseDir.getAbsolutePath() + "\n"
                                + e.getLocalizedMessage());
                setProgressBar(100, SWT.ERROR);
                shutdown.set(true);
            } finally {
                // shutdown the time.
                if (updateTimer != null) {
                    updateTimer.cancel();
                    updateTimer = null;
                }

                // Release resources of active case copy.
                if (caseCopy != null) {
                    if (!keepCaseCopy(copyInfo)) {
                        try {
                            // Allow the caseCopy to clean its resources.
                            caseCopy.finishCase();
                    } catch (Exception ex) {
                            // Ignore
                        }
                        caseCopy = null;
                    }
                timer.stop();
                if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                    String message = String.format("Case %s took %s.",
                            caseDir.getName(),
                            TimeUtil.prettyDuration(timer.getElapsedTime()));
                    statusHandler.handle(Priority.INFO, message);
                }
                }

                // Release current lock.
                if (allowCopy && (plugin != null)) {
                    releaseLock(plugin);
                }

                // Release resources of any pending case copy.
                if (caseCopyMap.size() > 0) {
                    for (CopyInfo cpi : caseCopyMap.keySet()) {
                        try {
                            cpi.caseCopy.finishCase();
                        } catch (CaseCreateException ex) {
                            // Ignore
                        }
                    }
                    caseCopyMap.clear();
                }
            }

            return Status.OK_STATUS;
        }

        /**
         * Finish copying all files needing locks.
         * 
         * @throws CaseCreateException
         * @throws InterruptedException
         */
        private void waitForLocks() throws CaseCreateException,
                InterruptedException {
            int retryCount = 0;
            boolean updateStatus = true;
            while (caseCopyMap.size() > 0) {
                if (updateStatus) {
                    ++retryCount;
                    StringBuilder tooltip = new StringBuilder();
                    tooltip.append("Waiting to finish ").append(
                            caseCopyMap.size());
                    if (caseCopyMap.size() == 1) {
                        tooltip.append(" category.");
                    } else {
                        tooltip.append(" categories.");
                    }
                    tooltip.append("\nAttempt: ").append(retryCount);
                    setStateLbl("Waiting for locks", tooltip.toString());
                    updateStatus = false;
                }

                synchronized (this) {
                    wait(TimeUtil.MILLIS_PER_SECOND / 2L);
                }

                if (shutdown.get()) {
                    return;
                }

                if (retrytimer.getElapsedTime() >= LOCK_RETRY_TIME) {
                    retryCasesCopy();
                    updateStatus = true;
                }
            }
        }

        /**
         * Attempt to copy files still waiting on plug-in locks.
         * 
         * @throws CaseCreateException
         */
        private void retryCasesCopy() throws CaseCreateException {
            if (shutdown.get()) {
                return;
            }

            if (caseCopyMap.size() > 0) {
                retrytimer.stop();
                retrytimer.reset();
                String lockedPlugin = null;
                try {
                    Iterator<CopyInfo> copyInfoIter = caseCopyMap.keySet()
                            .iterator();
                    while (copyInfoIter.hasNext()) {
                        CopyInfo copyInfo = copyInfoIter.next();
                        setStateLbl(copyInfo.archive + " | "
                                + copyInfo.category,
                                copyInfo.caseDir.getAbsolutePath() + "\n"
                                        + copyInfo.archive + "\n"
                                        + copyInfo.category);

                        Map<String, List<File>> pluginMap = caseCopyMap
                                .get(copyInfo);
                        Iterator<String> pluginIter = pluginMap.keySet()
                                .iterator();
                        while (pluginIter.hasNext()) {
                            String plugin = pluginIter.next();
                            if (shutdown.get()) {
                                return;
                            }

                            if (requestLock(plugin)) {
                                lockedPlugin = plugin;
                                for (File source : pluginMap.get(plugin)) {
                                    copyInfo.caseCopy.copy(source);
                                    if (shutdown.get()) {
                                        return;
                                    }
                                }
                                releaseLock(plugin);
                                lockedPlugin = null;
                                pluginIter.remove();
                            }
                        }
                        if (pluginMap.size() == 0) {
                            copyInfo.caseCopy.finishCase();
                            copyInfoIter.remove();
                        }
                    }
                } finally {
                    if (lockedPlugin != null) {
                        releaseLock(lockedPlugin);
                    }
                }
            }

            if (caseCopyMap.size() > 0) {
                retrytimer.start();
            }
        }

        /**
         * Request a lock for the plug-in.
         * 
         * @param details
         * @return true when lock obtained otherwise false
         */
        private boolean requestLock(String details) {
            SharedLockRequest request = new SharedLockRequest(
                    ArchiveConstants.CLUSTER_NAME, details,
                    RequestType.READER_LOCK);

            try {
                Object o = ThriftClient.sendRequest(request);
                if (o instanceof SharedLockResponse) {
                    SharedLockResponse response = (SharedLockResponse) o;
                    if (response.isSucessful()) {
                        if (updateTimer == null) {
                            updateTimer = new Timer(
                                    "Case Creation update timer", true);
                        }
                        TimerTask timerTask = new LockUpdateTask(details);
                        updateTimer.schedule(timerTask,
                                TimeUtil.MILLIS_PER_MINUTE,
                                TimeUtil.MILLIS_PER_MINUTE);
                        return true;
                    }
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }

            return false;
        }

        /**
         * Release previously obtained lock for the details.
         * 
         * @param details
         * @return true when lock released otherwise false.
         */
        private boolean releaseLock(String details) {
            SharedLockRequest request = new SharedLockRequest(
                    ArchiveConstants.CLUSTER_NAME, details,
                    RequestType.READER_UNLOCK);
            try {
                if (updateTimer != null) {
                    updateTimer.cancel();
                    updateTimer = null;
                }
                Object o = ThriftClient.sendRequest(request);
                if (o instanceof SharedLockResponse) {
                    SharedLockResponse response = (SharedLockResponse) o;
                    if (response.isSucessful()) {
                        details = null;
                        return true;
                    }
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }

            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.core.runtime.jobs.Job#canceling()
         */
        @Override
        protected void canceling() {
            shutdown.set(true);
            generateJob = null;
        }
    }

    /**
     * This class copies selected files/directories to a case-directory/archive.
     */
    private static class CopyMove implements ICaseCopy {
        private final IUFStatusHandler statusHandler;

        /**
         * Flag to indicate user canceled the case generation.
         */
        private AtomicBoolean shutdown;

        /**
         * Top destination directory to move files/dirctories to.
         */
        private File destDir;

        /**
         * Index on source Files where relative path starts.
         */
        private int startRelativePath;

        /**
         * Constructor.
         */
        public CopyMove() {
            statusHandler = UFStatus.getHandler(this.getClass());
        }

        /**
         * Copy source File to desired destination.
         * 
         * @param source
         * @param destination
         * @throws IOException
         */
        private void copyFile(File source, File destination) throws IOException {
            if (shutdown.get()) {
                return;
            }

            if (!source.exists()) {
                if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    String message = String.format(
                            "Purged and unable to place in case: %s",
                            source.getAbsoluteFile());
                    statusHandler.handle(Priority.DEBUG, message);
                }
                return;
            }

            if (source.isDirectory()) {

                if (!destination.exists()) {
                    destination.mkdir();
                }

                String[] files = source.list();

                for (String file : files) {
                    copyFile(new File(source, file),
                            new File(destination, file));
                }
            } else if (source.exists()) {
                // DR 2270 bump HDF files up a directory.
                if (destination.getName().endsWith(hdfExt)) {
                    destination = new File(destination.getParentFile()
                            .getParentFile(), destination.getName());
                }
                FileUtil.copyFile(source, destination);
                destination.setLastModified(source.lastModified());
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.archive.ui.ICaseCopy#copy(java.io.File)
         */
        @Override
        public void copy(File source) throws CaseCreateException {
            String relativePath = source.getAbsolutePath().substring(
                    startRelativePath);
            File destination = new File(destDir, relativePath);
            try {
                destination.getParentFile().mkdirs();
                copyFile(source, destination);
            } catch (IOException ex) {
                throw new CaseCreateException("Copy Move ", ex);
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.archive.ui.ICaseCopy#startCase(java.io.File,
         * com.raytheon.uf.common.archive.config.DisplayData,
         * java.util.concurrent.atomic.AtomicBoolean)
         */
        @Override
        public void startCase(File caseDir, DisplayData displayData,
                AtomicBoolean shutdown) {
            this.shutdown = shutdown;
            String archiveDirName = ArchiveConstants
                    .convertToFileName(displayData.getArchiveName());
            destDir = new File(caseDir, archiveDirName);
            destDir.mkdirs();
            startRelativePath = displayData.getRootDir().length();
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.archive.ui.ICaseCopy#finishCase()
         */
        @Override
        public void finishCase() {
            // Nothing to do.
        }
    }

    /**
     * This class takes selected directories/files to
     * case-directory/archive/compress-category-file. The compress-category-file
     * is a tar gzip file containing the category's data.
     */
    private static class CompressCopy implements ICaseCopy {
        private final IUFStatusHandler statusHandler;

        /**
         * Flag to indicate user canceled case generation.
         */
        protected AtomicBoolean shutdown;

        /**
         * Top Level destination directory.
         */
        protected File destDir;

        /**
         * Stream to the file being created.
         */
        protected FileOutputStream fileStream;

        /**
         * Stream to perform the compression.
         */
        protected GZIPOutputStream zipStream;

        /**
         * Stream to create the tar image.
         */
        protected ArchiveOutputStream tarStream;

        /**
         * The category directory name used to generate tar file name(s).
         */
        protected String categoryDirName;

        /**
         * Index to start of relative path in source File.
         */
        protected int startRelativePath;

        /**
         * Directories already created in the tar image.
         */
        protected final HashSet<File> tarDirFile = new HashSet<File>();

        /**
         * Buffer to use for reading in a file.
         */
        protected final byte[] buffer = new byte[(int) (32 * FileUtils.ONE_KB)];

        /**
         * Current tar file being created.
         */
        protected File tarFile;

        /**
         * Constructor.
         */
        public CompressCopy() {
            this.statusHandler = UFStatus.getHandler(this.getClass());
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.archive.ui.ICaseCopy#copy(java.io.File)
         */
        @Override
        public void copy(File source) throws CaseCreateException {
            try {
                addParentDir(source);
                addTarFiles(new File[] { source });
            } catch (Exception e) {
                throw new CaseCreateException("Compress Copy failed: ", e);
            }
        }

        /**
         * Add list of Files to the tar image.
         * 
         * @param files
         * @throws IOException
         * @throws ArchiveException
         * @throws CaseCreateException
         */
        private void addTarFiles(File[] files) throws IOException,
                ArchiveException {
            for (File file : files) {
                if (shutdown.get()) {
                    return;
                }
                String name = file.getAbsolutePath().substring(
                        startRelativePath);
                if (!file.exists()) {
                    if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                        String message = String.format(
                                "Purged and unable to place in case: %s",
                                file.getAbsoluteFile());
                        statusHandler.handle(Priority.DEBUG, message);
                    }
                    continue;
                }
                if (file.isDirectory()) {
                    if (!tarDirFile.contains(file)) {
                        TarArchiveEntry entry = new TarArchiveEntry(file, name);
                        tarStream.putArchiveEntry(entry);
                        tarStream.closeArchiveEntry();
                        tarDirFile.add(file);
                        addTarFiles(file.listFiles());
                    }
                } else if (file.exists()) {
                    checkFit(file);
                    // DR 2270 bump HDF files up a directory.
                    if (name.endsWith(hdfExt)) {
                        File destination = new File(file.getParentFile()
                                .getParentFile(), file.getName());
                        name = destination.getAbsolutePath().substring(
                                startRelativePath);
                    }
                    TarArchiveEntry entry = new TarArchiveEntry(file, name);
                    entry.setSize(file.length());
                    FileInputStream fileStream = null;
                    tarStream.putArchiveEntry(entry);
                    try {
                        fileStream = new FileInputStream(file);
                        int len;
                        while ((len = fileStream.read(buffer)) != -1) {
                            tarStream.write(buffer, 0, len);
                        }
                    } finally {
                        if (fileStream != null) {
                            closeStream(fileStream);
                        }
                    }

                    tarStream.closeArchiveEntry();
                }
            }
        }

        /**
         * Convince method to close a steam and ignore any IOException.
         * 
         * @param stream
         */
        protected void closeStream(Closeable stream) {
            try {
                stream.close();
            } catch (IOException ex) {
                // Ignore
            }
        }

        /**
         * Allows sub-class to check to see if file will fit in the current tar
         * file and if needed setup new tar file.
         */
        protected void checkFit(File file) throws IOException, ArchiveException {
            // Do not change the tar file.
        }

        /**
         * If needed add parent directories to the tar image.
         * 
         * @param file
         * @throws IOException
         */
        protected void addParentDir(File file) throws IOException {
            File parent = file.getParentFile();
            if (parent != null && !tarDirFile.contains(parent)
                    && (parent.getAbsolutePath().length() > startRelativePath)) {
                addParentDir(parent);
                String name = parent.getAbsolutePath().substring(
                        startRelativePath);
                TarArchiveEntry entry = new TarArchiveEntry(parent, name);
                tarStream.putArchiveEntry(entry);
                tarStream.closeArchiveEntry();
                tarDirFile.add(parent);
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.archive.ui.ICaseCopy#startCase(java.io.File,
         * com.raytheon.uf.common.archive.config.DisplayData,
         * java.util.concurrent.atomic.AtomicBoolean)
         */
        @Override
        public void startCase(File caseDir, DisplayData displayData,
                AtomicBoolean shutdown) throws CaseCreateException {
            try {
                this.shutdown = shutdown;
                String archiveDirName = ArchiveConstants
                        .convertToFileName(displayData.getArchiveName());
                categoryDirName = ArchiveConstants
                        .convertToFileName(displayData.getCategoryName());
                destDir = new File(caseDir, archiveDirName);
                destDir.mkdirs();
                startRelativePath = displayData.getRootDir().length();
                openStreams();
            } catch (Exception e) {
                throw new CaseCreateException("Compress Copy start case: ", e);
            }
        }

        /**
         * Determine a new tar file and set up its streams.
         * 
         * @throws IOException
         * @throws ArchiveException
         */
        protected void openStreams() throws IOException, ArchiveException {
            tarDirFile.clear();
            tarFile = getTarFile();
            fileStream = new FileOutputStream(tarFile);
            zipStream = new GZIPOutputStream(fileStream);
            ArchiveStreamFactory factory = new ArchiveStreamFactory();
            tarStream = factory.createArchiveOutputStream(
                    ArchiveStreamFactory.TAR, zipStream);
            if (tarStream instanceof TarArchiveOutputStream) {
                ((TarArchiveOutputStream) tarStream)
                        .setLongFileMode(TarArchiveOutputStream.LONGFILE_GNU);
            }
        }

        /**
         * Determine new tar file.
         * 
         * @return tarFile
         */
        protected File getTarFile() {
            return new File(destDir, categoryDirName
                    + ArchiveConstants.TAR_EXTENSION);
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.viz.archive.ui.ICaseCopy#finishCase()
         */
        @Override
        public void finishCase() throws CaseCreateException {
            try {
                closeStreams();
            } catch (IOException e) {
                throw new CaseCreateException("Compress Copy finish: ", e);
            }
        }

        /**
         * Close all the streams for current tar file.
         * 
         * @throws IOException
         */
        protected void closeStreams() throws IOException {
            try {
                if (tarStream != null) {
                    tarStream.finish();
                }
                if (zipStream != null) {
                    zipStream.finish();
                }
            } finally {
                if (tarStream != null) {
                    closeStream(tarStream);
                } else if (zipStream != null) {
                    closeStream(zipStream);
                } else if (fileStream != null) {
                    closeStream(fileStream);
                }
                tarStream = null;
                zipStream = null;
                fileStream = null;
            }
        }
    }

    /*
     * This class intended for making "image" files read for burning to a CD or
     * DVD.
     */
    private static class CompressAndSplitCopy extends CompressCopy {
        /**
         * Number of bytes to back off the split limit to allow finishing the
         * tar without exceeding the limit.
         */
        private final long BACK_OFF_BYTES = 5 * FileUtils.ONE_KB;

        /**
         * Maximum bytes for a tar file.
         */
        private final long splitSize;

        /**
         * Count of tar files for a category.
         */
        private int fileCnt = 0;

        /**
         * Constructor.
         * 
         * @param splitSize
         */
        public CompressAndSplitCopy(long splitSize) {
            super();
            this.splitSize = splitSize - BACK_OFF_BYTES;
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.viz.archive.ui.GenerateCaseDlg.CompressCopy#startCase
         * (java.io.File, com.raytheon.uf.common.archive.config.DisplayData,
         * java.util.concurrent.atomic.AtomicBoolean)
         */
        @Override
        public void startCase(File caseDir, DisplayData displayData,
                AtomicBoolean shutdown) throws CaseCreateException {
            this.fileCnt = 0;
            super.startCase(caseDir, displayData, shutdown);
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.viz.archive.ui.GenerateCaseDlg.CompressCopy#getTarFile
         * ()
         */
        @Override
        protected File getTarFile() {
            int cnt = ++fileCnt;
            String name = String.format("%s_%03d%s", categoryDirName, cnt,
                    ArchiveConstants.TAR_EXTENSION);
            return new File(destDir, name);
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.viz.archive.ui.GenerateCaseDlg.CompressCopy#checkFit
         * (java.io.File)
         */
        @Override
        protected void checkFit(File file) throws IOException, ArchiveException {
            // force update of tarFile length.
            tarStream.flush();
            zipStream.flush();
            fileStream.flush();

            /*
             * Most likely over estimates the size since it is unknown how well
             * file will compress.
             */
            long size = tarFile.length() + file.length();
            if (size >= splitSize) {
                closeStreams();
                openStreams();
                addParentDir(file);
            }
        }
    }

    /** Task to update the lock plugin's last execute time. */
    private final class LockUpdateTask extends TimerTask {
        /** The locked cluster task's details. */
        private final String details;

        public LockUpdateTask(String details) {
            this.details = details;
        }

        @Override
        public void run() {
            SharedLockRequest request = new SharedLockRequest(
                    ArchiveConstants.CLUSTER_NAME, details,
                    RequestType.READER_UPDATE_TIME);
            try {
                ThriftClient.sendRequest(request);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /** Information needed to update status when retrying a copy. */
    private static class CopyInfo {
        protected final ICaseCopy caseCopy;

        protected final String archive;

        protected final String category;

        protected final File caseDir;

        public CopyInfo(ICaseCopy caseCopy, String archive, String category,
                File caseDir) {
            this.caseCopy = caseCopy;
            this.archive = archive;
            this.category = category;
            this.caseDir = caseDir;
        }
    }
}
