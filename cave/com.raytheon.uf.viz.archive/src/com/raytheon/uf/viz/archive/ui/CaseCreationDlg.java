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
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.archive.config.ArchiveConstants.Type;
import com.raytheon.uf.common.archive.config.DisplayData;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.SizeUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.widgets.DateTimeEntry;

/**
 *
 * Case creation dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * May 23, 2013  1964     lvenable  Initial creation
 * Jun 10, 2013  1966     rferrel   Implemented back in hooks for display and
 *                                  generation of cases.
 * Jul 24, 2013  2220     rferrel   Add recompute size button.
 * Jul 24, 2013  2221     rferrel   Changes for select configuration.
 * Aug 06, 2013  2222     rferrel   Changes to display all selected data.
 * Aug 26, 2013  2225     rferrel   Make perspective independent and no longer
 *                                  modal.
 * Mar 24, 2014  2853     rferrel   Populate case label directory with default
 *                                  value.
 * Mar 26, 2014  32880    rferrel   Implement case compression and split.
 * Apr 11, 2014  3023     rferrel   Configurable Threshold options.
 * Apr 23, 2014  3045     rferrel   To prevent race condition only allow a case
 *                                  load after all labels are loaded.
 * Aug 26, 2014  3553     rferrel   loadedAllDisplayData must now call its
 *                                  super.
 * Mar 01, 2016  3989     tgurney   Rename AwipsCalendar to CalendarDialog
 * Dec 18, 2018  7677     randerso  Updated remaining disk space indication to
 *                                  remove FATAL wording that was confusing
 *                                  users. Code cleanup.
 *
 * </pre>
 *
 * @author lvenable
 */
public class CaseCreationDlg extends AbstractArchiveDlg {
    private static final String DATE_FORMAT = "E MMM dd yyyy HH:00 z";

    /** The case creation label's default directory. */
    private final String defaultCaseDir;

    /** Start time entry. */
    private DateTimeEntry startTimeEntry;

    /** End time entry. */
    private DateTimeEntry endTimeEntry;

    /** Start date. */
    private Date startDate;

    /** End date. */
    private Date endDate;

    /** Display the case name. */
    private Text caseNameText;

    /** Compression check box. */
    private Button compressChk;

    /** Break files check box. */
    private Button breakFilesChk;

    /** Button to save new select case configuration. */
    private Button saveAsBtn;

    /** Button to load select case configuration. */
    private Button loadBtn;

    /** File size spinner control. */
    private Spinner fileSizeSpnr;

    /** File size combo box. */
    private Combo fileSizeCbo;

    /** Maximum file size label. */
    private Label maxFileSizeLbl;

    /** Directory location label. */
    private Label locationLbl;

    /** Directory location state. */
    private Label diskSpaceIndicator;

    /** Uncompressed file size label. */
    private Label uncompressSizeLbl;

    /** Displays total number of items selected from all categories. */
    private Label totalSelectedItemsLbl;

    /** Button to generate the case. */
    private Button generateBtn;

    /** Number of selected items. */
    private int selectedItemsSize = 0;

    /** Dialog to create new select case. */
    private CaseLoadSaveDeleteDlg saveAsDlg;

    /** Dialog to load a select case. */
    private CaseLoadSaveDeleteDlg loadDlg;

    /** Dialog to delete a select case. */
    private CaseLoadSaveDeleteDlg deleteDlg;

    /** Allow only single instance of dialog. */
    private GenerateCaseDlg generateCaseDlg;

    /** Manager for configurable values for the dialog. */
    private final CaseCreationManager ccManager;

    private long availableSpace = -1;

    /**
     * Constructor.
     *
     * @param parentShell
     *            Parent shell.
     * @param defaultCaseDir
     *            the default case directory
     */
    public CaseCreationDlg(Shell parentShell, String defaultCaseDir) {
        super(parentShell, SWT.SHELL_TRIM,
                CAVE.DO_NOT_BLOCK | CAVE.PERSPECTIVE_INDEPENDENT
                        | CAVE.MODE_INDEPENDENT | CAVE.INDEPENDENT_SHELL);
        this.type = Type.Case;
        this.setSelect = false;
        this.type = Type.Case;
        this.defaultCaseDir = defaultCaseDir;
        this.ccManager = new CaseCreationManager();
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;

        return mainLayout;
    }

    @Override
    protected void init(Shell shell) {
        setText("Archive Case Creation -");
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        gl.horizontalSpacing = 0;
        mainComp.setLayout(gl);

        createTimeControls();

        GuiUtil.addSeparator(shell, SWT.HORIZONTAL);
        createCaseCompressionControls();

        GuiUtil.addSeparator(shell, SWT.HORIZONTAL);
        createFileBrowserControls();

        createTable();

        GuiUtil.addSeparator(shell, SWT.HORIZONTAL);
        createBottomActionButtons();
    }

    /**
     * Create the time controls.
     */
    private void createTimeControls() {
        Composite timeComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 20;
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        timeComp.setLayout(gl);
        timeComp.setLayoutData(gd);

        Composite startComp = new Composite(timeComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        startComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        startComp.setLayoutData(gd);

        Label label = new Label(startComp, SWT.NONE);
        label.setText("Start Time:");
        startTimeEntry = new DateTimeEntry(startComp, DATE_FORMAT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        startTimeEntry.setLayoutData(gd);
        startTimeEntry.addUpdateListener(new DateTimeEntry.IUpdateListener() {

            @Override
            public void dateTimeUpdated(Date date) {
                startDateUpdated(date);
            }
        });

        Composite endComp = new Composite(timeComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.marginWidth = 0;
        gl.marginHeight = 0;
        endComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        endComp.setLayoutData(gd);

        label = new Label(endComp, SWT.NONE);
        label.setText("End Time:");
        endTimeEntry = new DateTimeEntry(endComp, DATE_FORMAT);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        endTimeEntry.setLayoutData(gd);
        endTimeEntry.addUpdateListener(new DateTimeEntry.IUpdateListener() {

            @Override
            public void dateTimeUpdated(Date date) {
                endDateUpdated(date);
            }
        });
    }

    @Override
    public void setRetentionTimes(long startRetentionHours) {
        long startTimeOffset = startRetentionHours * TimeUtil.MILLIS_PER_HOUR;
        endDate = TimeUtil.newDate();
        long time = endDate.getTime();
        time -= startTimeOffset;
        startDate = new Date(time);
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                startTimeEntry.setDate(startDate);
                endTimeEntry.setDate(endDate);
            }
        });
    }

    /**
     * Create the case compression controls.
     */
    private void createCaseCompressionControls() {
        Composite caseCompressionComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        caseCompressionComp.setLayout(gl);
        caseCompressionComp.setLayoutData(gd);

        createComboControls(caseCompressionComp);
        GuiUtil.addSeparator(caseCompressionComp, SWT.VERTICAL);
        createCompressionControls(caseCompressionComp);
    }

    @Override
    protected Composite createComboControls(Composite comp) {
        Composite comboComp = super.createComboControls(comp);

        Label caseNameCreate = new Label(comboComp, SWT.NONE);
        caseNameCreate.setText("Case Name:");

        caseNameText = new Text(comboComp, SWT.BORDER);
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        caseNameText.setLayoutData(gd);
        caseNameText.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
                checkGenerateButton();
            }
        });

        return comboComp;
    }

    /**
     * Create the compression controls.
     *
     * @param comp
     *            Composite to put the control in.
     */
    private void createCompressionControls(Composite comp) {
        Composite compressionComp = new Composite(comp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        compressionComp.setLayout(gl);
        compressionComp.setLayoutData(gd);

        /*
         * Uncompressed file size label
         */
        Composite compressionLblComp = new Composite(compressionComp, SWT.NONE);
        gl = new GridLayout(2, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        compressionLblComp.setLayout(gl);
        compressionLblComp.setLayoutData(gd);

        Label uncompressLbl = new Label(compressionLblComp, SWT.NONE);
        uncompressLbl.setText("Uncompressed Case Size:");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        uncompressSizeLbl = new Label(compressionLblComp, SWT.NONE);
        uncompressSizeLbl.setLayoutData(gd);

        Label totSelectedLbl = new Label(compressionLblComp, SWT.NONE);
        totSelectedLbl.setText("Total Selected Items:");

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        totalSelectedItemsLbl = new Label(compressionLblComp, SWT.NONE);
        totalSelectedItemsLbl.setLayoutData(gd);

        /*
         * Compression controls
         */
        compressChk = new Button(compressionComp, SWT.CHECK);
        compressChk.setText("Compress Files");
        compressChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleCompressSelection();
            }
        });

        gd = new GridData();
        gd.horizontalIndent = 20;
        breakFilesChk = new Button(compressionComp, SWT.CHECK);
        breakFilesChk.setText("Break into multiple files");
        breakFilesChk.setLayoutData(gd);
        breakFilesChk.setEnabled(false);
        breakFilesChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleBreakFilesSelection(breakFilesChk.getSelection());
            }
        });

        Composite maxFileSizeComp = new Composite(compressionComp, SWT.NONE);
        gl = new GridLayout(3, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalIndent = 20;
        maxFileSizeComp.setLayout(gl);
        maxFileSizeComp.setLayoutData(gd);

        maxFileSizeLbl = new Label(maxFileSizeComp, SWT.NONE);
        maxFileSizeLbl.setText("Max File Size: ");
        maxFileSizeLbl.setEnabled(false);

        gd = new GridData(60, SWT.DEFAULT);
        fileSizeSpnr = new Spinner(maxFileSizeComp, SWT.BORDER);
        fileSizeSpnr.setIncrement(1);
        fileSizeSpnr.setPageIncrement(50);
        fileSizeSpnr.setMaximum(2000);
        fileSizeSpnr.setMinimum(500);
        fileSizeSpnr.setLayoutData(gd);
        fileSizeSpnr.setEnabled(false);

        fileSizeCbo = new Combo(maxFileSizeComp,
                SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        fileSizeCbo.setEnabled(false);
        fileSizeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleFileSizeChangeSelection();
            }
        });
        fileSizeCbo.add("MB");
        fileSizeCbo.add("GB");
        fileSizeCbo.select(0);
    }

    /**
     * Create the file browser controls.
     */
    private void createFileBrowserControls() {
        Composite fileBrowserComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.marginHeight = 0;
        fileBrowserComp.setLayout(gl);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        fileBrowserComp.setLayoutData(gd);

        Label dummy = new Label(fileBrowserComp, SWT.NONE);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        gd.horizontalSpan = 3;
        dummy.setLayoutData(gd);

        Label stateLbl = new Label(fileBrowserComp, SWT.NONE);
        stateLbl.setText("Remaining Disk Space:");
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        stateLbl.setLayoutData(gd);

        Label caseLbl = new Label(fileBrowserComp, SWT.NONE);
        caseLbl.setText("Case Location: ");

        gd = new GridData(300, SWT.DEFAULT);
        locationLbl = new Label(fileBrowserComp, SWT.BORDER);
        locationLbl.setLayoutData(gd);

        Button browseBtn = new Button(fileBrowserComp, SWT.PUSH);
        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT);
        browseBtn.setLayoutData(gd);
        browseBtn.setText(" Browse... ");
        browseBtn.setToolTipText("Select directory to place case.");
        browseBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleBrowserSelection();
            }
        });

        diskSpaceIndicator = new Label(fileBrowserComp,
                SWT.BORDER | SWT.CENTER);
        gd = new GridData(SWT.FILL, SWT.CENTER, true, false);
        diskSpaceIndicator.setLayoutData(gd);
        diskSpaceIndicator
                .setToolTipText("Available space - Uncompressed Case Size");
    }

    /**
     * Create the bottom action buttons.
     */
    private void createBottomActionButtons() {

        Composite actionControlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(8, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        actionControlComp.setLayout(gl);
        actionControlComp.setLayoutData(gd);

        // TODO - Future implementation.
        // Button exportBtn = new Button(actionControlComp, SWT.PUSH);
        // exportBtn.setText(" Export Case Config... ");
        // exportBtn.addSelectionListener(new SelectionAdapter() {
        //
        // @Override
        // public void widgetSelected(SelectionEvent e) {
        //
        // }
        // });

        String tooltip = "Waiting on loading of Data Sets.";
        Color color = shell.getDisplay().getSystemColor(SWT.COLOR_YELLOW);
        saveBtn = new Button(actionControlComp, SWT.PUSH);
        saveBtn.setText(" Save ");
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent selectionEvent) {
                if (haveAllLabels) {
                    saveSelection(selectName);
                    clearModified();
                }
            }
        });
        saveBtn.setToolTipText(tooltip);
        saveBtn.setEnabled(false);
        saveBtn.setBackground(color);

        saveAsBtn = new Button(actionControlComp, SWT.PUSH);
        saveAsBtn.setText(" Save As... ");
        saveAsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent selectionEvent) {
                if (haveAllLabels) {
                    handleSaveAsCase();
                }
            }
        });
        saveAsBtn.setToolTipText(tooltip);
        saveAsBtn.setBackground(color);

        loadBtn = new Button(actionControlComp, SWT.PUSH);
        loadBtn.setText(" Load... ");
        loadBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent selectionEvent) {
                if (haveAllLabels) {
                    handleLoadCase();
                }
            }
        });
        loadBtn.setToolTipText(tooltip);
        loadBtn.setBackground(color);

        Button deleteBtn = new Button(actionControlComp, SWT.PUSH);
        deleteBtn.setText(" Delete... ");
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent selectionEvent) {
                handleDeleteCase();
            }
        });

        generateBtn = new Button(actionControlComp, SWT.PUSH);
        generateBtn.setText(" Generate ");
        generateBtn.setEnabled(false);
        generateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                generateCase();
            }
        });

        createShowingSelectedBtn(actionControlComp);

        Button sizeBtn = new Button(actionControlComp, SWT.PUSH);
        sizeBtn.setText(" Recompute Sizes ");
        sizeBtn.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                resetSizes();
            }
        });

        gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        Button closeBtn = new Button(actionControlComp, SWT.PUSH);
        closeBtn.setText(" Close ");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (verifyClose()) {
                    close();
                } else {
                    e.doit = false;
                }
            }
        });
    }

    private void handleSaveAsCase() {
        if (saveAsDlg == null || saveAsDlg.isDisposed()) {
            saveAsDlg = new CaseLoadSaveDeleteDlg(shell,
                    CaseLoadSaveDeleteDlg.Type.SaveAs);
            saveAsDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        String name = returnValue.toString();
                        if (saveSelection(name)) {
                            clearModified();
                            loadSelect(name);
                            setSelectName(name);
                        }
                    }
                }
            });
            saveAsDlg.open();
        } else {
            saveAsDlg.bringToTop();
        }
    }

    private void handleLoadCase() {
        if (isModified() && !MessageDialog.openConfirm(shell,
                "Case Confirmation",
                "Unsave changes will be lost.\nPress OK to continue.")) {
            return;

        }
        if (loadDlg == null || loadDlg.isDisposed()) {
            loadDlg = new CaseLoadSaveDeleteDlg(shell,
                    CaseLoadSaveDeleteDlg.Type.Load);
            loadDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        String name = returnValue.toString();
                        if (loadSelect(name)) {
                            populateTableComp();
                            updateTotals(null);
                            setSelectName(name);
                            clearModified();
                        }
                    }
                }
            });
            loadDlg.open();
        } else {
            loadDlg.bringToTop();
        }
    }

    private void handleDeleteCase() {
        if (deleteDlg == null || deleteDlg.isDisposed()) {
            deleteDlg = new CaseLoadSaveDeleteDlg(shell,
                    CaseLoadSaveDeleteDlg.Type.Delete);
            deleteDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof String) {
                        String selectName = returnValue.toString();
                        deleteSelect(selectName);
                    }
                }
            });
            deleteDlg.open();
        } else {
            deleteDlg.bringToTop();
        }
    }

    /**
     * Display dialog that performs the Generation of the case.
     */
    private void generateCase() {
        setCursorBusy(true);
        File caseDir = Paths.get(locationLbl.getText(), caseNameText.getText())
                .toFile();
        Calendar startCal = getStart();
        Calendar endCal = getEnd();

        List<DisplayData> displayDatas = getSelectedData();
        boolean doCompress = compressChk.getSelection();

        boolean doMultiFiles = breakFilesChk.getSelection();
        int compressSize = fileSizeSpnr.getSelection();
        String sizeType = fileSizeCbo.getItem(fileSizeCbo.getSelectionIndex());

        setCursorBusy(true);
        if (generateCaseDlg == null || generateCaseDlg.isDisposed()) {
            long splitSize = 0L;
            if (doCompress && doMultiFiles) {
                if ("MB".equals(sizeType)) {
                    splitSize = compressSize * FileUtils.ONE_MB;
                } else {
                    splitSize = compressSize * FileUtils.ONE_GB;
                }
            }
            generateCaseDlg = new GenerateCaseDlg(shell, caseDir, startCal,
                    endCal, displayDatas, doCompress, doMultiFiles, splitSize);
            generateCaseDlg.addJobChangeListener(new JobChangeAdapter() {

                @Override
                public void done(IJobChangeEvent event) {
                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            setCursorBusy(false);
                        }
                    });
                }
            });
            generateCaseDlg.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    updateDiskSpaceIndicator();
                    setCursorBusy(false);
                    generateCaseDlg = null;
                }
            });
            generateCaseDlg.open();
        } else {
            generateCaseDlg.bringToTop();
        }

    }

    /**
     * Enable/Disable controls based on the compression check box.
     */
    private void handleCompressSelection() {
        if (compressChk.getSelection()) {
            handleBreakFilesSelection(breakFilesChk.getSelection());
        } else {
            handleBreakFilesSelection(false);
        }

        breakFilesChk.setEnabled(compressChk.getSelection());
    }

    /**
     * Validate case location
     */
    private boolean caseLocationValid() {
        String caseName = caseNameText.getText();
        String location = locationLbl.getText();

        if (caseName.isEmpty() || location.isEmpty()) {
            return false;
        }

        return Files.isDirectory(Paths.get(location));
    }

    /**
     * Enable/Disable file size controls.
     *
     * @param enabled
     *            Enabled flag.
     */
    private void handleBreakFilesSelection(boolean enabled) {
        maxFileSizeLbl.setEnabled(enabled);
        fileSizeSpnr.setEnabled(enabled);
        fileSizeCbo.setEnabled(enabled);
    }

    /**
     * Enables the generate button will user has entered all needed elements.
     */
    private void checkGenerateButton() {
        if (generateBtn != null && !generateBtn.isDisposed()) {
            generateBtn.setEnabled(caseLocationValid() && selectedItemsSize > 0
                    && availableSpace >= ccManager.getFatalThreshold());
        }
    }

    /**
     * Action performed when the file size has changed.
     */
    private void handleFileSizeChangeSelection() {
        /*
         * If the same item was selected just return.
         */
        if (fileSizeCbo.getItem(fileSizeCbo.getSelectionIndex())
                .equals(fileSizeCbo.getData())) {
            return;
        }

        if ("MB".equals(fileSizeCbo.getItem(fileSizeCbo.getSelectionIndex()))) {
            fileSizeSpnr.setIncrement(1);
            fileSizeSpnr.setPageIncrement(50);
            fileSizeSpnr.setMaximum(2000);
            fileSizeSpnr.setMinimum(500);
            fileSizeSpnr.setSelection(500);
        } else {
            fileSizeSpnr.setIncrement(1);
            fileSizeSpnr.setPageIncrement(5);
            fileSizeSpnr.setMinimum(1);
            fileSizeSpnr.setMaximum(10);
            fileSizeSpnr.setSelection(1);
        }

        fileSizeCbo
                .setData(fileSizeCbo.getItem(fileSizeCbo.getSelectionIndex()));
    }

    /**
     * Display the directory browser dialog.
     */
    private void handleBrowserSelection() {
        DirectoryDialog dlg = new DirectoryDialog(shell, SWT.OPEN);
        dlg.setText("Case Location");
        String dirName = dlg.open();
        updateLocationLbl(dirName);
    }

    /**
     * Update the case label and fields dependent on the change.
     *
     * @param dirName
     */
    private void updateLocationLbl(String dirName) {
        if (dirName != null) {
            locationLbl.setText(trimDirectoryName(dirName));
            locationLbl.setToolTipText(dirName);
            locationLbl.setData(new File(dirName));
            updateDiskSpaceIndicator();
            checkGenerateButton();
        }
    }

    /**
     * Update location's state.
     */
    private void updateDiskSpaceIndicator() {
        if (isDisposed()) {
            return;
        }
        Object o = locationLbl.getData();
        if (!(o instanceof File)) {
            return;
        }
        File dir = (File) o;
        availableSpace = dir.getUsableSpace();

        o = uncompressSizeLbl.getData();
        if (o instanceof Long) {
            availableSpace -= (Long) o;
        }

        Display display = shell.getDisplay();
        Color fgColor = display.getSystemColor(SWT.COLOR_BLACK);
        Color bgColor = display.getSystemColor(SWT.COLOR_GREEN);

        if (availableSpace < ccManager.getFatalThreshold()) {
            fgColor = display.getSystemColor(SWT.COLOR_WHITE);
            bgColor = display.getSystemColor(SWT.COLOR_BLACK);
        } else if (availableSpace < ccManager.getDangerThreshold()) {
            bgColor = display.getSystemColor(SWT.COLOR_RED);
        } else if (availableSpace < ccManager.getCautionThreshold()) {
            bgColor = display.getSystemColor(SWT.COLOR_YELLOW);
        }

        diskSpaceIndicator.setForeground(fgColor);
        diskSpaceIndicator.setBackground(bgColor);
        diskSpaceIndicator.setText(
                String.format("%s", SizeUtil.prettyByteSize(availableSpace)));

        checkGenerateButton();
    }

    private void startDateUpdated(Date date) {
        if (date.after(endDate)) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK);
            mb.setText("Date Error");
            mb.setMessage(
                    "The selected start date is after the end date.  Resetting.");
            mb.open();
            startTimeEntry.setDate(startDate);
            return;
        }

        if (!startDate.equals(date)) {
            startDate = date;
            sizeJob.resetTime(getStart(), getEnd());
            modified();
        }
    }

    private void endDateUpdated(Date date) {
        if (date.before(startDate)) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK);
            mb.setText("Date Error");
            mb.setMessage(
                    "The selected end date is before the start date.  Resetting.");
            mb.open();
            endTimeEntry.setDate(endDate);
            return;
        }
        if (!endDate.equals(date)) {
            endDate = date;
            sizeJob.resetTime(getStart(), getEnd());
            modified();
        }
    }

    /**
     * Trim the directory name if it is too long to fit in the label. The
     * beginning of the path will be replaced with "...".
     *
     * @param path
     * @return
     */
    private String trimDirectoryName(String path) {

        int labelWidth = locationLbl.getBounds().width - 5;

        GC gc = new GC(locationLbl);
        int pathLength = gc.stringExtent(path).x;

        if (pathLength > labelWidth) {
            int from = 0;
            if (path.startsWith(File.separator)) {
                from = 1;
            }

            do {
                int p = path.indexOf(File.separator, from);
                if (p == -1) {
                    break;
                }
                path = "..." + path.substring(p);
                from = 4;
                pathLength = gc.stringExtent(path).x;
            } while (pathLength > labelWidth);
        }

        gc.dispose();
        return path;
    }

    @Override
    protected void setTotalSizeText(String text) {
        if (!uncompressSizeLbl.isDisposed()) {
            uncompressSizeLbl.setText(text);
        }
    }

    @Override
    protected void setTotalSelectedSize(long totalSize) {
        super.setTotalSelectedSize(totalSize);
        Long tSize = null;
        if (totalSize > 0) {
            tSize = new Long(totalSize);
        }
        uncompressSizeLbl.setData(tSize);
        updateDiskSpaceIndicator();
    }

    @Override
    protected void setTotalSelectedItems(int totalSelected) {
        selectedItemsSize = totalSelected;
        totalSelectedItemsLbl.setText(Integer.toBinaryString(totalSelected));
        checkGenerateButton();
    }

    @Override
    protected Calendar getStart() {
        Calendar startCal = TimeUtil.newCalendar();
        startCal.setTimeInMillis(startDate.getTime());
        return startCal;
    }

    @Override
    protected Calendar getEnd() {
        Calendar endCal = TimeUtil.newCalendar();
        endCal.setTimeInMillis(endDate.getTime());
        return endCal;
    }

    @Override
    public void modified() {
        saveBtn.setEnabled(true);
    }

    @Override
    public void clearModified() {
        super.clearModified();
        saveBtn.setEnabled(false);
    }

    @Override
    protected void preOpened() {
        super.preOpened();

        File caseDir = new File(defaultCaseDir);
        if (caseDir.isDirectory()) {
            updateLocationLbl(defaultCaseDir);
        } else {
            MessageDialog.openError(shell, "Error",
                    String.format(
                            "Unable to find Case Location directory:\n%s\nMay need to mount the directory.",
                            defaultCaseDir));
        }
    }

    @Override
    public void loadedAllDisplayData() {
        /*
         * Restore the buttons' default background color and tooltip text. The
         * buttons color is not the system standard and the tool tip text
         * indicates it is waiting for the labels to be loaded.
         */
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                if (!isDisposed()) {
                    saveBtn.setBackground(null);
                    saveBtn.setToolTipText(null);
                    saveAsBtn.setBackground(null);
                    saveAsBtn.setToolTipText(null);
                    loadBtn.setBackground(null);
                    loadBtn.setToolTipText(null);
                }
            }
        });
        super.loadedAllDisplayData();
    }

    @Override
    protected boolean isModified() {
        return super.isModified();
    }

}
