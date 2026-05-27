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
package com.raytheon.viz.hydrobase.dialogs;

import java.text.SimpleDateFormat;
import java.util.TimeZone;
import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.PurgeDynData;
import com.raytheon.viz.hydrocommon.data.PurgeProductData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.util.HydroDataUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Data Purge Parameters dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 4, 2008				lvenable	Initial creation
 * Dec 17, 2008 1787        askripsk    Connected to Database.
 * May 6, 2009  2181        mpduff      Keep selection upon submit.
 * Apr 18, 2013 1790        rferrel     Make dialog non-blocking.
 * Mar 31, 2014 #2970       lvenable    Put dispose checks in the runAsync calls.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class DataPurgeParamsDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataPurgeParamsDlg.class);

    /**
     * Location data list control.
     */
    private List locDataList;

    /**
     * Table name text control.
     */
    private Text tableNameTF;

    /**
     * Host area text control.
     */
    private Text hostAreaTF;

    /**
     * Backup area text control.
     */
    private Text backupAreaTF;

    /**
     * Time FLD text control.
     */
    private Text timeFldNameTF;

    /**
     * Location data update button.
     */
    private Button locDataUpdateBtn;

    /**
     * Text product purge list control.
     */
    private List textProductPurgeList;

    /**
     * Product ID text control.
     */
    private Text productIdTF;

    /**
     * Version text control.
     */
    private Text versionTF;

    /**
     * Text product add button.
     */
    private Button textAddBtn;

    /**
     * Text product update button.
     */
    private Button textUpdateBtn;

    /**
     * Text delete add button.
     */
    private Button textDeleteBtn;

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Cache of data for Location Data Purge Parameters
     */
    private java.util.List<PurgeDynData> locDataPurgeParams;

    /**
     * Cache of data for Text Product Purge Parameters
     */
    private java.util.List<PurgeProductData> textProdPurgeParams;

    /**
     * Date format for text products.
     */
    private SimpleDateFormat textDateFormat;

    /**
     * The selected index.
     */
    private int selectionIndex = -999;

    /**
     * System wait cursor. No need to dispose.
     */
    Cursor waitCursor;

    /**
     * Use by setBusy to determine cursor display.
     */
    AtomicInteger busyCnt = new AtomicInteger(0);

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public DataPurgeParamsDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Data Purge Parameters");

        textDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        textDateFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 5;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        controlFont.dispose();
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
        setReturnValue(false);

        waitCursor = shell.getDisplay().getSystemCursor(SWT.CURSOR_WAIT);

        // Initialize all of the controls and layouts
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createLocationDataGroup();

        createTextProductGroup();

        // Add a separator bar
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createCloseButton();

        getDialogData();
    }

    /**
     * Create the Location Data group and controls.
     */
    private void createLocationDataGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group locationGroup = new Group(shell, SWT.NONE);
        locationGroup.setLayout(new GridLayout(4, false));
        locationGroup.setLayoutData(gd);
        locationGroup.setText(" Location Data Purge Paramenters ");

        gd = new GridData();
        gd.horizontalSpan = 4;
        gd.horizontalIndent = 4;
        Label locDataTopLbl = new Label(locationGroup, SWT.NONE);
        locDataTopLbl.setText(getLocationDataTopLabel());
        locDataTopLbl.setFont(controlFont);
        locDataTopLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 4;
        gd.horizontalIndent = 4;
        Label locDataBottomLbl = new Label(locationGroup, SWT.NONE);
        locDataBottomLbl.setText(getLocationDataBottomLabel());
        locDataBottomLbl.setFont(controlFont);
        locDataBottomLbl.setLayoutData(gd);

        gd = new GridData(650, 160);
        gd.horizontalSpan = 4;
        locDataList = new List(locationGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        locDataList.setLayoutData(gd);
        locDataList.setFont(controlFont);
        locDataList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateLocationInformationDisplay();
            }
        });

        gd = new GridData(150, SWT.DEFAULT);
        tableNameTF = new Text(locationGroup, SWT.BORDER);
        tableNameTF.setLayoutData(gd);
        tableNameTF.setEditable(false);

        gd = new GridData(50, SWT.DEFAULT);
        gd.horizontalIndent = 65;
        hostAreaTF = new Text(locationGroup, SWT.BORDER);
        hostAreaTF.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        gd.horizontalIndent = 110;
        backupAreaTF = new Text(locationGroup, SWT.BORDER);
        backupAreaTF.setLayoutData(gd);

        gd = new GridData(150, SWT.DEFAULT);
        gd.horizontalIndent = 35;
        timeFldNameTF = new Text(locationGroup, SWT.BORDER);
        timeFldNameTF.setLayoutData(gd);
        timeFldNameTF.setTextLimit(18);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 120;
        gd.horizontalSpan = 4;
        locDataUpdateBtn = new Button(locationGroup, SWT.PUSH);
        locDataUpdateBtn.setText("Update");
        locDataUpdateBtn.setLayoutData(gd);
        locDataUpdateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveLocRecord();
            }
        });
    }

    /**
     * Create the Text Product group and controls.
     */
    private void createTextProductGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group textProdGroup = new Group(shell, SWT.NONE);
        textProdGroup.setLayout(new GridLayout(2, false));
        textProdGroup.setLayoutData(gd);
        textProdGroup.setText(" Text Product Purge Paramenters ");

        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.horizontalIndent = 4;
        Label textProdTopLbl = new Label(textProdGroup, SWT.NONE);
        textProdTopLbl.setText(getTextProductTopLabel());
        textProdTopLbl.setFont(controlFont);
        textProdTopLbl.setLayoutData(gd);

        gd = new GridData();
        gd.horizontalSpan = 2;
        gd.horizontalIndent = 4;
        Label textProdBottomLbl = new Label(textProdGroup, SWT.NONE);
        textProdBottomLbl.setText(getTextProductBottomLabel());
        textProdBottomLbl.setFont(controlFont);
        textProdBottomLbl.setLayoutData(gd);

        gd = new GridData(650, 160);
        gd.horizontalSpan = 2;
        textProductPurgeList = new List(textProdGroup, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        textProductPurgeList.setLayoutData(gd);
        textProductPurgeList.setFont(controlFont);
        textProductPurgeList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                updateTextInformationDisplay();
            }
        });

        gd = new GridData(110, SWT.DEFAULT);
        productIdTF = new Text(textProdGroup, SWT.BORDER);
        productIdTF.setLayoutData(gd);
        productIdTF.setTextLimit(10);

        gd = new GridData(30, SWT.DEFAULT);
        gd.horizontalIndent = 40;
        versionTF = new Text(textProdGroup, SWT.BORDER);
        versionTF.setLayoutData(gd);

        // --------------------------------------------------
        // Create the Add, Update, and Delete buttons.
        // --------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Composite buttonComp = new Composite(textProdGroup, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, true));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, true);
        gd.widthHint = 100;
        textAddBtn = new Button(buttonComp, SWT.PUSH);
        textAddBtn.setText("Add");
        textAddBtn.setLayoutData(gd);
        textAddBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveTextRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, true);
        gd.widthHint = 100;
        textUpdateBtn = new Button(buttonComp, SWT.PUSH);
        textUpdateBtn.setText("Update");
        textUpdateBtn.setLayoutData(gd);
        textUpdateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveTextRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, true);
        gd.widthHint = 100;
        textDeleteBtn = new Button(buttonComp, SWT.PUSH);
        textDeleteBtn.setText("Delete");
        textDeleteBtn.setLayoutData(gd);
        textDeleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteTextRecord();
            }
        });
    }

    /**
     * Create the close button.
     */
    private void createCloseButton() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Get the top label text for the location data list.
     * 
     * @return Top label text.
     */
    private String getLocationDataTopLabel() {
        String format = "                     %S";

        String labelStr = String.format(format,
                "Days/Hrs (Hrs) To Retain for Locations in");

        return labelStr;
    }

    /**
     * Get the bottom label text for the location data list.
     * 
     * @return Bottom label text.
     */
    private String getLocationDataBottomLabel() {
        String format = "%S           %S              %S       %S";

        String labelStr = String.format(format, "Table Name", "Host Area",
                "Backup Areas", "Time Fld Name");

        return labelStr;
    }

    /**
     * Get the top label text for the text product purge list.
     * 
     * @return Top label text.
     */
    private String getTextProductTopLabel() {
        String format = "                 %S    %S";

        String labelStr = String.format(format, "Versions", "Latest");

        return labelStr;
    }

    /**
     * Get the bottom label text for the text product purge list.
     * 
     * @return Bottom label text.
     */
    private String getTextProductBottomLabel() {
        String format = "%S       %S     %S             %S";

        String labelStr = String.format(format, "Product Id", "to Keep",
                "Product Time", "Posting Time");

        return labelStr;
    }

    /**
     * Retrieve purge data and update the display.
     * 
     */
    private void getDialogData() {
        getLocDialogData();
        getTextDialogData();
    }

    /**
     * Retrieve Purge Dyn Data and update the display.
     */
    private void getLocDialogData() {
        setBusy(true);

        // Remove from UI thread.
        Job job = new Job("") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                try {
                    locDataPurgeParams = HydroDBDataManager.getInstance()
                            .getData(PurgeDynData.class);
                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            if (isDisposed()) {
                                return;
                            }
                            updateLocDialogDisplay();
                        }
                    });
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Problems getting location data. ", e);
                } finally {
                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            if (isDisposed()) {
                                return;
                            }
                            setBusy(false);
                        }
                    });
                }
                return Status.OK_STATUS;
            }
        };
        job.schedule();
    }

    /**
     * Retrieve Purge Product data and update the display.
     */
    private void getTextDialogData() {
        setBusy(true);

        // Remove from UI thread.
        Job job = new Job("") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                try {
                    textProdPurgeParams = HydroDBDataManager.getInstance()
                            .getData(PurgeProductData.class);
                    VizApp.runAsync(new Runnable() {

                        @Override
                        public void run() {
                            if (isDisposed()) {
                                return;
                            }
                            updateTextDialogDisplay();
                        }
                    });
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Problems getting purge data. ", e);
                } finally {
                    VizApp.runAsync(new Runnable() {
                        @Override
                        public void run() {
                            if (isDisposed()) {
                                return;
                            }
                            setBusy(false);
                        }
                    });
                }
                return Status.OK_STATUS;
            }
        };
        job.schedule();
    }

    private void updateLocDialogDisplay() {
        selectionIndex = locDataList.getSelectionIndex();
        locDataList.removeAll();

        for (PurgeDynData currData : locDataPurgeParams) {
            locDataList.add(getDisplayString(currData));
        }

        if (locDataPurgeParams.size() > 0) {
            if (selectionIndex != -999) {
                locDataList.select(selectionIndex);
            } else {
                locDataList.select(0);
            }
            updateLocationInformationDisplay();
        }
    }

    private void updateTextDialogDisplay() {
        selectionIndex = textProductPurgeList.getSelectionIndex();
        textProductPurgeList.removeAll();

        for (PurgeProductData currData : textProdPurgeParams) {
            textProductPurgeList.add(getDisplayString(currData));
        }

        if (textProdPurgeParams.size() > 0) {
            if (selectionIndex != -999) {
                textProductPurgeList.select(selectionIndex);
            } else {
                textProductPurgeList.select(0);
            }
            updateTextInformationDisplay();
        }
    }

    /**
     * Format the Purge Dyn Data for display.
     * 
     * @param currData
     * @return
     */
    private String getDisplayString(PurgeDynData currData) {
        String displayFormat = "%-18s %-8s (%5s)       %-8s (%5s)      %-18.18s";

        int numDays = 0;
        int numHours = 0;

        if (currData.getHostHours() != HydroConstants.MISSING_VALUE) {
            numDays = currData.getHostHours() / 24;
            numHours = currData.getHostHours() % 24;
        }

        String dayHoursHost = String.format("%5d/%2d", numDays, numHours);

        numDays = 0;
        numHours = 0;

        if (currData.getBackupHours() != HydroConstants.MISSING_VALUE) {
            numDays = currData.getBackupHours() / 24;
            numHours = currData.getBackupHours() % 24;
        }

        String dayHoursBackup = String.format("%5d/%2d", numDays, numHours);

        return String.format(displayFormat, currData.getTableName(),
                dayHoursHost,
                HydroDataUtils.getDisplayString(currData.getHostHours()),
                dayHoursBackup,
                HydroDataUtils.getDisplayString(currData.getBackupHours()),
                currData.getTimeColumnName());
    }

    /**
     * Format the purge product data for display.
     * 
     * @param currData
     * @return
     */
    private String getDisplayString(PurgeProductData currData) {
        String displayFormat = "%-10s      %6s       %-19s      %-19s";

        String prodTime = "N/A";
        if (currData.getProductTime() != null) {
            prodTime = textDateFormat.format(currData.getProductTime());
        }

        String postTime = "N/A";
        if (currData.getPostingTime() != null) {
            postTime = textDateFormat.format(currData.getPostingTime());
        }

        return String
                .format(displayFormat, currData.getProductID(), HydroDataUtils
                        .getDisplayString(currData.getNumberOfVersions()),
                        prodTime, postTime);
    }

    /**
     * Update purge information for selected location.
     */
    private void updateLocationInformationDisplay() {
        PurgeDynData currData = getCurrentlySelectedLocation();

        if (currData != null) {
            tableNameTF.setText(currData.getTableName());
            hostAreaTF.setText(HydroDataUtils.getDisplayString(currData
                    .getHostHours()));
            backupAreaTF.setText(HydroDataUtils.getDisplayString(currData
                    .getBackupHours()));
            timeFldNameTF.setText(currData.getTimeColumnName());
        }
    }

    /**
     * Update purge information for selected text product.
     */
    private void updateTextInformationDisplay() {
        PurgeProductData currData = getCurrentlySelectedText();

        if (currData != null) {
            productIdTF.setText(currData.getProductID());
            versionTF.setText(HydroDataUtils.getDisplayString(currData
                    .getNumberOfVersions()));
        }
    }

    /**
     * 
     * @return selected location purge data or null if none
     */
    private PurgeDynData getCurrentlySelectedLocation() {
        PurgeDynData rval = null;

        if (locDataList.getSelectionCount() > 0) {
            rval = locDataPurgeParams.get(locDataList.getSelectionIndex());
        }

        return rval;
    }

    /**
     * 
     * @return selected text product purge data or null if none
     */
    private PurgeProductData getCurrentlySelectedText() {
        PurgeProductData rval = null;

        if (textProductPurgeList.getSelectionCount() > 0) {
            rval = textProdPurgeParams.get(textProductPurgeList
                    .getSelectionIndex());
        }

        return rval;
    }

    /**
     * Save current purged data.
     */
    private void saveLocRecord() {
        PurgeDynData currData = new PurgeDynData();

        Integer hostHours = HydroDataUtils.getIntegerFromTF(shell, hostAreaTF,
                "Host Hours");
        Integer backupHours = HydroDataUtils.getIntegerFromTF(shell,
                backupAreaTF, "Backup Hours");

        if ((hostHours != null) && (backupHours != null)) {
            currData.setBackupHours(backupHours);
            currData.setHostHours(hostHours);
            currData.setTableName(tableNameTF.getText());
            currData.setTimeColumnName(timeFldNameTF.getText());

            try {
                HydroDBDataManager.getInstance().putData(currData);

                getLocDialogData();
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Problems saving data. ", e);
            }
        }
    }

    /*
     * Save a record.
     */
    private void saveTextRecord() {
        PurgeProductData currData = new PurgeProductData();

        Integer versions = HydroDataUtils.getIntegerFromTF(shell, versionTF,
                "Versions To Keep");

        if (versions != null) {
            if (!productIdTF.getText().equals("")
                    && !versionTF.getText().equals("")) {
                currData.setProductID(productIdTF.getText());
                currData.setNumberOfVersions(versions);

                try {
                    HydroDBDataManager.getInstance().putData(currData);

                    getTextDialogData();
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Problems saving data. ", e);
                }
            } else {
                MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK);
                mb.setText("Unable to Save");
                mb.setMessage("Please enter data for Product ID and Versions To Keep.");
                mb.open();
            }
        }
    }

    /**
     * Remove a record.
     */
    private void deleteTextRecord() {
        PurgeProductData currData = getCurrentlySelectedText();

        if (currData != null) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                    | SWT.CANCEL);
            mb.setText("Delete Confirmation");
            mb.setMessage("Do you wish to delete this entry?");

            int result = mb.open();

            if (result == SWT.OK) {
                try {
                    HydroDBDataManager.getInstance().deleteRecord(currData);
                    selectionIndex = 0;
                    getTextDialogData();
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Problems deleteing data. ", e);
                }
            }
        } else {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Selection");
            mb.setMessage("Please select an item from the list first.");
            mb.open();
        }
    }

    /**
     * Determine what cursor to display.
     * 
     * @param busy
     */
    private void setBusy(boolean busy) {
        if (busy) {
            busyCnt.incrementAndGet();
            shell.setCursor(waitCursor);
        } else if (busyCnt.decrementAndGet() == 0) {
            shell.setCursor(null);
        }
    }
}
