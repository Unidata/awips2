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

package com.raytheon.viz.hydro.datatrashcan;

import java.util.ArrayList;
import java.util.Collections;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.IGetSortType;
import com.raytheon.viz.hydrocommon.data.DataTrashCanData;
import com.raytheon.viz.hydrocommon.datamanager.DataTrashCanDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Data Trash Can dialog for Hydroview.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 18 JUL 2010  2110       mpduff      Tweaked list box labels
 * 05 FEB 2013  1578       rferrel     Made dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class DataTrashCanDlg extends CaveSWTDialog implements IGetSortType {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DataTrashCanDlg.class);

    /**
     * Font used for list controls.
     */
    private Font font;

    /**
     * Location check box.
     */
    private Button locationChk;

    /**
     * Location text control.
     */
    private Text locationTF;

    /**
     * Physical element check box/
     */
    private Button peChk;

    /**
     * Reject type combo box.
     */
    private Combo rejectTypeCbo;

    /**
     * Physical element data list control.
     */
    private List peDataList;

    /**
     * Sort by combo list.
     */
    private Combo sortByCbo;

    /**
     * Data list control.
     */
    private List dataList;

    /**
     * Collection of trash data.
     */
    private java.util.List<DataTrashCanData> trashData;

    /**
     * Collection of filtered trash data.
     */
    private java.util.List<DataTrashCanData> filteredTrashData;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public DataTrashCanDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Data Trash Can");
        filteredTrashData = new ArrayList<DataTrashCanData>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#disposed()
     */
    @Override
    protected void disposed() {
        font.dispose();
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
        font = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        // Initialize all of the controls and layouts
        initializeComponents();
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents() {
        createTopGroupCotrols();
        createDataListLabels();
        createDataListControl();
        createBottomButtons();

        // Get/Display data
        getTrashData();
    }

    /**
     * Create the controls located at the top of the dialog.
     */
    private void createTopGroupCotrols() {
        GridData mainGridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group topGroup = new Group(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        topGroup.setLayout(gl);
        topGroup.setLayoutData(mainGridData);

        // -----------------------------------------
        // Create the left side of the top section
        // -----------------------------------------
        GridData leftGD = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        Composite leftComp = new Composite(topGroup, SWT.NONE);
        GridLayout leftGl = new GridLayout(4, false);
        leftComp.setLayout(leftGl);
        leftComp.setLayoutData(leftGD);

        Label filterByLbl = new Label(leftComp, SWT.NONE);
        filterByLbl.setText("Filter By:");

        locationChk = new Button(leftComp, SWT.CHECK);
        locationChk.setText("Location:");
        locationChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                locationTF.setEnabled(locationChk.getSelection());
                filterDisplayList();
            }
        });

        GridData gd = new GridData(80, SWT.DEFAULT);
        locationTF = new Text(leftComp, SWT.BORDER);
        locationTF.setLayoutData(gd);
        locationTF.setEnabled(false);
        locationTF.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
            }

            @Override
            public void keyReleased(KeyEvent e) {
                filterDisplayList();
            }
        });

        locationTF.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();
            }
        });

        peChk = new Button(leftComp, SWT.CHECK);
        peChk.setText("Phys. Element:");
        peChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                peDataList.setEnabled(peChk.getSelection());
                filterDisplayList();
            }
        });

        Label rejectLbl = new Label(leftComp, SWT.NONE);
        rejectLbl.setText("Reject Type:");

        gd = new GridData(130, SWT.DEFAULT);
        gd.horizontalSpan = 2;
        rejectTypeCbo = new Combo(leftComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        rejectTypeCbo.setLayoutData(gd);
        rejectTypeCbo.add("All");
        rejectTypeCbo.add("Auto");
        rejectTypeCbo.add("Manual");
        rejectTypeCbo.select(0);
        rejectTypeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                filterDisplayList();
            }
        });

        // -----------------------------------------
        // Create the list control in the center of
        // the top section
        // -----------------------------------------
        Composite centerComp = new Composite(topGroup, SWT.NONE);
        GridLayout centerGl = new GridLayout(1, false);
        centerComp.setLayout(centerGl);

        gd = new GridData(200, 75);
        peDataList = new List(centerComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        peDataList.setLayoutData(gd);
        peDataList.setFont(font);
        peDataList.setEnabled(false);
        peDataList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                filterDisplayList();
            }
        });

        // Populate the PE Data List
        loadPhyElemListData();

        // -----------------------------------------
        // Create the sort control on the left side
        // of the top section
        // -----------------------------------------

        GridData rightGD = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        Composite rightComp = new Composite(topGroup, SWT.NONE);
        GridLayout rightGl = new GridLayout(2, false);
        rightComp.setLayout(rightGl);
        rightComp.setLayoutData(rightGD);

        Label sortByLbl = new Label(rightComp, SWT.NONE);
        sortByLbl.setText("Sort By:");

        sortByCbo = new Combo(rightComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        sortByCbo.add("Location");
        sortByCbo.add("Time");
        sortByCbo.select(0);
        sortByCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                sortDisplayList();
            }
        });
    }

    /**
     * Create the labels for the data list.
     */
    private void createDataListLabels() {
        Composite labelComp = new Composite(shell, SWT.NONE);
        GridLayout labelGl = new GridLayout(1, false);
        labelComp.setLayout(labelGl);

        Label label = new Label(labelComp, SWT.NONE);
        String labelText = String
                .format("%-8s         %-33s %2s  %6s  %3s  %4s %12s  %15s   %19s         %2s %2s %2s %8s    %6s   %11.11s          %-10.10s    %11.11s",
                        "Location", "Name", "PE", "Dur", "TS", "Ext", "Value",
                        "ObsTime", "BasisTime", "RV", "SQ", "QC", "User",
                        "Type", "PostTime", "Product", "ProdTime");
        label.setText(labelText);
    }

    /**
     * Create the data list control.
     */
    private void createDataListControl() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        gd.widthHint = 1150;
        gd.heightHint = 450;
        dataList = new List(shell, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        dataList.setLayoutData(gd);

        dataList.setFont(font);
    }

    /**
     * Create the buttons located at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData mainGD = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridLayout buttonGl = new GridLayout(4, false);
        buttonGl.horizontalSpacing = 10;
        buttonComp.setLayout(buttonGl);
        buttonComp.setLayoutData(mainGD);

        GridData gd = new GridData(250, SWT.DEFAULT);
        Button moveSelectedBtn = new Button(buttonComp, SWT.PUSH);
        moveSelectedBtn.setText("Move Selected to Data Tables");
        moveSelectedBtn.setLayoutData(gd);
        moveSelectedBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                try {
                    moveToDataTables();
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error moving data: ", e);
                }
            }
        });

        gd = new GridData(160, SWT.DEFAULT);
        Button deleteSelectedBtn = new Button(buttonComp, SWT.PUSH);
        deleteSelectedBtn.setText("Delete Selected");
        deleteSelectedBtn.setLayoutData(gd);
        deleteSelectedBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteSelected();
            }
        });

        gd = new GridData(160, SWT.DEFAULT);
        Button deleteAllBtn = new Button(buttonComp, SWT.PUSH);
        deleteAllBtn.setText("Delete All");
        deleteAllBtn.setLayoutData(gd);
        deleteAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteAll();
            }
        });

        gd = new GridData(SWT.END, SWT.DEFAULT, true, false);
        gd.widthHint = 100;
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Get the data from the DB
     * 
     * @throws VizException
     */
    private void getTrashData() {
        try {
            trashData = DataTrashCanDataManager.getInstance()
                    .getDataTrashCanData(this);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Problem getting trash data: ", e);
        }

        filterDisplayList();
    }

    /**
     * Sort the data and update the data list control.
     */
    private void sortDisplayList() {
        // Get the correct collection of data and Sort
        Collections.sort(filteredTrashData);

        // Filter and Populate
        updateDisplayList();
    }

    /**
     * Apply add active filters to limit the display list.
     */
    private void filterDisplayList() {
        filteredTrashData.clear();

        filterByLID();
        filterByRejectType();
        filterByPE();

        sortDisplayList();
    }

    /**
     * Filter the list by desired location ID.
     */
    private void filterByLID() {
        String stationSearch = locationTF.getText().toUpperCase();

        if (locationChk.getSelection()) {
            for (DataTrashCanData unfilteredLID : trashData) {
                if (unfilteredLID.getLid().contains(stationSearch)) {
                    filteredTrashData.add(unfilteredLID);
                }
            }
        } else {
            for (DataTrashCanData unfilteredLID : trashData) {
                filteredTrashData.add(unfilteredLID);
            }
        }
    }

    /**
     * Filter by reject type All, Auto, or Manual.
     */
    private void filterByRejectType() {
        String rejectType = rejectTypeCbo.getItem(rejectTypeCbo
                .getSelectionIndex());

        if (rejectType.compareToIgnoreCase("All") != 0) {
            ArrayList<DataTrashCanData> temp = new ArrayList<DataTrashCanData>();

            for (DataTrashCanData unfilteredLID : filteredTrashData) {
                if (((rejectType.compareToIgnoreCase("Auto") == 0) && (unfilteredLID
                        .getRejectType().compareToIgnoreCase("A") == 0))
                        || ((rejectType.compareToIgnoreCase("Manual") == 0) && (unfilteredLID
                                .getRejectType().compareToIgnoreCase("M") == 0))) {
                    temp.add(unfilteredLID);
                }
            }

            filteredTrashData = temp;
        }
    }

    /**
     * When active filter by the selected physical element.
     */
    private void filterByPE() {

        if (peChk.getSelection()) {
            ArrayList<DataTrashCanData> temp = new ArrayList<DataTrashCanData>();

            int[] selectedInd = peDataList.getSelectionIndices();
            ArrayList<String> peFilter = new ArrayList<String>();

            for (int i : selectedInd) {
                peFilter.add(peDataList.getItem(i).split(" ")[0].toUpperCase());
            }

            for (DataTrashCanData unfilteredLID : filteredTrashData) {
                if (peFilter.contains(unfilteredLID.getPe().toUpperCase())) {
                    temp.add(unfilteredLID);
                }
            }

            filteredTrashData = temp;
        }
    }

    /**
     * Update the display list with the contents of filteredTrashData.
     */
    private void updateDisplayList() {
        dataList.removeAll();

        for (DataTrashCanData currData : filteredTrashData) {
            dataList.add(currData.toString());
        }
    }

    /**
     * Populate the physical elements list.
     */
    private void loadPhyElemListData() {
        try {
            for (String currPE : DataTrashCanDataManager.getInstance()
                    .getPEList()) {
                peDataList.add(currPE);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Problem getting PE values: ", e);
        }
    }

    /**
     * Called by the "Delete Selected" button. Deletes only the selected records
     * from the RejectedData table.
     */
    private void deleteSelected() {
        if (MessageDialog
                .openConfirm(
                        null,
                        "Delete Confirmation",
                        "Do you wish to delete "
                                + ((dataList.getSelectionIndices().length > 1) ? "these "
                                        + dataList.getSelectionIndices().length
                                        + " records?"
                                        : "this record?"))) {
            ArrayList<DataTrashCanData> recordsToDelete = new ArrayList<DataTrashCanData>();

            for (int i : dataList.getSelectionIndices()) {
                recordsToDelete.add(filteredTrashData.get(i));
            }

            deleteRecords(recordsToDelete);
        }
    }

    /**
     * Called by the "Delete All" button. Removes all records from the
     * RejectedData table.
     */
    private void deleteAll() {
        if (MessageDialog.openConfirm(null, "Empty Confirmation",
                "Do you wish to delete ALL records in the Trash Bin?")) {
            // Delete All records from RejectedData
            deleteRecords(trashData);
        }
    }

    /**
     * Performs the actual deletion of records via calls to the data manager.
     * 
     * @param dataToDelete
     */
    private void deleteRecords(java.util.List<DataTrashCanData> dataToDelete) {
        // Have DM delete records
        try {
            DataTrashCanDataManager.getInstance().deleteTrashRecords(
                    dataToDelete);
        } catch (VizException e) {
            MessageDialog.openConfirm(null, "Unable to delete records.",
                    "Unable to delete records.");
        }

        // Refresh data records
        getTrashData();
    }

    /**
     * Repost selected data to the correct PE table and remove it from the
     * rejected data table.
     * 
     * @throws VizException
     */
    private void moveToDataTables() throws VizException {
        // Repost to PE Table and Delete from Trash Table via DM
        DataTrashCanDataManager.getInstance().repostData(
                getCurrentlySelectedRange());

        // Refresh Data
        getTrashData();
    }

    /**
     * Obtain a list of selected trash can data.
     * 
     * @return rval
     */
    private java.util.List<DataTrashCanData> getCurrentlySelectedRange() {
        ArrayList<DataTrashCanData> rval = new ArrayList<DataTrashCanData>();

        for (int i : dataList.getSelectionIndices()) {
            rval.add(filteredTrashData.get(i));
        }

        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.hydrocommon.IGetSortType#getSortType()
     */
    @Override
    public String getSortType() {
        return sortByCbo.getItem(sortByCbo.getSelectionIndex());
    }
}
