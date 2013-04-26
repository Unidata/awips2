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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.DamTypeData;
import com.raytheon.viz.hydrocommon.data.ReservoirOwnerData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Reference Fields dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 8, 2008				lvenable	Initial creation
 * Apr 26, 2013             rferrel     Make dialog non-blocking.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ReferenceFieldsDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReferenceFieldsDlg.class);

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Fields combo box.
     */
    private Combo fieldsCbo;

    /**
     * Add button.
     */
    private Button addBtn;

    /**
     * Update button.
     */
    private Button updateBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Data list control.
     */
    private List dataList;

    /**
     * Data text field.
     */
    private Text dataTF;

    /**
     * Dam Type Data.
     */
    private java.util.List<DamTypeData> damTypeData;

    /**
     * Reservoir Owner Data.
     */
    private java.util.List<ReservoirOwnerData> resOwnerData;

    private final int OWNER = 0;

    private final int TYPE = 1;

    /**
     * Non-blocking Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public ReferenceFieldsDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Reference Fields");
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
        mainLayout.verticalSpacing = 2;
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

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createControls();

        // Add a separator bar
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        createOkButton();

        getData();
        populateDataList(fieldsCbo.getSelectionIndex());
    }

    /**
     * Create the main controls.
     */
    private void createControls() {
        Composite mainComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.horizontalSpacing = 25;
        mainComp.setLayout(gl);

        // --------------------------------------------
        // Create left side controls
        // --------------------------------------------
        GridData gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        Composite leftComp = new Composite(mainComp, SWT.NONE);
        leftComp.setLayout(new GridLayout(2, false));
        leftComp.setLayoutData(gd);

        Label fieldLbl = new Label(leftComp, SWT.NONE);
        fieldLbl.setText("Fields:");

        fieldsCbo = new Combo(leftComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        fieldsCbo.add("Reservoir Owner");
        fieldsCbo.add("Reservoir Type");
        fieldsCbo.select(0);
        fieldsCbo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                populateDataList(fieldsCbo.getSelectionIndex());
            }
        });

        // --------------------------------------------
        // Create right side controls
        // --------------------------------------------

        Composite rightComp = new Composite(mainComp, SWT.NONE);
        rightComp.setLayout(new GridLayout(2, false));

        // Add the buttons
        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Composite buttonComp = new Composite(rightComp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        addBtn = new Button(buttonComp, SWT.PUSH);
        addBtn.setText("Add");
        addBtn.setLayoutData(gd);
        addBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                addEntry();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        updateBtn = new Button(buttonComp, SWT.PUSH);
        updateBtn.setText("Update");
        updateBtn.setLayoutData(gd);
        updateBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                updateSelectedItem();
            }
        });

        gd = new GridData(100, SWT.DEFAULT);
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteSelectedItem();
            }
        });

        // Add the data list
        gd = new GridData(250, 250);
        dataList = new List(rightComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        dataList.setLayoutData(gd);
        dataList.setFont(controlFont);

        // Filler label
        new Label(rightComp, SWT.NONE);

        // Add the data text field
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        dataTF = new Text(rightComp, SWT.BORDER);
        dataTF.setLayoutData(gd);
    }

    /**
     * Create the OK button at the bottom of the dialog.
     */
    private void createOkButton() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Get dam type and reservoir owner data.
     */
    private void getData() {
        DamTypeData seedData1 = new DamTypeData();
        seedData1.setType("Unk");

        if (damTypeData != null) {
            damTypeData.clear();
        }
        try {
            damTypeData = HydroDBDataManager.getInstance().getData(seedData1);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Uable to get dam type data. ", e);
        }

        // Get the Reservoir Owner Data
        ReservoirOwnerData seedData2 = new ReservoirOwnerData();
        seedData2.setOwner("Unk");

        if (resOwnerData != null) {
            resOwnerData.clear();
        }
        try {
            resOwnerData = HydroDBDataManager.getInstance().getData(seedData2);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Uable to get reservoir owner data. ", e);
        }
    }

    /**
     * Place data in the desired list.
     * 
     * @param index
     */
    private void populateDataList(int index) {
        dataList.removeAll();

        if (index == OWNER) {
            for (ReservoirOwnerData data : resOwnerData) {
                dataList.add(data.getOwner());
            }
        } else if (index == TYPE) {
            for (DamTypeData data : damTypeData) {
                dataList.add(data.getType());
            }
        }

        if (dataList.getItemCount() > 0) {
            dataList.setSelection(0);
        }
    }

    /**
     * Add new data entry and update the lists.
     */
    private void addEntry() {
        if (dataList.getSelectionIndex() < 0) {
            return;
        }

        if (dataTF.getText().trim().length() == 0) {
            MessageBox mb = new MessageBox(getParent(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("Add Error");
            mb.setMessage("You need to enter something in the text field.");
            mb.open();
            return;
        }

        if (fieldsCbo.getSelectionIndex() == OWNER) {
            try {
                ReservoirOwnerData newData = new ReservoirOwnerData();
                newData.setOwner(dataTF.getText().trim());
                HydroDBDataManager.getInstance().putData(newData);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Uable to add reservoir owner data. ", e);
            }
        } else if (fieldsCbo.getSelectionIndex() == TYPE) {
            try {
                DamTypeData newData = new DamTypeData();
                newData.setType(dataTF.getText().trim());
                HydroDBDataManager.getInstance().putData(newData);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Uable to add dam type data. ", e);
            }
        }

        getData();
        populateDataList(fieldsCbo.getSelectionIndex());
    }

    /**
     * Update values for selected item and update display.
     */
    private void updateSelectedItem() {
        if (dataList.getSelectionIndex() < 0) {
            return;
        }

        if (dataTF.getText().trim().length() == 0) {
            MessageBox mb = new MessageBox(getParent(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("Update Error");
            mb.setMessage("You need to enter something in the text field.");
            mb.open();
            return;
        }

        if (fieldsCbo.getSelectionIndex() == OWNER) {
            try {
                ReservoirOwnerData data = resOwnerData.get(dataList
                        .getSelectionIndex());
                HydroDBDataManager.getInstance().deleteRecord(data);

                ReservoirOwnerData newData = new ReservoirOwnerData();
                newData.setOwner(dataTF.getText().trim());
                HydroDBDataManager.getInstance().putData(newData);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Uable to update reservoir owner data. ", e);
                ;
            }
        } else if (fieldsCbo.getSelectionIndex() == TYPE) {
            try {
                DamTypeData data = damTypeData
                        .get(dataList.getSelectionIndex());
                HydroDBDataManager.getInstance().deleteRecord(data);

                DamTypeData newData = new DamTypeData();
                newData.setType(dataTF.getText().trim());
                HydroDBDataManager.getInstance().putData(newData);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Uable to update dam type data. ", e);
            }
        }

        getData();
        populateDataList(fieldsCbo.getSelectionIndex());
    }

    /**
     * Delete selected item and update the display.
     */
    private void deleteSelectedItem() {
        if (dataList.getSelectionIndex() < 0) {
            MessageBox mb = new MessageBox(getParent(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("Delete Error");
            mb.setMessage("You need to select an item from the list for deletion.");
            mb.open();
            return;
        }

        if (fieldsCbo.getSelectionIndex() == OWNER) {
            try {
                ReservoirOwnerData data = resOwnerData.get(dataList
                        .getSelectionIndex());
                HydroDBDataManager.getInstance().deleteRecord(data);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Uable to delete reservoir owner data. ", e);
            }
        } else if (fieldsCbo.getSelectionIndex() == TYPE) {
            try {
                DamTypeData data = damTypeData
                        .get(dataList.getSelectionIndex());
                HydroDBDataManager.getInstance().deleteRecord(data);
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Uable to delete dam type data. ", e);
            }
        }

        getData();
        populateDataList(fieldsCbo.getSelectionIndex());
    }
}
