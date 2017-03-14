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
package com.raytheon.uf.viz.monitor.scan.commondialogs;

import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.common.monitor.scan.config.TrendSetConfigMgr;
import com.raytheon.uf.viz.monitor.scan.config.SCANConfig;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * 
 * Create/Edit dialog for creating and editing trend sets.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2009  #3039      lvenable     Initial creation
 * 24 Jul 2013  #2143      skorolev     Changes for non-blocking dialog.
 * Aug 15, 2013  2143      mpduff       Remove resize.
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class EditCreateTrendDlg extends CaveSWTDialog implements
        ICommonDialogAction {

    /**
     * List control displaying the available attributes.
     */
    private List availAttrList;

    /**
     * List control displaying a list of selected attributes.
     */
    private List selectedAttrList;

    /**
     * Add button.
     */
    private Button addBtn;

    /**
     * Remove button.
     */
    private Button removeBtn;

    /**
     * Remove all button.
     */
    private Button removeAllBtn;

    /**
     * List control displaying all of the available trend sets.
     */
    private List availTrendsList;

    /**
     * New trend sets button.
     */
    private Text newTrendTF;

    /**
     * Add new trend set button.
     */
    private Button addNewTrendBtn;

    /**
     * Remove trend set button.
     */
    private Button removeTrendBtn;

    /**
     * Update trend set button.
     */
    private Button updateTrendBtn;

    /**
     * Attribute button width.
     */
    private final int attrBtnWidth = 100;

    /**
     * SCAN table identifier.
     */
    private final ScanTables scanTable;

    /**
     * Flag indicating if the OK button was selected.
     */
    private boolean okSelected = false;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param scanTable
     *            SCAN table identifier.
     */
    public EditCreateTrendDlg(Shell parentShell, ScanTables scanTable) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Edit/Create Trend Sets");

        this.scanTable = scanTable;
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
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 10;
        return mainLayout;
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
        /*
         * Add a listener to handle the user clicking the close button in the
         * title bar.
         */
        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent e) {
                if (okSelected == false) {
                    handleCancelAction();
                }
            }
        });

        createAttributeControls();
        addSeparator(shell);
        createTrendControls();
        addSeparator(shell);
        createBottomButtons();

        populateAvailableTrendsList();
    }

    /**
     * Create the attribute controls.
     */
    private void createAttributeControls() {
        Composite attrComp = new Composite(shell, SWT.NONE);
        attrComp.setLayout(new GridLayout(3, false));
        attrComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        /*
         * Create the top labels.
         */
        Label availAtrrLbl = new Label(attrComp, SWT.NONE);
        availAtrrLbl.setText("Available\nAttributes:");

        // Filler label
        new Label(attrComp, SWT.NONE);

        Label selectedAtrrLbl = new Label(attrComp, SWT.NONE);
        selectedAtrrLbl.setText("Selected\nAttributes:");

        /*
         * Create the Available Attributes list control.
         */
        GridData gd = new GridData(100, 170);
        availAttrList = new List(attrComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        availAttrList.setLayoutData(gd);

        populateAvailableAttributeList();

        /*
         * Create the List controls.
         */
        Composite controlComp = new Composite(attrComp, SWT.NONE);
        controlComp.setLayout(new GridLayout(1, false));
        controlComp.setLayoutData(new GridData(SWT.DEFAULT, SWT.CENTER, false,
                true));

        gd = new GridData(attrBtnWidth, SWT.DEFAULT);
        addBtn = new Button(controlComp, SWT.PUSH);
        addBtn.setText("Add >>");
        addBtn.setLayoutData(gd);
        addBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleAddAttributeAction();
            }
        });

        gd = new GridData(attrBtnWidth, SWT.DEFAULT);
        removeBtn = new Button(controlComp, SWT.PUSH);
        removeBtn.setText("Remove");
        removeBtn.setLayoutData(gd);
        removeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleRemoveAttributeAction();
            }
        });

        gd = new GridData(attrBtnWidth, SWT.DEFAULT);
        removeAllBtn = new Button(controlComp, SWT.PUSH);
        removeAllBtn.setText("Remove All");
        removeAllBtn.setLayoutData(gd);
        removeAllBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleRemoveAllAttributesAction();
            }
        });

        gd = new GridData(attrBtnWidth, SWT.DEFAULT);
        updateTrendBtn = new Button(controlComp, SWT.PUSH);
        updateTrendBtn.setText("Update Trend");
        updateTrendBtn.setLayoutData(gd);
        updateTrendBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleUpdateTrendAction();
            }
        });

        /*
         * Create the Selected Attributes list control.
         */
        gd = new GridData(100, 170);
        selectedAttrList = new List(attrComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        selectedAttrList.setLayoutData(gd);
    }

    /**
     * Create the available trend controls.
     */
    private void createTrendControls() {
        Composite trendComp = new Composite(shell, SWT.NONE);
        trendComp.setLayout(new GridLayout(2, false));
        trendComp
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        Label availAtrrLbl = new Label(trendComp, SWT.NONE);
        availAtrrLbl.setText("Available\nTrends:");

        // Filler label
        new Label(trendComp, SWT.NONE);

        GridData gd = new GridData(100, 170);
        availTrendsList = new List(trendComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);
        availTrendsList.setLayoutData(gd);
        availTrendsList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleTrendListSelection();
            }
        });

        /*
         * Trend List Controls
         */
        Composite trendControlComp = new Composite(trendComp, SWT.NONE);
        trendControlComp.setLayout(new GridLayout(2, false));
        trendControlComp.setLayoutData(new GridData(SWT.DEFAULT, SWT.CENTER,
                false, true));

        gd = new GridData(100, SWT.DEFAULT);
        newTrendTF = new Text(trendControlComp, SWT.BORDER);
        newTrendTF.setLayoutData(gd);

        addNewTrendBtn = new Button(trendControlComp, SWT.PUSH);
        addNewTrendBtn.setText("Add New Trend");
        addNewTrendBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleAddNewTrend();
            }
        });

        gd = new GridData();
        gd.horizontalSpan = 2;
        removeTrendBtn = new Button(trendControlComp, SWT.PUSH);
        removeTrendBtn.setText("Remove Selected Trend");
        removeTrendBtn.setLayoutData(gd);
        removeTrendBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleRemoveTrendAction();
            }
        });
    }

    /**
     * Create the button at the bottom of the dialog.
     */
    private void createBottomButtons() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleOkAction();
            }
        });

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                closeDialog();
            }
        });
    }

    /**
     * Add a horizontal separator to the specified composite.
     * 
     * @param parentComp
     *            PArent composite.
     */
    private void addSeparator(Composite parentComp) {
        GridLayout gl = (GridLayout) parentComp.getLayout();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Populate the available attribute list control.
     */
    private void populateAvailableAttributeList() {
        String[] trendAttrNames = SCANConfig.getInstance().getTrendAttributes(
                scanTable);

        for (String attrName : trendAttrNames) {
            availAttrList.add(attrName);
        }

        if (availAttrList.getItemCount() > 0) {
            availAttrList.setSelection(0);
        }
    }

    /**
     * Populate the available trends list control.
     */
    private void populateAvailableTrendsList() {
        TrendSetConfigMgr trendCfgMgr = SCANConfig.getInstance()
                .getTrendConfigMgr(scanTable);

        Set<String> trendSetKeys = trendCfgMgr.getTrendSetMap().keySet();

        for (String key : trendSetKeys) {
            availTrendsList.add(key);
        }

        if (availTrendsList.getItemCount() > 0) {
            availTrendsList.setSelection(0);
            handleTrendListSelection();
        }
    }

    /**
     * Add a new attribute.
     */
    private void handleAddAttributeAction() {
        if (selectedAttrList.getItemCount() >= 5) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_INFORMATION | SWT.OK);
            mb.setText("Attribute Add");
            mb.setMessage("There can only be a maximum of 5 attributes in a trend set.");
            mb.open();
            return;
        }

        String attr = availAttrList.getItem(availAttrList.getSelectionIndex());

        if (selectedAttrList.indexOf(attr) < 0) {
            selectedAttrList.add(attr);
        }
    }

    /**
     * Remove the selected attribute.
     */
    private void handleRemoveAttributeAction() {
        int index = selectedAttrList.getSelectionIndex();

        if (index >= 0) {
            selectedAttrList.remove(index);
        }
    }

    /**
     * Remove all attributes from the attribute list control.
     */
    private void handleRemoveAllAttributesAction() {
        selectedAttrList.removeAll();
    }

    /**
     * Update the selected trend.
     */
    private void handleUpdateTrendAction() {
        String attributeStr = makeAttributesString();
        String trendName = availTrendsList.getItem(availTrendsList
                .getSelectionIndex());

        SCANConfig.getInstance().getTrendConfigMgr(scanTable)
                .addUpdateTrendSet(trendName, attributeStr);
    }

    /**
     * Method called when a user selects an item from the trend list control.
     */
    private void handleTrendListSelection() {

        String selectedTrend = availTrendsList.getItem(availTrendsList
                .getSelectionIndex());
        checkTrendListSelection(selectedTrend);

        TrendSetConfigMgr trendCfgMgr = SCANConfig.getInstance()
                .getTrendConfigMgr(scanTable);

        String attributes = trendCfgMgr.getTrendSetMap().get(selectedTrend);
        populateAttributeListWithTrendAttributes(attributes);
    }

    /**
     * Add a new trend..
     */
    private void handleAddNewTrend() {
        /*
         * TODO : check if the selection is exists if so alert the user
         */
        String newTrendName = newTrendTF.getText().trim();

        if (newTrendName.length() == 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Entry Error");
            mb.setMessage("Need to and a trend name to the entry field.");
            mb.open();
            return;
        }

        if (availTrendsList.indexOf(newTrendName) >= 0) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Entry Error");
            mb.setMessage("That trend name already exists.  Please enter a new one.");
            mb.open();

            newTrendTF.setFocus();
            newTrendTF.selectAll();

            return;
        }

        availTrendsList.add(newTrendName);
        availTrendsList.select(availTrendsList.indexOf(newTrendName));
        checkTrendListSelection(newTrendName);

        selectedAttrList.removeAll();
    }

    /**
     * Remove the selected trend.
     */
    private void handleRemoveTrendAction() {
        String selectedTrend = availTrendsList.getItem(availTrendsList
                .getSelectionIndex());

        SCANConfig.getInstance().getTrendConfigMgr(scanTable)
                .removeTrendSet(selectedTrend);

        availTrendsList.remove(selectedTrend);

        if (availTrendsList.getItemCount() >= 0) {
            availTrendsList.select(0);
            handleTrendListSelection();
        }
    }

    /**
     * OK button action that saves all of the trend sets and closes the dialog.
     */
    private void handleOkAction() {
        SCANConfig scanCfg = SCANConfig.getInstance();

        String[] invalidTrends = scanCfg.getTrendConfigMgr(scanTable)
                .getInvalidAttributeNumber();

        if (invalidTrends.length != 0) {
            StringBuilder sb = new StringBuilder();
            sb.append("The following trends have less than 2 attributes.\n\n");

            for (String str : invalidTrends) {
                sb.append(str).append("\n");
            }

            sb.append("\nDo you wish to continue?");

            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.YES
                    | SWT.NO);
            mb.setText("Attributes");
            mb.setMessage(sb.toString());
            int choice = mb.open();

            if (choice == SWT.NO) {
                return;
            }
        }

        handleUpdateTrendAction();
        scanCfg.getTrendConfigMgr(scanTable).saveTrendSets();

        okSelected = true;
        close();
    }

    /**
     * Close the dialog.
     */
    private void handleCancelAction() {
        SCANConfig.getInstance().getTrendConfigMgr(scanTable)
                .reloadConfiguration();
    }

    /**
     * Make a comma delimited string of attributes for the configuration data.
     * 
     * @return A comma delimited string of attributes.
     */
    private String makeAttributesString() {
        String[] items = selectedAttrList.getItems();

        StringBuilder sb = new StringBuilder();

        boolean firstTime = true;

        for (String item : items) {
            if (firstTime == false) {
                sb.append(",");
            }

            sb.append(item);
            firstTime = false;
        }

        return sb.toString();
    }

    /**
     * Populate the selected attributes list control with the attribute of the
     * selected trend set.
     * 
     * @param attributes
     *            String of comma delimited attribute names.
     */
    private void populateAttributeListWithTrendAttributes(String attributes) {
        selectedAttrList.removeAll();

        if (attributes == null) {
            return;
        }

        String[] attrs = attributes.split(",");

        for (String attr : attrs) {
            selectedAttrList.add(attr);
        }
    }

    /**
     * Check if the selected trend is a "default" trend set. If so then disable
     * the remove trend button.
     * 
     * @param selectedTrend
     *            The selected trend set.
     */
    private void checkTrendListSelection(String selectedTrend) {
        if (selectedTrend.compareTo("default") == 0) {
            removeTrendBtn.setEnabled(false);
        } else {
            removeTrendBtn.setEnabled(true);
        }
    }

    /**
     * Close the dialog.
     */
    @Override
    public void closeDialog() {
        handleCancelAction();
        close();
    }

}
