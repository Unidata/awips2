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
package com.raytheon.viz.xdat;

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
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Site List dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 27 Oct 2008             lvenable    Initial creation.
 * 17 Jan 2008             lvenable    Updated code to format data display on
 *                                     the screen.
 * 10 Feb 2009             wkwock      Added functions.
 * 31 May 2015  4501       skorolev    Got rid of Vector.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class SiteListDlg extends CaveSWTDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Data list control.
     */
    private List dataListLst;

    /**
     * Label for the data list control.
     */
    private Label dataListLbl;

    /**
     * State combo box.
     */
    private Combo stateCbo;

    /**
     * Search text control.
     */
    private Text searchTF;

    /**
     * Data array.
     */
    private java.util.List<String[]> dataArray;

    /**
     * This is for TESTING PURPOSES ONLY. There can be a subset of states in the
     * state combo control.
     */
    String[] allStates = new String[] { "AK", "AL", "AR", "AZ", "CA", "CO",
            "CT", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY",
            "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND",
            "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI",
            "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY" };

    /**
     * Database manager.
     */
    private XdatDB databaseMgr;

    /**
     * Callback interface used to display the data on the screen.
     */
    private ITextDisplay displayCB;

    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     * @param pe
     *            Physical element.
     * @param database
     *            Database manager.
     * @param displayCB
     *            Display callback interface.
     */
    public SiteListDlg(Shell parentShell, XdatDB database,
            ITextDisplay displayCB) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE);
        setText("Site List");

        databaseMgr = database;
        this.displayCB = displayCB;
        allStates = database.getStateList();
    }

    @Override
    protected Layout constructShellLayout() {
        return new GridLayout(1, false);
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createControls();

        createCloseButton();

        populateStateCombo();
        updateDataListLabel(true);
    }

    @Override
    protected void preOpened() {
        shell.setMinimumSize(shell.getBounds().width, shell.getBounds().height);
    }

    /**
     * Create the main controls on the dialog.
     */
    private void createControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite controlComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        gl.verticalSpacing = 10;
        controlComp.setLayout(gl);
        controlComp.setLayoutData(gd);

        Label selStateLbl = new Label(controlComp, SWT.NONE);
        selStateLbl.setText("Select a State: ");

        gd = new GridData(70, SWT.DEFAULT);
        stateCbo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        stateCbo.setLayoutData(gd);

        stateCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateDataListLabel(true);
            }
        });

        Label searchLbl = new Label(controlComp, SWT.NONE);
        searchLbl.setText("Search: ");

        gd = new GridData(150, SWT.DEFAULT);
        searchTF = new Text(controlComp, SWT.BORDER);
        searchTF.setLayoutData(gd);
        searchTF.addListener(SWT.KeyUp, new Listener() {
            public void handleEvent(org.eclipse.swt.widgets.Event event) {
                if ((event.keyCode == SWT.CR)
                        || (event.keyCode == SWT.KEYPAD_CR)) {
                    updateDataListLabelWithSearchStr();
                }
            }

        });

        // Add a horizontal separator bar
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 2;
        Label sepLbl = new Label(controlComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        gd.horizontalIndent = 4;
        dataListLbl = new Label(controlComp, SWT.NONE);
        dataListLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 350;
        gd.heightHint = 400;
        gd.horizontalSpan = 2;
        dataListLst = new List(controlComp, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL | SWT.H_SCROLL);
        dataListLst.setLayoutData(gd);
        dataListLst.setFont(controlFont);

        dataListLst.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayListSelection();
            }
        });
    }

    /**
     * Create the close button at the bottom of the dialog.
     */
    private void createCloseButton() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
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
     * Populate the state combo box.
     */
    private void populateStateCombo() {
        /*
         * NOTE: The states combo box is dynamic. From the screen shots it
         * appears that other 2 character information appears in the combo box
         * other than the 50 US states.
         */
        stateCbo.add(" ");
        for (int i = 0; i < allStates.length; i++) {
            stateCbo.add(allStates[i]);
        }
        stateCbo.select(0);
    }

    /**
     * Update the label above the data list control.
     * 
     * @param state
     *            State flag. If true then the label reflects the data in the
     *            list control as being 'by state'. If false then the label
     *            reflects the data retrieved by the search string.
     */
    private void updateDataListLabelWithSearchStr() {
        String searchStr = null;
        String listFmt = "%-9S  %S";
        String searchFmt = "%d IDs with %s Data matching '%s'";
        String des = null;
        String peType = displayCB.getSelectedPE();

        stateCbo.select(0);
        searchStr = searchTF.getText().toUpperCase();

        dataArray = databaseMgr.search_string_list(searchStr, peType);
        dataListLst.removeAll();

        if (dataArray == null) {
            String msg = String.format(searchFmt, 0, peType, searchStr);
            dataListLbl.setText(msg);
            return;
        }

        for (String[] rowData : dataArray) {
            if (rowData[1] == null) {
                des = "";
            } else {
                des = rowData[1];
            }

            dataListLst.add(String.format(listFmt, rowData[0], des));
        }

        String str = String.format(searchFmt, dataArray.size(), peType,
                searchStr);
        dataListLbl.setText(str);
    }

    /**
     * Update the label above the data list control.
     * 
     * @param state
     *            State flag. If true then the label reflects the data in the
     *            list control as being 'by state'. If false then the label
     *            reflects the data retrieved by the search string.
     */
    private void updateDataListLabel(boolean state) {
        String pe = displayCB.getSelectedPE();
        String stateName = null;
        String listFmt = "%-9S  %S";
        String searchFmt = "%d IDs with %s Data matching '%s'";
        String des = null;

        searchTF.setText("");
        if (state == true) {
            stateName = stateCbo.getItem(stateCbo.getSelectionIndex());
        } else {
            stateName = searchTF.getText();
        }

        dataArray = databaseMgr.searchIds(stateName, pe);
        dataListLst.removeAll();

        if (dataArray == null) {
            String msg = String.format(searchFmt, 0, pe, stateName);
            dataListLbl.setText(msg);
            return;
        }

        for (String[] rowData : dataArray) {
            if (rowData[1] == null) {
                des = "";
            } else {
                des = rowData[1];
            }

            dataListLst.add(String.format(listFmt, rowData[0], des));
        }

        String str = String.format(searchFmt, dataArray.size(), pe, stateName);
        dataListLbl.setText(str);
    }

    /**
     * Display the data associate with the item selected in the data list.
     */
    private void displayListSelection() {

        // Get the PE type from main window
        String peType = displayCB.getSelectedPE();

        // Get the ID from the data array.
        int selectionIndex = dataListLst.getSelectionIndex();
        String selectedId = null;
        if (selectionIndex > -1) {
            selectedId = dataArray.get(selectionIndex)[0];
        }

        java.util.List<String[]> dataList = databaseMgr.getListData(selectedId,
                peType, displayCB.getStartDate(), displayCB.getEndDate());

        if (dataList == null) {
            String[] msg = new String[] { "No data available." };
            displayCB.setDisplayText(msg);
            return;
        }

        /*
         * Format the data.
         */
        String locationDes = databaseMgr.getLocationDes(selectedId);

        if (locationDes.equals("null")) {
            locationDes = "";
        }

        String[] displayData = new String[dataList.size() + 3]; // 1st three
        // lines for
        // header
        String dataFmt = "%-8S %2s   %-4S %2S %1S %19S %13S % 6.2f   % 6.2f";
        String displayHeader = " ID      PE  DUR   TS E       OBSTIME           PRODUCT    VALUE   CHANGE";
        String dashLine = "---------------------------------------------------------------------------";
        displayData[0] = "\t\t" + selectedId + "\t  " + locationDes;
        displayData[1] = displayHeader;
        displayData[2] = dashLine;

        for (int i = 0; i < dataList.size(); i++) {
            String[] rowData = dataList.get(i);

            double dblVal = Double.valueOf(rowData[5]);
            double rndVal = (Math.round(dblVal * 100.0)) / 100.0;
            double change = 0;

            if (i < (dataList.size() - 1)) { // if this is not the last one,
                                             // then
                // calculate the changes
                String[] nextRowData = dataList.get(i + 1);
                if ((Double.valueOf(rowData[5]) != HydroConstants.MISSING_VALUE)
                        && (Double.valueOf(nextRowData[5]) != HydroConstants.MISSING_VALUE)) {
                    change = Double.valueOf(rowData[5])
                            - Double.valueOf(nextRowData[5]);
                    change = (Math.round(change * 100.0)) / 100.0;
                }
            }

            String productID = rowData[0];
            if (productID == null) {
                productID = "";
            }

            displayData[i + 3] = String.format(dataFmt, selectedId, peType,
                    rowData[1], rowData[2], rowData[3], rowData[4], productID,
                    rndVal, change);
        }

        displayCB.setDisplayText(displayData);
    }
}
