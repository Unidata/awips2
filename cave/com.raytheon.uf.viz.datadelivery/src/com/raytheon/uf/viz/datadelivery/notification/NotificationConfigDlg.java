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
package com.raytheon.uf.viz.datadelivery.notification;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;

import com.raytheon.uf.viz.datadelivery.common.ui.ITableChange;
import com.raytheon.uf.viz.datadelivery.common.xml.ColumnXML;
import com.raytheon.uf.viz.datadelivery.notification.PriorityImages.Priority;
import com.raytheon.uf.viz.datadelivery.notification.PriorityImages.PriorityDisplay;
import com.raytheon.uf.viz.datadelivery.notification.xml.MessageLoadXML;
import com.raytheon.uf.viz.datadelivery.notification.xml.NotificationConfigXML;
import com.raytheon.uf.viz.datadelivery.notification.xml.PrioritySettingXML;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.widgets.duallist.DualList;
import com.raytheon.viz.ui.widgets.duallist.DualListConfig;
import com.raytheon.viz.ui.widgets.duallist.IUpdate;

/**
 * Notification Configuration Dialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 2, 2012            mpduff     Initial creation
 * Feb 23, 2012           jpiatt     Remove save in handleOK method.
 * Apr 16, 2012   452     jpiatt     Added row configuration.
 * Jun  1, 2012   645     jpiatt     Added tooltips.
 * Jun 07, 2012   687     lvenable   Table data refactor.
 * Aug 08, 2012   863     jpiatt     Added new interface method.
 * Aug 13, 2012   430     jpiatt     Modifications for sort asc & desc.
 * Oct 22, 2012  1284     mpduff     Code Cleanup.
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class NotificationConfigDlg extends CaveSWTDialog implements IUpdate {

    /** Notification Config XML */
    private NotificationConfigXML xml;

    /** Callback to NotificationDialog */
    private final ITableChange callback;

    /** Sort Ascending Radio Button */
    private Button sortAscRdo;

    /** Sort Decending Radio Button */
    private Button sortDescRdo;

    /** Priority Display Color Radio Button */
    private Button colorRdo;

    /** Priority Display Color/Number Radio Button */
    private Button colorNumRdo;

    /** Priority Display Color/Number/Name Radio Button */
    private Button colorNumNameRdo;

    /** Priority Display Number Radio Button */
    private Button numRdo;

    /** Priority Display Number/Name Radio Button */
    private Button numNameRdo;

    /** All Messages Check Box */
    private Button allMsgChk;

    /** Load Last Hours Spinner */
    private Spinner spinner;

    /** Messages Radio Button */
    private Button msgRdo;

    /** Hours of Messages Radio Button */
    private Button hourRdo;

    /** Sort Combo box */
    private Combo sortColumnCbo;

    /** Page Combo box */
    private Combo rowNumCbo;

    /** Dual List Object */
    private DualList dualList;

    /** Dual List Object */
    private final String[] rows = { "20", "50", "100", "500", "1000", "5000" };

    /** Sort Ascending flag */
    private boolean sortAsc = false;

    /**
     * Constructor.
     *
     * @param parentShell
     * @param callback
     */
    public NotificationConfigDlg(Shell parentShell, ITableChange callback) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.INDEPENDENT_SHELL);
        setText("Notification Configuration");
        this.callback = callback;
    }

    /**
     * Initialize the composite.
     */
    @Override
    protected void initializeComponents(Shell shell) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        GridLayout gl = new GridLayout(1, false);

        shell.setLayout(gl);
        shell.setLayoutData(gd);

        readConfigFile();
        createStartupGroup();
        createDisplayGroup();
        createBottomButtons();
        loadLists();

    }

    /**
     * Read the configuration file
     */
    public void readConfigFile() {
        NotificationConfigManager configMan = NotificationConfigManager
                .getInstance();
        xml = configMan.getConfigXml();

        ArrayList<ColumnXML> col = xml.getColumnList();

        if (col != null) {
            for (ColumnXML column : col) {
                sortAsc = column.isSortAsc();
            }
        }
    }

    /**
     * Create the group used on startup.
     */
    private void createStartupGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(1, false);

        // Number of messages to load on startup
        Group startupGroup = new Group(shell, SWT.NONE);
        startupGroup.setLayout(gl);
        startupGroup.setLayoutData(gd);
        startupGroup.setText(" Initial Startup Configuration ");
        startupGroup
                .setToolTipText("Items which are loaded when the table is opened");

        // Load all messages check box
        allMsgChk = new Button(startupGroup, SWT.CHECK);
        allMsgChk.setText("Load All Messages");
        allMsgChk.setToolTipText("Load all available messages");
        allMsgChk.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleAllMsgChk();
            }
        });

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, true, false);
        gl = new GridLayout(3, false);
        Composite msgComp = new Composite(startupGroup, SWT.NONE);
        msgComp.setLayout(gl);
        msgComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, true, false);
        Label loadLbl = new Label(msgComp, SWT.NONE);
        loadLbl.setText("Load Last");
        loadLbl.setLayoutData(gd);

        // Message/Hour Number spinner
        gd = new GridData(25, SWT.DEFAULT);
        spinner = new Spinner(msgComp, SWT.BORDER);
        spinner.setMinimum(0);
        spinner.setMaximum(9999);
        spinner.setSelection(48);
        spinner.setIncrement(1);
        spinner.setPageIncrement(10);
        spinner.setToolTipText("Select number of messages or hours to load");

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, true, false);
        gl = new GridLayout(1, false);
        Composite msgComp2 = new Composite(msgComp, SWT.NONE);
        msgComp2.setLayout(gl);
        msgComp2.setLayoutData(gd);

        // Message radio button
        msgRdo = new Button(msgComp2, SWT.RADIO);
        msgRdo.setText("Messages");
        msgRdo.setToolTipText("Display last number of messages");

        // Hour radio button
        hourRdo = new Button(msgComp2, SWT.RADIO);
        hourRdo.setText("Hours of Messages");
        hourRdo.setSelection(true);
        hourRdo.setToolTipText("Display last number of hours");

        MessageLoadXML msgLoad = xml.getMessageLoad();
        if (msgLoad != null && msgLoad.isLoadAllMessages()) {
            allMsgChk.setSelection(true);
            spinner.setEnabled(false);
            msgRdo.setEnabled(false);
            hourRdo.setEnabled(false);
        } else {
            if (msgLoad != null) {
                spinner.setSelection(msgLoad.getLoadLast());
                msgRdo.setSelection(msgLoad.isNumMessages());
                hourRdo.setSelection(msgLoad.isNumHours());
            }
        }

        // Initial Sort Combo Box
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gl = new GridLayout(3, false);
        Composite sortComp = new Composite(startupGroup, SWT.NONE);
        sortComp.setLayout(gl);
        sortComp.setLayoutData(gd);

        Label label = new Label(sortComp, SWT.NONE);
        label.setText("Initial Sort Column:");

        sortColumnCbo = new Combo(sortComp, SWT.READ_ONLY);
        sortColumnCbo.setLayoutData(new GridData(150, SWT.DEFAULT));
        sortColumnCbo
                .setToolTipText("Select column to sort upon initial table load");

        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gl = new GridLayout(1, false);
        Composite sortDirComp = new Composite(sortComp, SWT.NONE);
        sortDirComp.setLayout(gl);
        sortDirComp.setLayoutData(gd);

        // Sort Ascending radio button
        sortAscRdo = new Button(sortDirComp, SWT.RADIO);
        sortAscRdo.setText("Sort Ascending");
        sortAscRdo.setToolTipText("Sort column from A-Z");

        // Sort Descending radio button
        sortDescRdo = new Button(sortDirComp, SWT.RADIO);
        sortDescRdo.setText("Sort Descending");
        sortDescRdo.setToolTipText("Sort the column from Z-A");

        sortDescRdo.setSelection(!sortAsc);
        sortAscRdo.setSelection(sortAsc);

    }

    /**
     * Create the group used for immediate display.
     */
    private void createDisplayGroup() {
        int pageSet = 0;
        int i = 0;

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, true);
        GridLayout gl = new GridLayout(1, false);

        Group displayGroup = new Group(shell, SWT.NONE);
        displayGroup.setLayout(gl);
        displayGroup.setLayoutData(gd);
        displayGroup.setText(" Display Configuration Settings ");
        displayGroup
                .setToolTipText("Items which refresh the table after clicking OK");

        // Pagination Combo Box
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gl = new GridLayout(3, false);
        Composite rowComp = new Composite(displayGroup, SWT.NONE);
        rowComp.setLayout(gl);
        rowComp.setLayoutData(gd);

        Label label = new Label(rowComp, SWT.NONE);
        label.setText("Rows Per Page:");

        // Select rows per page combo box
        pageSet = xml.getPaginationSetting();
        rowNumCbo = new Combo(rowComp, SWT.READ_ONLY);
        rowNumCbo.setLayoutData(new GridData(150, SWT.DEFAULT));
        rowNumCbo.setItems(rows);
        rowNumCbo
                .setToolTipText("Select number of table rows to display per page");

        for (String rowVal : rows) {
            if (!(Integer.parseInt(rowVal) == pageSet)) {
                i++;
            } else {
                break;
            }

        }

        rowNumCbo.select(i);

        // Priority settings
        gd = new GridData(SWT.LEFT, SWT.DEFAULT, true, false);
        gl = new GridLayout(3, true);
        Composite pComp = new Composite(displayGroup, SWT.NONE);
        pComp.setLayout(gl);
        pComp.setLayoutData(gd);

        PrioritySettingXML priority = xml.getPrioritySetting();
        PriorityImages pImages = new PriorityImages(shell);
        pImages.setPriorityDisplay(PriorityDisplay.Color);

        colorRdo = new Button(pComp, SWT.RADIO);
        colorRdo.setImage(pImages.getImage(Priority.Priority1));
        colorRdo.setToolTipText("Priority display color");
        pImages.setPriorityDisplay(PriorityDisplay.ColorNum);
        colorNumRdo = new Button(pComp, SWT.RADIO);
        colorNumRdo.setImage(pImages.getImage(Priority.Priority1));
        colorNumRdo.setToolTipText("Priority display color and number");
        pImages.setPriorityDisplay(PriorityDisplay.ColorNumName);
        colorNumNameRdo = new Button(pComp, SWT.RADIO);
        colorNumNameRdo.setImage(pImages.getImage(Priority.Priority1));
        colorNumNameRdo
                .setToolTipText("Priority display color, number and name");
        pImages.setPriorityDisplay(PriorityDisplay.Num);
        numRdo = new Button(pComp, SWT.RADIO);
        numRdo.setImage(pImages.getImage(Priority.Priority1));
        numRdo.setToolTipText("Priority display number");
        pImages.setPriorityDisplay(PriorityDisplay.NumName);
        numNameRdo = new Button(pComp, SWT.RADIO);
        numNameRdo.setImage(pImages.getImage(Priority.Priority1));
        numNameRdo.setToolTipText("Priority display number and name");

        if (priority.isColor()) {
            colorRdo.setSelection(true);
            colorNumRdo.setSelection(false);
            colorNumNameRdo.setSelection(false);
            numRdo.setSelection(false);
            numNameRdo.setSelection(false);
        } else if (priority.isColorNum()) {
            colorRdo.setSelection(false);
            colorNumRdo.setSelection(true);
            colorNumNameRdo.setSelection(false);
            numRdo.setSelection(false);
            numNameRdo.setSelection(false);
        } else if (priority.isColorNumName()) {
            colorRdo.setSelection(false);
            colorNumRdo.setSelection(false);
            colorNumNameRdo.setSelection(true);
            numRdo.setSelection(false);
            numNameRdo.setSelection(false);
        } else if (priority.isNum()) {
            colorRdo.setSelection(false);
            colorNumRdo.setSelection(false);
            colorNumNameRdo.setSelection(false);
            numRdo.setSelection(true);
            numNameRdo.setSelection(false);
        } else if (priority.isNumName()) {
            colorRdo.setSelection(false);
            colorNumRdo.setSelection(false);
            colorNumNameRdo.setSelection(false);
            numRdo.setSelection(false);
            numNameRdo.setSelection(true);
        }

        // Column settings
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gl = new GridLayout(1, false);

        Composite columnComp = new Composite(displayGroup, SWT.NONE);
        columnComp.setLayout(gl);
        columnComp.setLayoutData(gd);

        // column visibility settings
        xml.getColumnList();
        ArrayList<String> selectedList = new ArrayList<String>();
        ArrayList<String> fullList = new ArrayList<String>();
        for (ColumnXML col : xml.getColumnList()) {
            if (col.isVisible()) {
                selectedList.add(col.getName());
            }
            fullList.add(col.getName());
        }

        DualListConfig dualConfig = new DualListConfig();
        dualConfig.setListHeight(150);
        dualConfig.setListWidth(100);
        dualConfig.setAvailableListLabel("Hidden Columns:");
        dualConfig.setSelectedListLabel("Visible Columns:");
        dualConfig.setShowUpDownBtns(true);
        dualConfig.setSelectedList(selectedList);
        dualConfig.setFullList(fullList);

        dualList = new DualList(columnComp, SWT.NONE, dualConfig, this);
    }

    /**
     * Create the buttons.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(6, false);

        Composite bottomComp = new Composite(shell, SWT.NONE);
        bottomComp.setLayout(gl);
        bottomComp.setLayoutData(gd);

        // OK button
        int buttonWidth = 75;
        GridData btnData = new GridData(buttonWidth, SWT.DEFAULT);
        Button okBtn = new Button(bottomComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(btnData);
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                handleOK();
            }
        });

        // Cancel button
        Button closeBtn = new Button(bottomComp, SWT.PUSH);
        closeBtn.setText("Cancel");
        closeBtn.setLayoutData(btnData);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Handle the OK button action.
     */
    private void handleOK() {

        // return if all columns set to hidden
        if (dualList.getSelectedListItems().length == 0) {
            DataDeliveryUtils
                    .showMessage(shell, SWT.ERROR, "No Columns Visible",
                            "No columns are visible.  At least one column must be visible.");
            return;
        }

        NotificationConfigManager configManager = NotificationConfigManager
                .getInstance();
        NotificationConfigXML xml = configManager.getConfigXml();

        xml.clearColumns();

        String sortCol = sortColumnCbo.getItem(sortColumnCbo
                .getSelectionIndex());

        String[] selectedColumns = dualList.getSelectedListItems();
        String[] availableColumns = dualList.getAvailableListItems();

        for (String columnName : selectedColumns) {
            updateColumnXMl(xml, columnName, sortCol, true);
        }

        for (String columnName : availableColumns) {
            updateColumnXMl(xml, columnName, sortCol, false);
        }

        String rowNum = rowNumCbo.getItem(rowNumCbo.getSelectionIndex());
        int rowNumVal = Integer.parseInt(rowNum);
        xml.setPaginationSetting(rowNumVal);

        PrioritySettingXML priority = new PrioritySettingXML();
        if (colorRdo.getSelection()) {
            priority.setColor(true);
        } else if (colorNumRdo.getSelection()) {
            priority.setColorNum(true);
        } else if (colorNumNameRdo.getSelection()) {
            priority.setColorNumName(true);
        } else if (numNameRdo.getSelection()) {
            priority.setNumName(true);
        } else if (numRdo.getSelection()) {
            priority.setNum(true);
        }

        if (!colorNumNameRdo.getSelection()) {
            priority.setColorNumName(false);
        }

        xml.setPrioritySetting(priority);

        MessageLoadXML messageLoad = new MessageLoadXML();
        if (allMsgChk.getSelection()) {
            messageLoad.setLoadAllMessages(true);
            messageLoad.setNumHours(false);
            messageLoad.setNumMessages(false);
        } else {
            messageLoad.setLoadAllMessages(false);
            messageLoad.setLoadLast(spinner.getSelection());
            messageLoad.setNumHours(hourRdo.getSelection());
            messageLoad.setNumMessages(msgRdo.getSelection());
        }

        xml.setMessageLoad(messageLoad);
        configManager.setConfigXml(xml);

        callback.tableChanged();
        close();
    }

    private void updateColumnXMl(NotificationConfigXML xml, String columnName,
            String sortCol, boolean visible) {
        ColumnXML col = new ColumnXML();
        col.setName(columnName);
        col.setVisible(visible);

        if (columnName.equalsIgnoreCase(sortCol)) {
            col.setSortColumn(true);
            col.setSortAsc(sortAscRdo.getSelection());
        } else {
            col.setSortColumn(false);
        }

        xml.addColumn(col);
    }

    /**
     * Handle the All Messages check box
     */
    private void handleAllMsgChk() {
        if (allMsgChk.getSelection()) {
            spinner.setEnabled(false);
            msgRdo.setEnabled(false);
            hourRdo.setEnabled(false);
        } else {
            spinner.setEnabled(true);
            msgRdo.setEnabled(true);
            hourRdo.setEnabled(true);
        }
    }

    /**
     * Load the column data into the lists
     */
    private void loadLists() {
        ArrayList<ColumnXML> columns = xml.getColumnList();
        String sort = null;
        for (ColumnXML column : columns) {
            if (column.isSortColumn()) {
                sort = column.getName();
                break;
            }
        }

        String[] selectedItems = dualList.getSelectedListItems();
        sortColumnCbo.setItems(selectedItems);
        for (int i = 0; i < selectedItems.length; i++) {
            String item = selectedItems[i];
            if (item.equalsIgnoreCase(sort)) {
                sortColumnCbo.select(i);
                break;
            }
        }

        if (sortColumnCbo.getSelectionIndex() == -1) {
            sortColumnCbo.select(0);
        }
    }

    /**
     * Handle the combo box with data.
     *
     * @param entries
     *            true if data in the combo box.
     */
    @Override
    public void hasEntries(boolean entries) {
        int idx = sortColumnCbo.getSelectionIndex();
        String sortCol = null;
        if (idx > -1) {
            sortCol = sortColumnCbo.getItem(idx);
        }
        sortColumnCbo.removeAll();
        if (entries) {
            if (dualList != null && dualList.getSelectedListItems() != null) {
                sortColumnCbo.setItems(dualList.getSelectedListItems());

                if (sortCol != null) {
                    String[] items = sortColumnCbo.getItems();
                    for (int i = 0; i < items.length; i++) {
                        String item = items[i];
                        if (sortCol.equals(item)) {
                            sortColumnCbo.select(i);
                            break;
                        }
                    }
                }

                if (sortColumnCbo.getSelectionIndex() == -1) {
                    if (dualList.getSelectedListItems().length > 0) {
                        sortColumnCbo.select(0);
                    }
                }
            }
        }
    }

    @Override
    public void selectionChanged() {
        // unused
    }

}