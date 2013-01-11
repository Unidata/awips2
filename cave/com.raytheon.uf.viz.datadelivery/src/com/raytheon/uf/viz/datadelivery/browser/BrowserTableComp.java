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
package com.raytheon.uf.viz.datadelivery.browser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.datadelivery.registry.DataSet;
import com.raytheon.uf.common.datadelivery.registry.GriddedDataSet;
import com.raytheon.uf.common.datadelivery.registry.Parameter;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.datadelivery.common.ui.IDialogClosed;
import com.raytheon.uf.viz.datadelivery.common.ui.SortImages.SortDirection;
import com.raytheon.uf.viz.datadelivery.common.ui.SubscriptionViewer;
import com.raytheon.uf.viz.datadelivery.common.ui.TableComp;
import com.raytheon.uf.viz.datadelivery.common.ui.TableCompConfig;
import com.raytheon.uf.viz.datadelivery.common.ui.TableDataManager;
import com.raytheon.uf.viz.datadelivery.common.ui.ViewDetailsDlg;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.BrowserColumnNames;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.TABLE_TYPE;

/**
 * 
 * Class that contains the data table. The data table will hold the data sets
 * that are retrieved when the filters are filled out and the data is retrieved.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2012            lvenable     Initial creation.
 * Jun  1, 2012    645     jpiatt       Added tooltips.
 * Jun 07, 2012    687     lvenable     Table data refactor.
 * Jun 21, 2012    736     djohnson     Accept List instead of ArrayList.
 * Jun 22, 2012    687     lvenable     Table data refactor.
 * Jul 24, 2012    955     djohnson     Accept List instead of ArrayList.
 * Aug 10, 2012   1022     djohnson     Use GriddedDataSet.
 * Aug 20, 2012   0743     djohnson     Finish making registry type-safe.
 * Aug 30, 2012   1120     jpiatt       Added clickSort flag.
 * Oct 05, 2012   1241     djohnson     Replace RegistryManager calls with registry handler calls.
 * Jan 10, 2013   1346     mpduff       Add additional information to the dataset details output.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class BrowserTableComp extends TableComp implements IDialogClosed {
    /** Status Handler */
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BrowserTableComp.class);

    /** Callback called when the table is updated. */
    private final IDataTableUpdate updateCallback;

    /** Mouse pointer object. */
    private final Point mousePt = new Point(0, 0);

    /** Pop up menu object. */
    private Menu popupMenu;

    /** View details map. */
    private final HashMap<String, ViewDetailsDlg> detailsDlgMap = new HashMap<String, ViewDetailsDlg>();

    /** View subscription map. */
    private final HashMap<String, SubscriptionViewer> subscriptionDlgMap = new HashMap<String, SubscriptionViewer>();

    /** Lock details flag. */
    private boolean lockDetailsDlgMap = false;

    /** Browser table data. */
    private TableDataManager<BrowserTableRowData> tableData = null;

    /** TableItem object. */
    private TableItem rightClickTableItem;

    /** TableColumn object. */
    private TableColumn tc;

    /** String array containing column. */
    private String[] columns = null;

    /** TableItem object. */
    private final Composite parentComp;

    /** Array of DataSet objects */
    private List<DataSet> dataList = null;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param tableConfig
     *            Table configuration.
     * @param updateCallback
     *            Callback for the table updates.
     */
    public BrowserTableComp(Composite parent, TableCompConfig tableConfig,
            IDataTableUpdate updateCallback) {
        super(parent, tableConfig, false);

        this.updateCallback = updateCallback;
        this.parentComp = parent;
        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        /*
         * Setup the layout for the composite
         */
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, true);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);

        createColumns();
    }

    /**
     * Called when the table row has changed.
     */
    private void updateTableRowChanged() {
        updateCallback.tableRowsChanged();
    }

    /**
     * Show or hide the tool tip for the table columns.
     * 
     * @param flag
     *            Show/Hide the tooltips.
     */
    public void showToolTips(boolean flag) {
        showColumnToolTips(flag);
    }

    /**
     * Create the popup menu.
     */
    private void createPopupMenu() {
        if (popupMenu != null) {
            popupMenu.dispose();
        }

        popupMenu = new Menu(table);

        MenuItem viewSubscriptionsMI = new MenuItem(popupMenu, SWT.NONE);
        viewSubscriptionsMI.setText("View Subscriptions...");
        viewSubscriptionsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displayViewSubscriptions();
            }
        });

        MenuItem viewMoreInfoMI = new MenuItem(popupMenu, SWT.NONE);
        viewMoreInfoMI.setText("View Dataset Information...");
        viewMoreInfoMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                displayViewMoreInformation();
            }
        });

        // Set the pop-up menu as the pop-up for the shell
        table.setMenu(popupMenu);
    }

    /**
     * Display the subscription viewer that will display the subscription for
     * the selected data set.
     */
    private void displayViewSubscriptions() {

        TableItem[] items = table.getSelection();

        if (items.length == 0) {
            return;
        }

        int index = table.indexOf(rightClickTableItem);

        DataSet dataSet = tableData.getDataArray().get(index).getDataset();

        String id = "";

        String dataSetName = dataSet.getDataSetName();
        if (dataSetName != null) {
            id = dataSetName;
        }
        String providerName = dataSet.getProviderName();

        if (subscriptionDlgMap.containsKey(id) == true) {
            subscriptionDlgMap.get(id).bringToTop();
        } else {
            SubscriptionViewer viewer = new SubscriptionViewer(this.getShell(),
                    providerName, this, dataSetName);
            viewer.open();
            subscriptionDlgMap.put(dataSetName, viewer);
        }
    }

    /**
     * Display the more details dialog.
     */
    private void displayViewMoreInformation() {
        TableItem[] items = table.getSelection();

        if (items.length == 0) {
            return;
        }

        int index = table.indexOf(rightClickTableItem);

        DataSet dsmd = tableData.getDataArray().get(index).getDataset();

        String id = "";

        if (dsmd.getDataSetName() != null) {
            id = dsmd.getDataSetName();
        }

        if (detailsDlgMap.containsKey(id) == true) {
            detailsDlgMap.get(id).bringToTop();
        } else {

            String formattedData = formatDatasetData(dsmd);

            String title = "More Information - (" + dsmd.getDataSetName() + ")";
            ViewDetailsDlg mid = new ViewDetailsDlg(parentComp.getShell(),
                    formattedData, title, this, id);
            mid.open();
            detailsDlgMap.put(id, mid);
        }
    }

    /**
     * Format the dataset data.
     * 
     * @param dataSet
     *            The dataset
     * @return
     */
    private String formatDatasetData(DataSet dataSet) {
        StringBuilder sb = new StringBuilder(150);
        sb.append("Dataset Name: ")
                .append(validateString(dataSet.getDataSetName()))
                .append(Util.EOL);
        sb.append("Dataset Type: ").append(dataSet.getDataSetType().toString())
                .append(Util.EOL);
        sb.append("Coverage: ").append(Util.EOL);

        sb.append("--- Projection  : ")
                .append(validateString(dataSet.getCoverage().getProjection()))
                .append(Util.EOL);

        sb.append("--- UpperLeft (Lon) : ")
                .append(dataSet.getCoverage().getUpperLeft().x)
                .append(Util.EOL);
        sb.append("--- UpperLeft (Lat) : ")
                .append(dataSet.getCoverage().getUpperLeft().y)
                .append(Util.EOL);
        sb.append("--- LowerRight (Lon): ")
                .append(dataSet.getCoverage().getLowerRight().x)
                .append(Util.EOL);
        sb.append("--- LowerRight (Lat): ")
                .append(dataSet.getCoverage().getLowerRight().y)
                .append(Util.EOL);
        sb.append("\n");

        if (dataSet instanceof GriddedDataSet) {
            GriddedDataSet gds = (GriddedDataSet) dataSet;
            List<Integer> cycleList = new ArrayList<Integer>(gds.getCycles());
            if (!cycleList.isEmpty()) {
                sb.append("Dataset Cycles: ");
                Collections.sort(cycleList);
                for (int i : cycleList) {
                    sb.append(i).append(" ");
                }
                sb.append(Util.EOL);
            }

            List<Integer> fcstHrs = new ArrayList<Integer>(
                    gds.getForecastHours());
            if (!fcstHrs.isEmpty()) {
                sb.append("Forecast Hours: ");

                Collections.sort(fcstHrs);

                StringBuilder hrBuffer = new StringBuilder();
                for (int i : fcstHrs) {
                    hrBuffer.append(i).append(" ");
                    // wrap at 50 characters
                    if (hrBuffer.length() > 50) {
                        sb.append(hrBuffer).append(Util.EOL);
                        sb.append("                ");
                        hrBuffer.setLength(0);
                    }
                }

                sb.append(Util.EOL);
            }
        }

        Map<String, Parameter> paramMap = dataSet.getParameters();
        if (!paramMap.isEmpty()) {
            sb.append("Parameters:").append(Util.EOL);
            List<String> paramList = new ArrayList<String>(paramMap.keySet());
            Collections.sort(paramList, String.CASE_INSENSITIVE_ORDER);

            // Get the largest parameter name for alignment
            int max = 0;
            for (String param : paramList) {
                if (max < param.length()) {
                    max = param.length();
                }
            }

            for (String param : paramList) {
                sb.append("--- ").append(param);
                sb.append(getSpacing(max + 1, param));
                sb.append(paramMap.get(param).getDefinition()).append(Util.EOL);
            }
        }

        return sb.toString();
    }

    /**
     * Generate the spacing of the parameter definitions.
     * 
     * @param start
     *            the point at which the string should start
     * @param the
     *            parameter name
     */
    private String getSpacing(int start, String param) {
        int numSpaces = start - param.length();
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < numSpaces; i++) {
            sb.append(" ");
        }
        return sb.toString();
    }

    /**
     * Validate string to verify it is not null.
     * 
     * @param str
     *            String to be verified.
     * @return The validated string.
     */
    private String validateString(String str) {
        if (str == null) {
            return "Unknown";
        }

        return str;
    }

    /**
     * Get the number of items in the table.
     * 
     * @return The number of items in the table.
     */
    public int getTableItemCount() {
        return table.getItemCount();
    }

    /**
     * Get the metadata for the selected dataset.
     * 
     * @return metatdata
     */
    public DataSet getSelectedDataset() {
        if (table.getSelectionCount() > 0) {
            int idx = this.table.getSelectionIndex();
            return tableData.getDataArray().get(idx).getDataset();
        }

        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.common.ui.IDialogClosed#dialogClosed
     * (java.lang.String)
     */
    @Override
    public void dialogClosed(String id) {

        if (lockDetailsDlgMap == true) {
            return;
        }

        if (detailsDlgMap.containsKey(id)) {
            detailsDlgMap.remove(id);
        }

        if (subscriptionDlgMap.containsKey(id)) {
            subscriptionDlgMap.remove(id);
        }
    }

    /**
     * Close all of the viewer dialogs.
     */
    private void closeAllViewerDialogs() {
        lockDetailsDlgMap = true;

        /*
         * Close all of the detail dialogs.
         */
        Set<String> keys = detailsDlgMap.keySet();

        for (String key : keys) {
            detailsDlgMap.get(key).close();
        }

        detailsDlgMap.clear();

        /*
         * Close all of the subscription dialogs.
         */
        keys = subscriptionDlgMap.keySet();

        for (String key : keys) {
            subscriptionDlgMap.get(key).close();
        }

        subscriptionDlgMap.clear();

        lockDetailsDlgMap = false;
    }

    /**
     * Handle the column selection.
     * 
     * @param event
     *            Selection Event.
     */
    private void handleColumnSelection(SelectionEvent event) {

        if (tableData == null) {
            return;
        }

        TableColumn tc = (TableColumn) event.getSource();

        updateSortDirection(tc, tableData, true);

        sortTable();
        updateColumnSortImage();
    }

    /**
     * Sort the rows of data in the table.
     */
    private void sortTable() {
        table.clearAll();
        table.removeAll();

        tableData.sortData();

        ArrayList<BrowserTableRowData> btrdArray = tableData.getDataArray();

        for (BrowserTableRowData btrd : btrdArray) {
            TableItem ti = new TableItem(this.table, SWT.NONE);
            ti.setText(BrowserColumnNames.NAME.ordinal(), btrd.getDataSetName());
            ti.setText(BrowserColumnNames.SUBSCRIPTION.ordinal(),
                    btrd.getSubscriptionName());
            ti.setText(BrowserColumnNames.PROVIDER.ordinal(),
                    btrd.getProviderName());
        }
    }

    /**
     * Clear all table rows.
     */
    public void clearTableEntries() {
        tableData = null;
        closeAllViewerDialogs();
        table.removeAll();

        // Remove any sort image from the table.
        for (TableColumn tc : table.getColumns()) {
            tc.setImage(null);
        }

        handleTableSelection(null);
        updateTableRowChanged();
    }

    /**
     * Update the table.
     * 
     * @param dataList
     *            Array of dataset metadata.
     */
    public void updateTable(List<DataSet> dataList) {
        this.dataList = dataList;

        populateTable();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.common.ui.TableComp#getCurrentSortDirection
     * ()
     */
    @Override
    protected SortDirection getCurrentSortDirection() {
        return tableData.getSortDirection();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.datadelivery.common.ui.TableComp#createColumns()
     */
    @Override
    protected void createColumns() {
        columns = new String[BrowserColumnNames.values().length];
        for (int i = 0; i < columns.length; i++) {
            columns[i] = BrowserColumnNames.values()[i].getColumnName();
        }

        for (int i = 0; i < columns.length; i++) {
            tc = new TableColumn(table, SWT.LEFT);
            tc.setText(columns[i]);

            if (i == 1) {
                tc.setAlignment(SWT.CENTER);
            }

            tc.setResizable(true);

            tc.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    handleColumnSelection(event);

                }
            });
        }

        packColumns();

        for (BrowserColumnNames tcn : BrowserColumnNames.values()) {
            sortDirectionMap.put(tcn.getColumnName(), SortDirection.ASCENDING);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.datadelivery.common.ui.TableComp#populateTable()
     */
    @Override
    public void populateTable() {
        if (tableData != null) {
            tableData = null;
        }

        tableData = new TableDataManager<BrowserTableRowData>(
                TABLE_TYPE.BROWSER);
        closeAllViewerDialogs();

        Set<String> datasetNames = Collections.emptySet();
        try {
            datasetNames = DataDeliveryHandlers.getSubscriptionHandler()
                    .getSubscribedToDataSetNames();
        } catch (RegistryHandlerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to retrieve subscription dataset names!", e);
        }

        /*
         * Populate the table with the data.
         */
        for (DataSet data : dataList) {

            BrowserTableRowData btrd = new BrowserTableRowData();
            btrd.setDataset(data);
            btrd.setDataSetName(data.getDataSetName());
            btrd.setProviderName(data.getProviderName());

            if (datasetNames.contains(data.getDataSetName())) {
                btrd.setSubscriptionName("Y");
            } else {
                btrd.setSubscriptionName("N");
            }

            tableData.addDataRow(btrd);
        }

        if (this.sortedColumn == null) {
            sortedColumn = table.getColumn(BrowserColumnNames.NAME.ordinal());
            tableData.setSortColumn(BrowserColumnNames.NAME.getColumnName());
        } else {
            tableData.setSortColumn(sortedColumn.getText());
        }

        // Sort & update the table
        sortTable();
        updateColumnSortImage();
        updateTableRowChanged();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.datadelivery.common.ui.TableComp#handleTableMouseClick
     * (org.eclipse.swt.events.MouseEvent)
     */
    @Override
    protected void handleTableMouseClick(MouseEvent me) {
        if (me.button == 3) {
            mousePt.x = me.x;
            mousePt.y = me.y;
            rightClickTableItem = table.getItem(mousePt);

            if (rightClickTableItem == null) {
                return;
            }

            createPopupMenu();
            popupMenu.setVisible(true);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.datadelivery.common.ui.TableComp#
     * handleTableSelectionChange(org.eclipse.swt.events.SelectionEvent)
     */
    @Override
    protected void handleTableSelection(SelectionEvent e) {
        if (table.getSelectionCount() > 0) {
            updateCallback.tableSelectionChanged(true);
        } else {
            updateCallback.tableSelectionChanged(false);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.notification.INotificationObserver#
     * notificationArrived
     * (com.raytheon.uf.viz.core.notification.NotificationMessage[])
     */
    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        // NOT USED.
    }
}
