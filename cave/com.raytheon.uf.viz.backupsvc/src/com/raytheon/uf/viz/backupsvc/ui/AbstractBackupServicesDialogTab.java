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
package com.raytheon.uf.viz.backupsvc.ui;

import java.text.SimpleDateFormat;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.VizApp;

/**
 * Abstract Base Class for both the Outgoing and Incoming tabs on the Service
 * Backup Dialog.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2021 84467      Robert.Blum      Initial creation
 * Jul 01, 2021 93517      Amanuel Challa   Removed filterJobs()
 * Jul 29, 2021 92922      Robert.Blum      Added size hints to table.
 * Jun 27, 2023 2034205    Amanuel.Challa   Removed selectedTableItems and added getSelectedTableItems()
 * </pre>
 *
 * @author Robert.Blum
 * @version 1.0
 */

public abstract class AbstractBackupServicesDialogTab {

    protected final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractBackupServicesDialogTab.class);

    /** Horizontal spacing between buttons */
    protected static final int HORIZONTAL_BUTTON_SPACING = 65;

    /**
     * Date-time format string.
     */
    private final static String DATE_TIME_FORMAT_STRING = "HH:mm'Z' dd-MMM-yyyy";

    /**
     * Date formatter for date-time strings.
     */
    protected final static SimpleDateFormat dateTimeFormatter = new SimpleDateFormat(
            DATE_TIME_FORMAT_STRING);

    protected TabItem tab;

    protected BackupServicesFileViewerDialog fileViewerDialog;

    protected Table table;

    protected Text statusText;

    protected String statusMsgText;

    /**
     * Reverse sort flag.
     */
    protected boolean reverseSort = false;

    public AbstractBackupServicesDialogTab(TabFolder parent, int style) {
        tab = new TabItem(parent, style);
        dateTimeFormatter.setTimeZone(TimeUtil.GMT_TIME_ZONE);

        Composite tabComp = new Composite(parent, SWT.NONE);
        GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        tabComp.setLayoutData(gridData);
        GridLayout layout = new GridLayout(1, false);
        tabComp.setLayout(layout);
        tab.setControl(tabComp);

        createTable(tabComp);
        createStatusText(tabComp);
        createLogWindow();
        createButtons(tabComp);
    }

    /**
     * Creates the log window
     */
    protected abstract void createLogWindow();

    /**
     * Queries the BackupSvcOutgoing and BackupSvcIncoming jobs to populate the
     * dialog.
     */
    protected abstract void queryJobs();

    /**
     * Clears and populates the table with the current filtered BackupSvc jobs.
     */
    protected abstract void populateTable();

    /**
     * Sorts the table by the provided column.
     *
     * @param column
     *            The column to sort on
     */
    protected abstract void sortColumnData(String column);

    /**
     * Creates the buttons for the Tab.
     *
     * @param tabComp
     *            Composite to be used as the parent
     */
    protected abstract void createButtons(Composite tabComp);

    /**
     * Gets the column headers for the table.
     *
     * @return Array of column headers..
     */
    protected abstract String[] getTableColumns();

    /**
     * Creates the main table that populates this TabItem and holds the
     * BackupSvc jobs.
     *
     * @param tabComp
     *            Composite used as the parent
     */
    private void createTable(Composite tabComp) {
        table = new Table(tabComp,
                SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL | SWT.MULTI);
        table.setHeaderVisible(true);
        table.setLinesVisible(true);

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 400;
        gd.widthHint = 1000;
        table.setLayoutData(gd);

        TableColumn sortColumn = null;

        // Create the Table Columns
        String[] columns = getTableColumns();
        for (String column2 : columns) {
            TableColumn column = new TableColumn(table, SWT.NULL);
            column.setText(column2);

            column.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    handleColumnSelection(event);
                }
            });

            if (column.getText().equals(getDefaultSortColumn())) {
                sortColumn = column;
            }
        }

        table.setSortColumn(sortColumn);
        table.setSortDirection(SWT.UP);
        packColumns();

    }

    /**
     * TableItems that are currently selected.
     *
     * @return currently selected items
     */
    public TableItem[] getSelectedTableItems() {
        return table.getSelection();
    }

    /**
     * Gets the default column to sort the table by.
     *
     * @return the default sort column
     */
    private String getDefaultSortColumn() {
        // Default sort is ID field
        return "ID";
    }

    /**
     * Goes through each table column and packs it.
     */
    protected void packColumns() {
        for (int i = 0; i < table.getColumnCount(); i++) {
            table.getColumn(i).pack();
        }
    }

    /**
     * Creates the Text widget used for the status messages.
     *
     * @param tabComp
     *            Composite used as the parent
     */
    protected void createStatusText(Composite tabComp) {
        statusText = new Text(tabComp, SWT.READ_ONLY);

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        statusText.setLayoutData(gd);
    }

    /**
     * Updates the status text widget with the currently set status message.
     */
    protected void updateStatusText() {
        statusText.setText(statusMsgText);
    }

    /**
     * Handles the sorting actions when a column header is selected.
     *
     * @param e
     *            SelectionEvent of the action
     */
    protected void handleColumnSelection(SelectionEvent e) {
        TableColumn column = (TableColumn) e.widget;
        TableColumn sortColumn = table.getSortColumn();

        if (column.equals(sortColumn) && (table.getSortDirection() == SWT.UP)) {
            table.setSortDirection(SWT.DOWN);
            reverseSort = true;
        } else {
            table.setSortDirection(SWT.UP);
            reverseSort = false;
        }
        table.setSortColumn(column);

        sortColumnData(column.getText());
    }

    /**
     * Refreshes the table by querying for the current BackupSvc Jobs and
     * populating the table.
     */
    public void handleRefresh() {
        queryJobs();

        // execute populateTable() via VizApp.runAsync() to ensure all UI
        // updates have to be done on the UI thread.
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                populateTable();
            }
        });

    }
}
