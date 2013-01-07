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
package com.raytheon.uf.viz.datadelivery.common.ui;

import java.util.HashMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

import com.raytheon.uf.viz.core.notification.INotificationObserver;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.uf.viz.datadelivery.common.ui.SortImages.SortDirection;
import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils;

/**
 * This is the base table composite. This class is intended to be extended so
 * common table classes can be created and shared.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 13, 2012   687    lvenable     Initial creation
 * Jun 27, 2012   702    jpiatt       Updates for subscription groups.
 * Aug 15, 2012   430    jpiatt       Modified sort.
 * Aug 30, 2012  1120    jpiatt       Added clickSort flag.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public abstract class TableComp extends Composite implements INotificationObserver {

    /** Data table. */
    protected Table table;

    /** SortImages object. */
    protected SortImages sortImages;

    /** TableItem object. */
    private final Composite parentComp;

    /** Selected table column. */
    protected TableColumn sortedColumn = null;

    /** Sort direction map. */
    protected HashMap<String, SortDirection> sortDirectionMap = new HashMap<String, SortDirection>();

    /** Table configuration. */
    private final TableCompConfig tableConfig;

    /** Configuration changed flag. */
    protected boolean configChange = false;

    /**
     * Flag indicating if an observer should be added to the Notification
     * manager.
     */
    private boolean notifObserverFlag = true;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param tableConfig
     *            Table configuration.
     * @param notifObserverFlag
     *            Flag indicating if an observer should be added to the
     *            Notification manager.
     */
    public TableComp(Composite parent, TableCompConfig tableConfig, boolean notifObserverFlag) {
        super(parent, SWT.NONE);
        this.parentComp = parent;
        this.tableConfig = tableConfig;
        this.notifObserverFlag = notifObserverFlag;
        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        if (notifObserverFlag) {
            NotificationManagerJob.addObserver("notify.msg", this);
        }

        sortImages = new SortImages(this);

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

        createTable();

        parentComp.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                parentDisposed();
            }
        });
    }

    /**
     * Action to take when the parent is disposed.
     */
    private void parentDisposed() {
        if (notifObserverFlag) {
            NotificationManagerJob.removeObserver("notify.msg", this);
        }
    }

    /**
     * Create the table.
     */
    private void createTable() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);

        if (tableConfig.getTableHeight() > 0) {
            gd.heightHint = tableConfig.getTableHeight();
        }

        table = new Table(this, tableConfig.getTableStyle());
        table.setLayoutData(gd);
        table.setHeaderVisible(tableConfig.isHeaderVisible());
        table.setLinesVisible(tableConfig.isLinesVisible());

        table.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent event) {
                handleTableMouseClick(event);
            }
        });

        table.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                handleTableSelection(e);
            }
        });
    }

    /**
     * Pack the table columns.
     */
    public final void packColumns() {
        for (TableColumn tc : table.getColumns()) {
            tc.pack();
        }
    }

    /**
     * Update the column sort image to show the column sorted.
     */
    public final void updateColumnSortImage() {

        for (TableColumn tc : table.getColumns()) {
            tc.setImage(null);
        }

        SortDirection sortDir = getCurrentSortDirection();

        if (sortDir == null) {
            sortDir = SortDirection.ASCENDING;
        }

        if (table.getItemCount() > 0) {
            //Only change image if table is re-opened
            if (!configChange) {
                sortedColumn.setImage(sortImages.getImage(sortDir));
            } else if (sortDir == SortDirection.ASCENDING) {
                sortedColumn.setImage(sortImages.getImage(SortDirection.ASCENDING));
            } else {
                sortedColumn.setImage(sortImages.getImage(SortDirection.DESCENDING));
            } 

        }

        packColumns();
    }

    /**
     * Show or hide the tool tips for the table columns.
     * 
     * @param show
     *            Flag indicating if the tool tips should be shown.
     */
    public final void showColumnToolTips(boolean show) {
        if (show) {
            HashMap<String, String> toolTipsMap = DataDeliveryUtils.getColumnToolTipsMap(tableConfig.getTableType());

            for (TableColumn tc : table.getColumns()) {
                tc.setToolTipText(toolTipsMap.get(tc.getText()));
            }
        }
        else {
            for (TableColumn tc : table.getColumns()) {
                tc.setToolTipText(null);
            }
        }
    }

    /**
     * Update the sort direction.
     * 
     * @param tc
     *            Table column.
     * @param tableData
     *            Data that contains the sort data that will be changed.
     * @param clickSort
     *            Only change column sort direction on click
     */
    public void updateSortDirection(TableColumn tc, ISortTable tableData, boolean clickSort) {

        if (!configChange) {
            SortDirection sortDirection = SortDirection.ASCENDING;

            if (sortedColumn != null && tc.getText().equals(sortedColumn.getText())) {

                if (sortDirectionMap.containsKey(sortedColumn.getText())) {
                    sortDirection = sortDirectionMap.get(sortedColumn.getText());

                    if (clickSort) {
                        if (sortDirection == SortDirection.DESCENDING) {
                            sortDirection = SortDirection.ASCENDING;
                        }
                        else {
                            sortDirection = SortDirection.DESCENDING;
                        }
                    }

                }

                sortDirectionMap.put(sortedColumn.getText(), sortDirection);
                tableData.setSortDirection(sortDirection);
            }
            else {
                if (sortDirectionMap.containsKey(tc.getText())) {
                    tableData.setSortDirection(sortDirectionMap.get(tc.getText()));
                }
                else {
                    sortDirectionMap.put(sortedColumn.getText(), sortDirection);
                    tableData.setSortDirection(sortDirection);
                }
            }
        }

        sortedColumn = tc;
        tableData.setSortColumn(sortedColumn.getText());
    }

    /**
     * Get the current sort direction.
     * 
     * @return The current sort direction.
     */
    protected abstract SortDirection getCurrentSortDirection();

    /**
     * Create the table columns.
     */
    protected abstract void createColumns();

    /**
     * Populate the data table.
     */
    public abstract void populateTable();

    /**
     * Handle the mouse click on the table.
     * 
     * @param event
     */
    protected abstract void handleTableMouseClick(MouseEvent event);

    /**
     * Handle a selection on the table.
     * 
     * @param e
     *            Selection event.
     */
    protected abstract void handleTableSelection(SelectionEvent e);
}
