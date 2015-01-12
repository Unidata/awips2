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
package com.raytheon.uf.viz.monitor.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig;
import com.raytheon.uf.viz.monitor.data.ColumnAttribData;
import com.raytheon.uf.viz.monitor.data.TableData;
import com.raytheon.viz.ui.EditorUtil;

/**
 * 
 * Station table composite.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2009            lvenable     Initial creation
 * Dec 18, 2009 3424      zhao         Implemented tableColRightMouseAction
 * Sep 04, 2014 3220      skorolev     Cleaned code.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class StationTableComp extends TableComp {
    /**
     * zone ID.
     */
    private String id = "Unknown";

    /**
     * zone ID label.
     */
    private Label idLbl;

    /**
     * Table callback.
     */
    private IStationTableAction tableCallback;

    /**
     * Control composite.
     */
    private Composite controlComp;

    /**
     * Mouse point.
     */
    private Point mousePt = new Point(0, 0);

    /**
     * Common table configuration.
     */
    private CommonTableConfig tableConfig;

    /**
     * Constructor.
     * 
     * @param paren
     *            Parent composite.
     * @param data
     *            Table data.
     * @param appName
     *            Application name.
     * @param callback
     *            Station callback.
     */
    public StationTableComp(Composite parent, TableData data,
            CommonConfig.AppName appName, IStationTableAction callback) {
        super(parent, data, appName);

        tableConfig = CommonTableConfig.getInstance();
        tableCallback = callback;

        init();
    }

    /**
     * Sets the Zone/County ID.
     * 
     * @param id
     *            ID.
     */
    public void setZoneCountyId(String id) {
        this.id = id;
    }

    /**
     * Mouse down action.
     * 
     * @param event
     *            Mouse event.
     */
    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TableComp#tableMouseDownAction(org.eclipse.swt.events.MouseEvent)
     */
    @Override
    protected void tableMouseDownAction(MouseEvent event) {
        tableIndex = table.getSelectionIndex();
        mousePt.x = event.x;
        mousePt.y = event.y;
        TableItem item = table.getItem(mousePt);
        if (item == null)
            return;
        /*
         * Check for right mouse clicks.
         */
        if (event.button == 3) {
            for (int i = 0; i < table.getColumnCount(); i++) {
                Rectangle rect = item.getBounds(i);
                if (rect.contains(mousePt)) {
                    int index = table.indexOf(item);
                    // System.out.println("Item " + index + "-" + i);
                    if (i == 0) {
                        tableCallback.launchObHistoryTable(index);
                        return;
                    } else {
                        tableCallback.launchTrendPlot(index, i);
                        return;
                    }
                }
            }
            table.deselectAll();
            table.redraw();
        }
        /*
         * Check for left mouse clicks.
         */
        if (event.button == 1) {
            Rectangle rect = item.getBounds(0);
            if (rect.contains(mousePt)) {
                tableCallback.zoomToStation(table.indexOf(item));
            }
        }
    }

    /**
     * Table mouse hover action.
     * 
     * @param event
     *            Mouse event.
     */
    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TableComp#tableMouseHoverAction(org.eclipse.swt.events.MouseEvent)
     */
    @Override
    protected void tableMouseHoverAction(MouseEvent event) {
        Rectangle rect;
        mousePt.x = event.x;
        mousePt.y = event.y;

        TableItem item = table.getItem(mousePt);

        if (item == null) {
            table.setToolTipText(null);
            return;
        }

        int index = table.indexOf(item);

        for (int i = 0; i < table.getColumnCount(); i++) {
            rect = item.getBounds(i);
            if (rect.contains(mousePt)) {
                table.setToolTipText(tableData.getTableRows().get(index)
                        .getTableCellDataArray()[i].getHoverText());
            }
        }
    }

    /**
     * Sets the table data and ID.
     * 
     * @param tableData
     *            Data to be displayed in the table.
     * @param id
     *            Zone/County ID.
     */
    public void setTableDataAndId(TableData tableData, String id) {
        setZoneCountyId(id);
        setTableData(tableData);
    }

    /**
     * Adds control above the table.
     * 
     * @param parentComp
     *            Parent composite.
     */
    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TableComp#addTopTableControls(org.eclipse.swt.widgets.Composite)
     */
    @Override
    protected void addTopTableControls(Composite parentComp) {
        controlComp = new Composite(parentComp, SWT.NONE);
        controlComp.setLayout(new GridLayout(2, false));
        controlComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData();
        gd.horizontalIndent = 10;

        idLbl = new Label(controlComp, SWT.NONE);
        idLbl.setText("Zone/County: " + id);

        gd = new GridData();
        gd.horizontalIndent = 10;
        Button returnToZoneBtn = new Button(controlComp, SWT.NONE);
        returnToZoneBtn.setText("Return to Zone/County Table");
        returnToZoneBtn.setLayoutData(gd);
        returnToZoneBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                restoreMap();
                table.removeAll();
                tableCallback.stationTableAction();
            }
        });
    }

    /**
     * Gets the column index.
     * 
     * @param appName
     *            Application name.
     * @param sortCol
     *            Sort column.
     * @return Column index.
     */
    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TableComp#getColumnIndex(com.raytheon.uf.common.monitor.data.CommonConfig.AppName, java.lang.String)
     */
    @Override
    public int getColumnIndex(AppName appName, String sortCol) {
        return tableConfig.getTableColumnIndex(appName, sortCol);
    }

    /**
     * Gets an array of column keys.
     * 
     * @param appName
     *            Application name.
     * @return String array of column keys.
     */
    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TableComp#getColumnKeys(com.raytheon.uf.common.monitor.data.CommonConfig.AppName)
     */
    @Override
    protected String[] getColumnKeys(AppName appName) {
        return tableConfig.getTableColumnKeys(appName);
    }

    /**
     * Gets the default column width.
     * 
     * @param appName
     *            Application name.
     * @return The default column width.
     */
    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TableComp#getDefaultColWidth(com.raytheon.uf.common.monitor.data.CommonConfig.AppName)
     */
    @Override
    protected int getDefaultColWidth(AppName appName) {
        int colWidth = tableConfig.getTableDefaultColWidth(appName);
        return colWidth;
    }

    /**
     * Gets the column attribute data.
     * 
     * @param colName
     *            Column name.
     * @return Column attribute data.
     */
    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TableComp#getColumnAttribteData(java.lang.String)
     */
    @Override
    protected ColumnAttribData getColumnAttribteData(String colName) {
        return tableConfig.getTableColumnAttr(colName);
    }

    /**
     * Sets the ID label.
     * 
     * @param name
     */
    public void setIdLabel(String name) {
        idLbl.setText("Zone/County: " + this.id + " - " + name);
        controlComp.layout();
    }

    /**
     * Mouse down action in the station table -- button-3 [Dec 17, 2009 zhao]
     */
    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TableComp#tableColRightMouseAction(org.eclipse.swt.events.MouseEvent)
     */
    @Override
    protected void tableColRightMouseAction(MouseEvent event) {
        /*
         * Check for right and Left mouse clicks.
         */
        if (event.button != 3) {
            return;
        }

        mousePt.x = event.x;
        mousePt.y = event.y;
        TableItem item = table.getItem(mousePt);

        if (item == null) {
            return;
        }

        for (int i = 0; i < table.getColumnCount(); i++) {
            Rectangle rect = item.getBounds(i);
            if (rect.contains(mousePt)) {
                int index = table.indexOf(item);
                if (i == 0) {
                    tableCallback.launchObHistoryTable(index);
                    return;
                } else {
                    tableCallback.launchTrendPlot(index, i);
                    return;
                }
            }
        }
    }

    /**
     * Returns to original map view for Zone/County Table
     */
    protected void restoreMap() {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            for (IDisplayPane pane : container.getDisplayPanes()) {
                pane.getRenderableDisplay().getExtent().reset();
                pane.refresh();
            }
            container.refresh();
        }
    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TableComp#packColumns()
     */
    @Override
    protected void packColumns() {
        for (int i = 0; i < table.getColumnCount(); i++) {
            table.getColumn(i).pack();
            table.getColumn(i).setWidth(defaultColWidth);
        }
    }
}
