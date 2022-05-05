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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

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
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------
 * Apr 06, 2009           lvenable  Initial creation
 * Dec 18, 2009  3424     zhao      Implemented tableColRightMouseAction
 * Sep 04, 2014  3220     skorolev  Cleaned code.
 * Feb 23, 2016  5393     randerso  Fix column width issues, code cleanup
 * Aug 16, 2018  7410     randerso  Refactored mouse handling
 *
 * </pre>
 *
 * @author lvenable
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
    @Override
    protected void tableMouseDownAction(MouseEvent event) {
        Point rc = getRowAndColumnIndex(event);
        if (rc != null) {
            if (event.button == 3) {
                /* right button clicked */
                if (rc.x == 0) {
                    tableCallback.launchObHistoryTable(rc.y);
                } else {
                    tableCallback.launchTrendPlot(rc.y, rc.x);
                }

            } else if (event.button == 1) {
                /* left button clicked */
                if (rc.x == 0) {
                    tableCallback.zoomToStation(rc.y);
                }
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
    @Override
    protected void addTopTableControls(Composite parentComp) {
        controlComp = new Composite(parentComp, SWT.NONE);
        controlComp.setLayout(new GridLayout(2, false));
        controlComp.setLayoutData(
                new GridData(SWT.FILL, SWT.DEFAULT, true, false));

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
            @Override
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
    @Override
    protected String[] getColumnKeys(AppName appName) {
        return tableConfig.getTableColumnKeys(appName);
    }

    /**
     * Gets the column attribute data.
     *
     * @param colName
     *            Column name.
     * @return Column attribute data.
     */
    @Override
    protected ColumnAttribData getColumnAttributeData(String colName) {
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
}
