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
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TableItem;

import com.raytheon.uf.common.monitor.data.ColumnAttribData;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.CommonTableConfig;
import com.raytheon.uf.viz.monitor.data.TableData;

/**
 * Zone table composite that contains the table for the zones.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 7, 2009            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ZoneTableComp extends TableComp {
    /**
     * Callback for the zone table.
     */
    private IZoneTableAction zoneTableCallback;

    /**
     * Mouse point.
     */
    private Point mousePt = new Point(0, 0);

    /**
     * Common table config data.
     */
    private CommonTableConfig tableConfig;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent composite.
     * @param data
     *            Table data.
     * @param appName
     *            Application name.
     * @param callback
     *            Zone table action callback.
     */
    public ZoneTableComp(Composite parent, TableData data,
            CommonConfig.AppName appName, IZoneTableAction callback) {
        super(parent, data, appName);

        this.zoneTableCallback = callback;

        tableConfig = CommonTableConfig.getInstance();

        init();
    }

    /**
     * Handle the mouse down action.
     * 
     * @param event
     *            Mouse event.
     */
    @Override
    protected void tableMouseDownAction(MouseEvent event) {
        tableIndex = table.getSelectionIndex();
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
                System.out.println("Item " + index + "-" + i);

                if (i == 0) {
                    this.zoneTableCallback.zoneTableAction(index);
                }
            }
        }
        table.deselectAll();
        table.redraw();
    }

    /**
     * Handle the table mouse hover action.
     * 
     * @param event
     *            Maouse event.
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
     * Add controls above the table on the display.
     * 
     * @param parentComp
     *            Parent composite.
     */
    @Override
    protected void addTopTableControls(Composite parentComp) {
        GridData gd = new GridData();
        gd.horizontalIndent = 10;
        Label zoneLbl = new Label(parentComp, SWT.NONE);
        zoneLbl.setText("Zone/County List");
        zoneLbl.setLayoutData(gd);
    }

    /**
     * Get the column index given the application name and sort column.
     * 
     * @param appName
     *            Application name.
     * @param columnName
     *            Column name.
     */
    @Override
    public int getColumnIndex(AppName appName, String columnName) {
        return tableConfig.getTableColumnIndex(appName, columnName);
    }

    /**
     * Get the array of column keys.
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
     * Get the default column width.
     * 
     * @param appName
     *            Application name.
     * @return The default column width.
     */
    @Override
    protected int getDefaultColWidth(AppName appName) {
        int colWidth = tableConfig.getTableDefaultColWidth(appName);
        return colWidth;
    }

    /**
     * Get the column attribute data.
     * 
     * @param colName
     *            Column name.
     * @return The column attribute data.
     */
    @Override
    protected ColumnAttribData getColumnAttribteData(String colName) {
        return tableConfig.getTableColumnAttr(colName);
    }

    @Override
    protected void tableColRightMouseAction(MouseEvent event) {
        // TODO Auto-generated method stub

    }

    @Override
    protected void packColumns() {
        for (int i = 0; i < table.getColumnCount(); i++) {
            table.getColumn(i).pack();
            table.getColumn(i).setWidth(defaultColWidth);
        }
    }
}
