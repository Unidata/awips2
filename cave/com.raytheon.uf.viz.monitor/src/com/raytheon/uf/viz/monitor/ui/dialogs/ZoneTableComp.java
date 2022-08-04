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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig;
import com.raytheon.uf.viz.monitor.data.ColumnAttribData;
import com.raytheon.uf.viz.monitor.data.TableData;

/**
 * Zone table composite that contains the table for the zones.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------
 * Apr 07, 2009           lvenable  Initial creation
 * Feb 23, 2016  5393     randerso  Fix column width issues, code cleanup
 * Aug 16, 2018  7410     randerso  Refactored mouse handling
 *
 * </pre>
 *
 * @author lvenable
 */
public class ZoneTableComp extends TableComp {
    /**
     * Callback for the zone table.
     */
    private IZoneTableAction zoneTableCallback;

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
        if (event.button == 1) {
            Point rc = getRowAndColumnIndex(event);
            if (rc != null) {
                if (rc.x == 0) {
                    this.zoneTableCallback.zoneTableAction(rc.y);
                }
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
     * Get the column attribute data.
     *
     * @param colName
     *            Column name.
     * @return The column attribute data.
     */
    @Override
    protected ColumnAttribData getColumnAttributeData(String colName) {
        return tableConfig.getTableColumnAttr(colName);
    }
}
