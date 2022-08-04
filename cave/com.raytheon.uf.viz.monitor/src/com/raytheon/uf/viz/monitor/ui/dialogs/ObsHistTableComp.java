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

import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.ObsHistType;
import com.raytheon.uf.viz.monitor.data.ColumnAttribData;
import com.raytheon.uf.viz.monitor.data.TableData;

/**
 * Observation History table composite.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------
 * Apr 06, 2009           lvenable  Initial creation
 * Feb 23, 2016  5393     randerso  Fix column width issues, code cleanup
 * Aug 16, 2018  7410     randerso  Removed unimplemented methods.
 *
 * </pre>
 *
 * @author lvenable
 */
public class ObsHistTableComp extends TableComp {
    /**
     * Common table configuration data.
     */
    private CommonTableConfig tableConfig;

    /**
     * Observation type.
     */
    private ObsHistType obsType;

    /**
     * Constructor.
     *
     * @param parent
     *            Parent composite.
     * @param data
     *            Table data.
     * @param appName
     *            Application name.
     * @param obsType
     *            Observation type.
     */
    public ObsHistTableComp(Composite parent, TableData data,
            CommonConfig.AppName appName, ObsHistType obsType) {
        super(parent, data, appName);

        this.obsType = obsType;

        tableConfig = CommonTableConfig.getInstance();

        init();
    }

    /**
     * Get the observation history column index.
     *
     * @param appName
     *            Application name.
     * @param sortCol
     *            Column to sort on.
     * @return The column index of the column to be sorted.
     */
    @Override
    protected int getColumnIndex(AppName appName, String sortCol) {
        return tableConfig.getObsHistColumnIndex(appName, obsType, sortCol);
    }

    /**
     * Get the array of column keys used to access the column metadata.
     *
     * @param app
     *            Application name.
     * @return String array of column keys.
     */
    @Override
    protected String[] getColumnKeys(AppName app) {
        return tableConfig.getObsHistColumnKeys(app, obsType);
    }

    /**
     * Get the table attribute data for the specified column.
     *
     * @param colName
     *            Column name.
     * @return The attribute data.
     */
    @Override
    protected ColumnAttribData getColumnAttributeData(String colName) {
        return tableConfig.getObsHistColumnAttr(colName);
    }
}
