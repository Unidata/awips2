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
package com.raytheon.viz.hydrocommon.datasources;

import java.util.List;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Singleton class for managing database query calls.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2008  1555       grichard     Initial creation.
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class DataSourcesDcpDataMgr {

    /**
     * The data sources manager.
     */
    private static DataSourcesDcpDataMgr manager = null;

    /**
     * The data sources data.
     */
    private DataSourcesDcpData sourcesData;

    /**
     * The current Location Identifier.
     */
    private String currentLid;

    /**
     * Private constructor.
     */
    private DataSourcesDcpDataMgr() {
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized DataSourcesDcpDataMgr getInstance() {
        if (manager == null) {
            manager = new DataSourcesDcpDataMgr();
        }

        return manager;
    }

    /**
     * Get the sources data.
     */
    public DataSourcesDcpData getDataSourcesDcpData() {

        if (sourcesData == null) {
            String stationQuerySql = "select lid,criteria,goes,rptfreq,rptime,notify from dcp where lid = '"+currentLid +"' limit 50";

            List<Object[]> data;
            try {
                long start = System.currentTimeMillis();
                data = DirectDbQuery.executeQuery(stationQuerySql, HydroConstants.IHFS, QueryLanguage.SQL);
               // data = (ArrayList<Object[]>) stationQuery.performQuery();
                for (Object[] rowData : data) {
                    sourcesData = new DataSourcesDcpData((String) rowData[0],
                            (String) rowData[1], (String) rowData[2],
                            (String) rowData[3], (String) rowData[4],
                            (String) rowData[5]);
                }
                long end = System.currentTimeMillis();
            } catch (VizException e) {
                e.printStackTrace();
            }
        }

        return sourcesData;
    }

    /**
     * Setter for current LID
     * 
     * @param currentLid
     */
    public void setCurrentLid(String currentLid) {
        this.currentLid = currentLid;
    }
}
