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
 * Oct 8, 2008  1555       grichard    Initial creation.
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class DataSourcesTlmDataMgr {

    /**
     * The data sources manager.
     */
    private static DataSourcesTlmDataMgr manager = null;

    /**
     * The data sources data.
     */
    private DataSourcesTlmData sourcesData;

    /**
     * The current Location Identifier.
     */
    private String currentLid;

    /**
     * Private constructor.
     */
    private DataSourcesTlmDataMgr() {
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized DataSourcesTlmDataMgr getInstance() {
        if (manager == null) {
            manager = new DataSourcesTlmDataMgr();
        }

        return manager;
    }

    /**
     * Get the sources data.
     */
    public DataSourcesTlmData getDataSourcesTlmData() {

        if (sourcesData == null) {
            String stationQuerySql = "select lid,cost,criteria,phone,sensorid,rptfreq from telem where lid = '" + currentLid+"'";
//            String stationQuerySql = "select lid,cost,criteria,phone,sensorid,rptfreq from telem where lid = '" + currentLid+"' limit 150";
            List<Object[]> data;
            try {
                long start = System.currentTimeMillis();
                data = DirectDbQuery.executeQuery(stationQuerySql, HydroConstants.IHFS, QueryLanguage.SQL);
                for (Object[] rowData : data) {
                    sourcesData = new DataSourcesTlmData((String) rowData[0],
                            (Double) rowData[1], (String) rowData[2],
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
