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
package com.raytheon.viz.hydrocommon.datamanager;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.viz.hydrocommon.data.DataLimitData;

/**
 * Class for managing database query calls. QuestionableDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2008 1636       askripsky   Initial Creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class DataLimitDataManager extends HydroDataManager {
    protected static DataLimitDataManager manager = null;

    /**
     * Private constructor.
     */
    private DataLimitDataManager() {
        super();
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized DataLimitDataManager getInstance() {
        if (manager == null) {
            manager = new DataLimitDataManager();
        }

        return (DataLimitDataManager) manager;
    }

    /**
     * Checks the LocDataLimits table for LID specific limits. If no results are
     * returned, the DataLimits table is checked.
     * 
     * @param lid
     * @param pe
     * @param dur
     * @param obstime
     * @return
     */
    public ArrayList<DataLimitData> getLimits(String lid, String pe,
            String dur, String obstime) {
        ArrayList<DataLimitData> rval = new ArrayList<DataLimitData>();
        StringBuffer limitQuery = new StringBuffer();

        String queryTable = "locdatalimits";

        // int reasonRangeMax = HydroConstants.MISSING_VALUE;
        // int reasonRangeMin = HydroConstants.MISSING_VALUE;

        // Runs twice,
        // first queries locdatalimits
        // then queries datalimits if no records from the first
        for (int i = 0; i < 2; i++) {
            // Set columns
            limitQuery.setLength(0);
            limitQuery.append("SELECT ");
            limitQuery.append("pe,");
            limitQuery.append("dur,");
            limitQuery.append("monthdaystart,");
            limitQuery.append("monthdayend,");
            limitQuery.append("gross_range_min,");
            limitQuery.append("gross_range_max,");
            limitQuery.append("reason_range_min,");
            limitQuery.append("reason_range_max,");
            limitQuery.append("roc_max,");
            limitQuery.append("alert_upper_limit,");
            limitQuery.append("alert_roc_limit,");
            limitQuery.append("alarm_upper_limit,");
            limitQuery.append("alarm_roc_limit,");
            limitQuery.append("alert_lower_limit,");
            limitQuery.append("alarm_lower_limit,");
            limitQuery.append("alert_diff_limit,");
            limitQuery.append("alarm_diff_limit");

            // Set table
            limitQuery.append(" FROM ");
            limitQuery.append(queryTable);

            // Set Constraints
            limitQuery.append(" WHERE pe = '");
            limitQuery.append(pe);
            limitQuery.append("' AND dur = '");
            limitQuery.append(dur);
            limitQuery.append("'");

            // Set time constraints
            // (monthdaystart < '02-01' and monthdayend > '12-32')
            limitQuery.append(" AND (monthdaystart < '");
            limitQuery.append(obstime);
            limitQuery.append("' AND monthdayend > '");
            limitQuery.append(obstime);
            limitQuery.append("')");

            // Lid constraint only applies to locdatalimits table
            if (queryTable.compareTo("locdatalimits") == 0) {
                limitQuery.append(" AND lid = '");
                limitQuery.append(lid);
                limitQuery.append("'");
            }

            try {
                List<Object[]> data = runQuery(limitQuery.toString());

                if (data.size() > 0) {
                    for (Object[] currData : data) {
                        rval.add(new DataLimitData(currData));
                    }

                    // Found data in the locdatalimit table, so don't have to
                    // check the datalimit table
                    break;
                } else {
                    queryTable = "datalimits";
                    limitQuery.setLength(0);
                    continue;
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        return rval;
    }
}
