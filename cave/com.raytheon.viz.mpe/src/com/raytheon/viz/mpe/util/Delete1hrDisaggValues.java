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
package com.raytheon.viz.mpe.util;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Date;

import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.HourlyppId;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.whfslib.GagePPWrite;
import com.raytheon.viz.mpe.util.Disagg6Hr.Values_1hr;
import com.raytheon.viz.mpe.util.Disagg6Hr.Values_6hr;

/**
 * -------------------------------------------------------------------------
 * Function to delete 1hr values resulting from a previous disagg run from the
 * HourlyPP table
 * 
 * For the begin and end days, the HourlyPP records are not deleted. Instead,
 * the records are updated with values set to missing and hourly_qc code set to
 * "-". For the other days, the records are deleted.
 * 
 * PK fields in the HourlyPP table are lid, ts, obsdate Each record in the
 * HourlyPP table contains 24 1hr values Records in the HourlyPP table have ts
 * set to 'PZ' Input to function: structure containing the disagg station
 * identifiers (disagg_station_6hr[j*num_disagg_stations+i].hb5) structure
 * containing the 1hr values resulting from disagg (disaggValues.HourlyValues)
 * array of obsdates (obsdates array) number of days of data to disagg =
 * num_days number of 6hr stations to disagg = num_records Output from function:
 * Write to log file: lid, hourly values for all stations
 * 
 * Calling routine: Disagg6hr
 * -------------------------------------------------------------------------
 * Array definitions:
 * 
 * disaggValues[ind1].HourlyValues[ind2] ind1 = j*num_disagg_stations+i ind2 =
 * 6*k+l where j = index on days to QC = 0 to num_days i = index on disagg
 * stations = 0 to num_disagg_stations k = index on 6hr periods = 0,1,2,3 l =
 * index on 1hr slots = 0,1,2,3,4,5
 * 
 * disagg_station_6hr[ind1].hb5 where ind1 defined as above
 * -------------------------------------------------------------------------
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2011            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class Delete1hrDisaggValues {

    public void delete1hrDisaggValues() {
        Values_1hr[] disaggValues = Disagg6Hr.disaggValues;
        BufferedWriter disagg_log_fd = Disagg6Hr.disagg_log_fd;
        String[] obsdate = Disagg6Hr.obsdate;
        Date[] obsdate_date_t = Disagg6Hr.obsdate_date_t;
        Values_6hr[] values6hr = Disagg6Hr.values6hr;
        int num_days_to_qc = DailyQcUtils.qcDays;
        int num_disagg_stations = Disagg6Hr.num_disagg_stations;

        String ts = "";
        String where = "";
        int i, j, jj;
        int index = -1;
        int ret = 0;

        Hourlypp hourlyPP;
        HourlyppId hourlyPPId;
        BufferedWriter out = disagg_log_fd;

        ts = "PZ";

        try {
            // out = new BufferedWriter(new FileWriter(disagg_log_fd));

            /*-----------------------------------------*/
            /* populate the HourlyPP structure */
            /*-----------------------------------------*/

            hourlyPPId = new HourlyppId();
            hourlyPPId.setTs(ts);
            hourlyPP = new Hourlypp();
            hourlyPP.setMinuteOffset("------------------------");
            hourlyPP.setHourlyQc("DDDDDDDDDDDDDDDDDDDDDDDD");

            hourlyPP.setSixhr06((short) -9999);
            hourlyPP.setSixhr12((short) -9999);
            hourlyPP.setSixhr18((short) -9999);
            hourlyPP.setSixhr24((short) -9999);
            hourlyPP.setSixhrqc("ZZZZ");
            hourlyPP.setSixhroffset("----");

            out.write("\nIn delete1hrDisaggValues\n");

            for (j = 0; j < num_days_to_qc + 1; j++) {

                out.write("\n");
                out.write(String.format(" \t Day %d\n", j));
                hourlyPPId.setObsdate(obsdate_date_t[j]);

                jj = j;
                if (j == num_days_to_qc) {
                    jj = jj - 1;
                }

                for (i = 0; i < num_disagg_stations; i++) {
                    index = jj * num_disagg_stations + i;
                    // do not allow jj to be > num_days_to_qc

                    disaggValues[index].ID = values6hr[index].ID;
                    hourlyPPId.setLid(disaggValues[index].ID);
                    hourlyPP.setId(hourlyPPId);

                    out.write(String.format("\n%s\n", disaggValues[index].ID));

                    if (j == 0) {
                        /*----------------------------------------------*/
                        /* first day */
                        /* first half of day is updated with all -9999 */
                        /* second half of day is updated with all -9999 */
                        /*----------------------------------------------*/
                        hourlyPP.setHour1((short) -9999);
                        hourlyPP.setHour2((short) -9999);
                        hourlyPP.setHour3((short) -9999);
                        hourlyPP.setHour4((short) -9999);
                        hourlyPP.setHour5((short) -9999);
                        hourlyPP.setHour6((short) -9999);
                        hourlyPP.setHour7((short) -9999);
                        hourlyPP.setHour8((short) -9999);
                        hourlyPP.setHour9((short) -9999);
                        hourlyPP.setHour10((short) -9999);
                        hourlyPP.setHour11((short) -9999);
                        hourlyPP.setHour12((short) -9999);
                        hourlyPP.setHour13((short) -9999);
                        hourlyPP.setHour14((short) -9999);
                        hourlyPP.setHour15((short) -9999);
                        hourlyPP.setHour16((short) -9999);
                        hourlyPP.setHour17((short) -9999);
                        hourlyPP.setHour18((short) -9999);
                        hourlyPP.setHour19((short) -9999);
                        hourlyPP.setHour20((short) -9999);
                        hourlyPP.setHour21((short) -9999);
                        hourlyPP.setHour22((short) -9999);
                        hourlyPP.setHour23((short) -9999);
                        hourlyPP.setHour24((short) -9999);

                        where = String.format(
                                "WHERE lid='%s' AND ts='PZ' AND obsdate='%s'",
                                hourlyPPId.getLid(), obsdate[j]);
                        out.write(String.format("%s\n", where));
                        out.write(String
                                .format("update record with all -9999\n"));
                        out.write("\n");

                        /* update record */

                        GagePPWrite.update_gage_rec(hourlyPP);

                    } else if (j == jj) {
                        /*----------------------------------------------*/
                        /* middle days */
                        /* delete record */
                        /*----------------------------------------------*/

                        where = String.format(
                                "WHERE lid='%s' AND ts='PZ' AND obsdate='%s'",
                                hourlyPPId.getLid(), obsdate[j]);
                        out.write(String.format("%s\n", where));
                        out.write("\n");

                        ret = deleteHourlyPP(where);

                    } else {
                        /*----------------------------------------------*/
                        /* last day - partial day defined */
                        /* second half of day is updated with all -9999 */
                        /* first half of day is previously defined */
                        /*----------------------------------------------*/

                        hourlyPP.setHour13((short) -9999);
                        hourlyPP.setHour14((short) -9999);
                        hourlyPP.setHour15((short) -9999);
                        hourlyPP.setHour16((short) -9999);
                        hourlyPP.setHour17((short) -9999);
                        hourlyPP.setHour18((short) -9999);
                        hourlyPP.setHour19((short) -9999);
                        hourlyPP.setHour20((short) -9999);
                        hourlyPP.setHour21((short) -9999);
                        hourlyPP.setHour22((short) -9999);
                        hourlyPP.setHour23((short) -9999);
                        hourlyPP.setHour24((short) -9999);

                        where = String.format(
                                "WHERE lid='%s' AND ts='PZ' AND obsdate='%s'",
                                hourlyPPId.getLid(), obsdate[j]);
                        out.write(String.format("%s\n", where));
                        out.write(String
                                .format("update record - last 12 hr of day with all -9999\n"));
                        out.write("\n");

                        /* update record */

                        GagePPWrite.update_gage_rec(hourlyPP);

                    }
                }
            }

        } catch (IOException e) {
            return;
        }
        // finally {
        // try {
        // if (out != null) {
        // out.close();
        // }
        //
        // } catch (IOException e) {
        // e.printStackTrace();
        // }
        //
        // }

    }

    public int deleteHourlyPP(String where) {
        int status = -1;

        String query = "DELETE FROM HourlyPP " + where;

        try {
            status = DirectDbQuery.executeStatement(query, HydroConstants.IHFS,
                    QueryLanguage.SQL);
        } catch (VizException e) {
            status = -1;
            e.printStackTrace();
        }

        return status;
    }
}
