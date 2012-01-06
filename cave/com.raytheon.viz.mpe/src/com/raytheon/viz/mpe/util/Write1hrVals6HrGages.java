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
import com.raytheon.viz.hydrocommon.whfslib.GagePPWrite;
import com.raytheon.viz.mpe.util.Disagg6Hr.Values_1hr;
import com.raytheon.viz.mpe.util.Disagg6Hr.Values_6hr;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class Write1hrVals6HrGages {

    public void write1hrValuesFor6hrGages() {

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

        Hourlypp hourlyPP;
        HourlyppId hourlyPPId;
        BufferedWriter out = disagg_log_fd;

        ts = "PZ";

        try {
            // out = new BufferedWriter(new FileWriter(disagg_log_fd));

            /*-----------------------------------------*/
            /* populate the HourlyPP structure */
            /*-----------------------------------------*/

            for (j = 0; j < num_days_to_qc + 1; j++) {

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

                out.write("\n");
                out.write(String.format(" \t Day %d\n", j));
                hourlyPPId.setObsdate(obsdate_date_t[j]);

                jj = j;
                if (j == num_days_to_qc) {
                    jj = jj - 1;
                }

                for (i = 0; i < num_disagg_stations; i++) {

                    index = jj * num_disagg_stations + i;
                    // do not allow jj to
                    // be > num_days_to_qc

                    /*
                     * station id = DISAGG_MISSING_STATION_SYMBOL signifies
                     * invalid station -- ignore
                     */
                    if (values6hr[index].ID
                            .equals(Disagg6Hr.DISAGG_MISSING_STATION_SYMBOL)) {
                        continue;
                    }

                    disaggValues[index].ID = values6hr[index].ID;
                    hourlyPPId.setLid(disaggValues[index].ID);
                    hourlyPP.setId(hourlyPPId);

                    out.write(String.format("\n%s\n", disaggValues[index].ID));

                    /*----------------------------------------------*/
                    /* Store values in hourlyPP structure */
                    /* hourlyPP.hourxx is a smallint */
                    /* hourlyPP.hourxx has units of inches x 100 */
                    /*----------------------------------------------*/

                    if (j == 0) {

                        /*----------------------------------------------*/
                        /* first day - partial day defined */
                        /* second half of day is missing */
                        /*----------------------------------------------*/

                        hourlyPP.setHour1((short) disaggValues[index].HourlyValues[12]);
                        hourlyPP.setHour2((short) disaggValues[index].HourlyValues[13]);
                        hourlyPP.setHour3((short) disaggValues[index].HourlyValues[14]);
                        hourlyPP.setHour4((short) disaggValues[index].HourlyValues[15]);
                        hourlyPP.setHour5((short) disaggValues[index].HourlyValues[16]);
                        hourlyPP.setHour6((short) disaggValues[index].HourlyValues[17]);
                        hourlyPP.setHour7((short) disaggValues[index].HourlyValues[18]);
                        hourlyPP.setHour8((short) disaggValues[index].HourlyValues[19]);
                        hourlyPP.setHour9((short) disaggValues[index].HourlyValues[20]);
                        hourlyPP.setHour10((short) disaggValues[index].HourlyValues[21]);
                        hourlyPP.setHour11((short) disaggValues[index].HourlyValues[22]);
                        hourlyPP.setHour12((short) disaggValues[index].HourlyValues[23]);
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
                    } else if (j == jj) {

                        /*----------------------------------------------*/
                        /* middle days - full day defined */
                        /* values are from different hydrologic days */
                        /*----------------------------------------------*/

                        index = jj * num_disagg_stations + i;

                        hourlyPP.setHour1((short) disaggValues[index].HourlyValues[12]);
                        hourlyPP.setHour2((short) disaggValues[index].HourlyValues[13]);
                        hourlyPP.setHour3((short) disaggValues[index].HourlyValues[14]);
                        hourlyPP.setHour4((short) disaggValues[index].HourlyValues[15]);
                        hourlyPP.setHour5((short) disaggValues[index].HourlyValues[16]);
                        hourlyPP.setHour6((short) disaggValues[index].HourlyValues[17]);
                        hourlyPP.setHour7((short) disaggValues[index].HourlyValues[18]);
                        hourlyPP.setHour8((short) disaggValues[index].HourlyValues[19]);
                        hourlyPP.setHour9((short) disaggValues[index].HourlyValues[20]);
                        hourlyPP.setHour10((short) disaggValues[index].HourlyValues[21]);
                        hourlyPP.setHour11((short) disaggValues[index].HourlyValues[22]);
                        hourlyPP.setHour12((short) disaggValues[index].HourlyValues[23]);

                        index = (jj - 1) * num_disagg_stations + i;
                        // values
                        // from
                        // previous
                        // hydro day

                        hourlyPP.setHour13((short) disaggValues[index].HourlyValues[0]);
                        hourlyPP.setHour14((short) disaggValues[index].HourlyValues[1]);
                        hourlyPP.setHour15((short) disaggValues[index].HourlyValues[2]);
                        hourlyPP.setHour16((short) disaggValues[index].HourlyValues[3]);
                        hourlyPP.setHour17((short) disaggValues[index].HourlyValues[4]);
                        hourlyPP.setHour18((short) disaggValues[index].HourlyValues[5]);
                        hourlyPP.setHour19((short) disaggValues[index].HourlyValues[6]);
                        hourlyPP.setHour20((short) disaggValues[index].HourlyValues[7]);
                        hourlyPP.setHour21((short) disaggValues[index].HourlyValues[8]);
                        hourlyPP.setHour22((short) disaggValues[index].HourlyValues[9]);
                        hourlyPP.setHour23((short) disaggValues[index].HourlyValues[10]);
                        hourlyPP.setHour24((short) disaggValues[index].HourlyValues[11]);

                    } else {

                        /*----------------------------------------------*/
                        /* last day - partial day defined */
                        /* second half of day is updated */
                        /* first half of day is previously defined */
                        /*----------------------------------------------*/

                        hourlyPP.setHour13((short) disaggValues[index].HourlyValues[0]);
                        hourlyPP.setHour14((short) disaggValues[index].HourlyValues[1]);
                        hourlyPP.setHour15((short) disaggValues[index].HourlyValues[2]);
                        hourlyPP.setHour16((short) disaggValues[index].HourlyValues[3]);
                        hourlyPP.setHour17((short) disaggValues[index].HourlyValues[4]);
                        hourlyPP.setHour18((short) disaggValues[index].HourlyValues[5]);
                        hourlyPP.setHour19((short) disaggValues[index].HourlyValues[6]);
                        hourlyPP.setHour20((short) disaggValues[index].HourlyValues[7]);
                        hourlyPP.setHour21((short) disaggValues[index].HourlyValues[8]);
                        hourlyPP.setHour22((short) disaggValues[index].HourlyValues[9]);
                        hourlyPP.setHour23((short) disaggValues[index].HourlyValues[10]);
                        hourlyPP.setHour24((short) disaggValues[index].HourlyValues[11]);
                    }

                    where = String.format(
                            "WHERE lid='%s' AND ts='PZ' AND obsdate='%s'",
                            hourlyPPId.getLid(), obsdate[j]);
                    out.write(String.format("%s\n", where));
                    out.write("\n");

                    if (j == num_days_to_qc) {
                        out.write(String.format(
                                "%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d",
                                hourlyPP.getHour13(), hourlyPP.getHour14(),
                                hourlyPP.getHour15(), hourlyPP.getHour16(),
                                hourlyPP.getHour17(), hourlyPP.getHour18(),
                                hourlyPP.getHour19(), hourlyPP.getHour20(),
                                hourlyPP.getHour21(), hourlyPP.getHour22(),
                                hourlyPP.getHour23(), hourlyPP.getHour24()));
                    } else {
                        out.write(String
                                .format("%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d,%d",
                                        hourlyPP.getHour1(),
                                        hourlyPP.getHour2(),
                                        hourlyPP.getHour3(),
                                        hourlyPP.getHour4(),
                                        hourlyPP.getHour5(),
                                        hourlyPP.getHour6(),
                                        hourlyPP.getHour7(),
                                        hourlyPP.getHour8(),
                                        hourlyPP.getHour9(),
                                        hourlyPP.getHour10(),
                                        hourlyPP.getHour11(),
                                        hourlyPP.getHour12(),
                                        hourlyPP.getHour13(),
                                        hourlyPP.getHour14(),
                                        hourlyPP.getHour15(),
                                        hourlyPP.getHour16(),
                                        hourlyPP.getHour17(),
                                        hourlyPP.getHour18(),
                                        hourlyPP.getHour19(),
                                        hourlyPP.getHour20(),
                                        hourlyPP.getHour21(),
                                        hourlyPP.getHour22(),
                                        hourlyPP.getHour23(),
                                        hourlyPP.getHour24()));
                    }

                    out.write("\n");

                    GagePPWrite.update_gage_rec(hourlyPP);

                } /* end for (i=0;i<num_disagg_stations;i++) */

            } /* end for (j=0;j<num_days_to_qc+1;j++) */
            // out.close();

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
        // }
    }
}
