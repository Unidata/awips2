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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.mpe.util.DailyQcUtils.Bad_Daily_Values;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;

/**
 * Object for handling bad values.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2009            snaples     Initial creation
 * Jun 17, 2015  17388    ptilles     added check on mpe_dqc_6hr_24hr_flag and
 *                                      changed definition of max_stations variable
 * May 21, 2018   7131    mduff       Updated to accommodate changes to other classes and cleanup.
 * Jul 12, 2018  7357     smanoj      Fixed bad file permission issue.
 * 
 * </pre>
 * 
 * @author snaples
 */

public class BadValues {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(BadValues.class);

    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    private int max_stations = DailyQcUtils.precip_stations.size();

    private int mpe_dqc_6hr_24hr_flag = 1;

    public BadValues() {
        // token name: mpe_dqc_6hr_24hr_set_bad
        // token value = OFF
        // mpe_dqc_6hr_24hr_flag = 0
        // if user sets 6hr value to Bad, then 24hr value is unaffected

        // token value = ON
        // mpe_dqc_6hr_24hr_flag = 1
        // if user sets 6hr value to Bad, then 24hr value is set to Bad

        String mpe_dqc_6hr_24hr_string = AppsDefaults.getInstance()
                .getToken("mpe_dqc_6hr_24hr_set_bad", "ON");

        if (("OFF").equalsIgnoreCase(mpe_dqc_6hr_24hr_string)) {
            mpe_dqc_6hr_24hr_flag = 0;
        }
    }

    public void readBadValues(String precd, int m) {

        Bad_Daily_Values bad_values[] = dqc.bad_values;
        Scanner s = null;

        File badPrecFile = new File(precd);
        if (badPrecFile.exists()) {
            try (BufferedReader in = new BufferedReader(
                    new FileReader(precd))) {
                for (int i = 0; i < 6000; i++) {

                    if (bad_values[i].used == 1) {
                        continue;
                    }

                    String vals = in.readLine();

                    if (vals == null) {
                        break;
                    }

                    s = new Scanner(vals);

                    bad_values[i].used = 1;
                    bad_values[i].hb5 = s.next();
                    bad_values[i].parm = s.next();
                    bad_values[i].day = m;
                    bad_values[i].quart = (int) s.nextDouble();
                    bad_values[i].fvalue = s.nextFloat();
                }
                in.close();
            } catch (IOException e) {
                statusHandler.handle(Priority.ERROR, e.getMessage(), e);
                return;
            }
        }
    }

    public int get_bad_values(int iday, int iquart) {
        Bad_Daily_Values bad_values[] = dqc.bad_values;

        for (int i = 0; i < 6000; i++) {
            if (bad_values[i].used == 0) {
                continue;
            }

            if (bad_values[i].day != iday || bad_values[i].quart != iquart) {
                continue;
            }

            for (int j = 0; j < max_stations; j++) {

                if (bad_values[i].hb5.equals(dqc.precip_stations.get(j).hb5)
                        && bad_values[i].parm
                                .charAt(4) == dqc.precip_stations.get(j).parm
                                        .charAt(4)) {

                    if (dqc.pdata[iday].stn[j].frain[iquart].qual == 5
                            && bad_values[i].fvalue != dqc.pdata[iday].stn[j].rrain[iquart].data
                            && dqc.pdata[iday].stn[j].rrain[iquart].data >= 0) {

                        /* eliminate all bad values for current month */
                        for (int h = 0; h < 6000; h++) {

                            if (bad_values[i].used == 0) {
                                continue;
                            }

                            if (bad_values[i].day != iday
                                    || bad_values[i].quart != iquart) {
                                continue;
                            }

                            bad_values[h].used = 0;
                        }

                        /* swap in level 1 data */

                        return (1);

                    } else {
                        dqc.pdata[iday].stn[j].frain[iquart].qual = 1;
                        dqc.pdata[iday].stn[j].frain[iquart].data = bad_values[i].fvalue;
                    }
                }
            }
        }
        return 0;
    }

    public void write_bad_values(String fname, int iday) {

        String ibuf;
        File bfile = new File(fname);
        Bad_Daily_Values bad_values[] = dqc.bad_values;
        
        try (BufferedWriter out = new BufferedWriter(new FileWriter(bfile))) {
            for (int i = 0; i < 6000; i++) {

                if (bad_values[i].used == 0) {
                    continue;
                }

                if (iday == bad_values[i].day) {
                    if (bad_values[i].fvalue < 0) {
                        continue;
                    }

                    ibuf = String.format("%s %s %d %f", bad_values[i].hb5,
                            bad_values[i].parm, bad_values[i].quart,
                            bad_values[i].fvalue);
                    out.write(ibuf);
                    out.newLine();
                }
           }
        } catch (IOException e) {
            statusHandler.handle(Priority.ERROR,
                    "Problem writing bad file " + fname + e.getMessage(), e);
        } finally {

            try {
                // Open up the write permission of the bad files for the GROUP.
                File badfile = new File(fname);

                if (!(Files.getPosixFilePermissions(badfile.toPath()))
                        .equals(FilePermissionHelper.POSIX_FILE_SET)) {
                    FilePermissionHelper.applyFilePermissions(badfile.toPath(),
                            FilePermissionHelper.POSIX_FILE_SET);
                }
            } catch (IOException e) {
                statusHandler.handle(Priority.ERROR,
                        "Failed to update permissions on bad file " + fname
                                + e.getMessage(),
                        e);
            }
        }

        return;
    }

    public void update_bad_values(int iday) {
        Bad_Daily_Values bad_values[] = dqc.bad_values;
        ArrayList<Station> station = dqc.precip_stations;

        for (int i = 0; i < 6000; i++) {

            if (bad_values[i].used == 0) {
                continue;
            }

            if (bad_values[i].day != iday) {
                continue;
            }

            bad_values[i].used = 0;

        }

        for (int j = 0; j < max_stations; j++) {
            for (int k = 0; k < 5; k++) {

                if (dqc.pdata[iday].stn[j].frain[k].qual != 1) {
                    continue;
                }

                for (int h = 0; h < 6000; h++) {

                    if (bad_values[h].used != 0) {
                        continue;
                    }

                    bad_values[h].used = 1;

                    /*
                     * since allow TD and partial gage set as bad, then missing
                     * level 1 data will be displayed as -1 on DQC, now need to
                     * retain the original level 2 value on DQC when set as bad
                     */

                    bad_values[h].fvalue = dqc.pdata[iday].stn[j].frain[k].data;

                    bad_values[h].hb5 = station.get(j).hb5;
                    bad_values[h].parm = station.get(j).parm;
                    bad_values[h].day = iday;
                    bad_values[h].quart = k;

                    break;
                }
            }
        }

        return;

    }

    public void restore_bad_values(int iday, List<Station> precip_stations,
            int max_stations) {
        for (int k = 0; k < 5; k++) {

            for (int i = 0; i < 6000; i++) {

                if (dqc.bad_values[i].used == 0) {
                    continue;
                }

                if (dqc.bad_values[i].day != iday
                        || dqc.bad_values[i].quart != k) {
                    continue;
                }

                for (int j = 0; j < max_stations; j++) {

                    if ((dqc.bad_values[i].hb5
                            .equalsIgnoreCase(precip_stations.get(j).hb5))
                            && dqc.bad_values[i].parm
                                    .charAt(4) == precip_stations.get(j).parm
                                            .charAt(4)) {

                        dqc.pdata[iday].stn[j].frain[k].data = dqc.bad_values[i].fvalue;
                        dqc.pdata[iday].stn[j].frain[k].qual = 1;

                        // 6hr qual code bad - check how to set 24hr qual code
                        // added for DR 17388

                        if (mpe_dqc_6hr_24hr_flag == 1) {

                            if (k >= 0 && k <= 3
                                    && dqc.pdata[iday].stn[j].rrain[4].data >= 0) {

                                dqc.pdata[iday].stn[j].frain[4].qual = 1;

                            }
                        }

                        break;
                    }
                }
            }
        }

        return;
    }

    public int is_bad(int iday, int iquart, String hb5, String parm) {
        for (int i = 0; i < 6000; i++) {

            if (dqc.bad_values[i].used == 0) {
                continue;
            }

            if (dqc.bad_values[i].day != iday
                    || dqc.bad_values[i].quart != iquart) {
                continue;
            }

            for (int j = 0; j < max_stations; j++) {

                if (dqc.bad_values[i].hb5.equals(hb5)
                        && dqc.bad_values[i].parm.charAt(4) == parm.charAt(4)) {
                    return 1;
                }
            }
        }

        return 0;
    }

    public void post_bad_values(int iday) {
        for (int k = 0; k < 5; k++) {
            for (int i = 0; i < 6000; i++) {
                if (dqc.bad_values[i].used == 0) {
                    continue;
                }

                if (dqc.bad_values[i].day != iday
                        || dqc.bad_values[i].quart != k) {
                    continue;
                }
                for (int j = 0; j < max_stations; j++) {
                    if ((dqc.bad_values[i].hb5
                            .equals(dqc.precip_stations.get(j).hb5))
                            && dqc.bad_values[i].parm
                                    .charAt(4) == dqc.precip_stations
                                            .get(j).parm.charAt(4)) {
                        if (dqc.pdata[iday].stn[j].frain[k].data == dqc.bad_values[i].fvalue) {
                            dqc.pdata[iday].stn[j].frain[k].data = dqc.bad_values[i].fvalue;
                            dqc.pdata[iday].stn[j].frain[k].qual = 1;
                        }

                        break;
                    }
                }
            }
        }
        return;
    }
}
