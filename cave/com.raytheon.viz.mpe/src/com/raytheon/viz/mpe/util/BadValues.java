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
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

import com.raytheon.viz.mpe.util.DailyQcUtils.Bad_Daily_Values;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pdata;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class BadValues {
    BufferedReader in = null;

    public void read_bad_values(String precd, int m) {

        Bad_Daily_Values bad_values[] = DailyQcUtils.bad_values;
        int i;
        Scanner s = null;

        try {

            in = new BufferedReader(new FileReader(precd));
            for (i = 0; i < 6000; i++) {

                if (bad_values[i].used == 1) {
                    continue;
                }

                String vals = in.readLine();

                if (vals == null) {
                    break;
                }

                // ier=sscanf(ibuf,"%s %s %d %f",hb5,pc,&iquart,&fvalue);
                s = new Scanner(vals);

                bad_values[i].used = 1;
                bad_values[i].hb5 = s.next();
                bad_values[i].parm = s.next();
                bad_values[i].day = m;
                bad_values[i].quart = (int) s.nextDouble();
                bad_values[i].fvalue = s.nextFloat();
            }
            in.close();
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            System.out.println("File not found " + precd);
            return;
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            return;
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    public int get_bad_values(int iday, int iquart)

    {
        Bad_Daily_Values bad_values[] = DailyQcUtils.bad_values;
        ReadPrecipStationList rp = new ReadPrecipStationList();
        int i, j, h;

        for (i = 0; i < 6000; i++) {

            if (bad_values[i].used == 0) {
                continue;
            }

            if (bad_values[i].day != iday || bad_values[i].quart != iquart) {
                continue;
            }

            for (j = 0; j < rp.getNumPstations(); j++) {

                if (bad_values[i].hb5.equals(DailyQcUtils.precip_stations
                        .get(j).hb5)
                        && bad_values[i].parm.charAt(4) == DailyQcUtils.precip_stations
                                .get(j).parm.charAt(4)) {

                    if (DailyQcUtils.pdata[iday].stn[j].frain[iquart].qual == 5
                            && bad_values[i].fvalue != DailyQcUtils.pdata[iday].stn[j].rrain[iquart].data
                            && DailyQcUtils.pdata[iday].stn[j].rrain[iquart].data >= 0) {

                        /* eliminate all bad values for current month */
                        for (h = 0; h < 6000; h++) {

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

                    }

                    else {

                        DailyQcUtils.pdata[iday].stn[j].frain[iquart].qual = 1;
                        DailyQcUtils.pdata[iday].stn[j].frain[iquart].data = bad_values[i].fvalue;
                    }
                }
            }
        }
        return 0;
    }

    public void write_bad_values(String fname, int iday) {

        String ibuf;
        int i;
        File bfile = new File(fname);
        BufferedWriter out = null;
        Bad_Daily_Values bad_values[] = DailyQcUtils.bad_values;

        try {
            out = new BufferedWriter(new FileWriter(bfile));

            for (i = 0; i < 6000; i++) {

                if (bad_values[i].used == 0) {
                    continue;
                }

                if (iday == bad_values[i].day) {

                    if (bad_values[i].fvalue < 0) {

                        System.out.println("Attempt to write value < 0\n");
                        continue;
                    }

                    // ier=sprintf(ibuf,"%s %s %d %f\n",bad_values[i].hb5,bad_values[i].parm,
                    // bad_values[i].quart,bad_values[i].fvalue);

                    ibuf = String.format("%s %s %d %f", bad_values[i].hb5,
                            bad_values[i].parm, bad_values[i].quart,
                            bad_values[i].fvalue);
                    out.write(ibuf);
                    out.newLine();

                }

            }

            out.close();
        } catch (IOException e) {
            e.printStackTrace();
            System.out.println("Could not open file: " + bfile);
        } finally {
            try {
                if (out != null) {
                    out.close();
                }
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return;
    }

    public void update_bad_values(int iday) {

        int i, j, h, k;
        Bad_Daily_Values bad_values[] = DailyQcUtils.bad_values;
        ArrayList<Station> station = DailyQcUtils.precip_stations;
        ReadPrecipStationList rp = new ReadPrecipStationList();
        Pdata[] pdata = DailyQcUtils.pdata;
        int max_stations = rp.getNumPstations();

        for (i = 0; i < 6000; i++) {

            if (bad_values[i].used == 0) {
                continue;
            }

            if (bad_values[i].day != iday) {
                continue;
            }

            bad_values[i].used = 0;

        }

        for (j = 0; j < max_stations; j++) {

            for (k = 0; k < 5; k++) {

                if (pdata[iday].stn[j].frain[k].qual != 1) {
                    continue;
                }

                for (h = 0; h < 6000; h++) {

                    if (bad_values[h].used != 0) {
                        continue;
                    }

                    bad_values[h].used = 1;

                    /*
                     * since allow TD and partial gage set as bad, then missing
                     * level 1 data will be displayed as -1 on DQC, now need to
                     * retain the original level 2 value on DQC when set as bad
                     */

                    bad_values[h].fvalue = pdata[iday].stn[j].frain[k].data;

                    // bad_values[h].fvalue = pdata[iday].stn[j].rrain[k].data;

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

    public void restore_bad_values(int iday,
            ArrayList<Station> precip_stations, int max_stations) {
        Bad_Daily_Values bad_values[] = DailyQcUtils.bad_values;

        int i, j, k;

        for (k = 0; k < 5; k++) {

            for (i = 0; i < 6000; i++) {

                if (bad_values[i].used == 0) {
                    continue;
                }

                if (bad_values[i].day != iday || bad_values[i].quart != k) {
                    continue;
                }

                for (j = 0; j < max_stations; j++) {

                    if ((bad_values[i].hb5.equalsIgnoreCase(precip_stations
                            .get(j).hb5))
                            && bad_values[i].parm.charAt(4) == precip_stations
                                    .get(j).parm.charAt(4)) {

                        DailyQcUtils.pdata[iday].stn[j].frain[k].data = bad_values[i].fvalue;
                        DailyQcUtils.pdata[iday].stn[j].frain[k].qual = 1;

                        /* fix for 6 hourly bad but 24 hour good */

                        if (k >= 0
                                && k <= 3
                                && DailyQcUtils.pdata[iday].stn[j].rrain[4].data >= 0) {

                            DailyQcUtils.pdata[iday].stn[j].frain[4].qual = 1;

                        }

                        break;

                    }

                }
            }

        }

        return;

    }

    public int is_bad(int iday, int iquart, String hb5, String parm)

    {
        Bad_Daily_Values bad_values[] = DailyQcUtils.bad_values;
        ReadPrecipStationList rp = new ReadPrecipStationList();
        int i, j;

        for (i = 0; i < 6000; i++) {

            if (bad_values[i].used == 0) {
                continue;
            }

            if (bad_values[i].day != iday || bad_values[i].quart != iquart) {
                continue;
            }

            for (j = 0; j < rp.getNumPstations(); j++) {

                if (bad_values[i].hb5.equals(hb5)
                        && bad_values[i].parm.charAt(4) == parm.charAt(4)) {
                    return 1;
                }

            }
        }

        return 0;

    }

    public void post_bad_values(int iday) {

        int i, j, k;
        Bad_Daily_Values bad_values[] = DailyQcUtils.bad_values;
        ReadPrecipStationList rp = new ReadPrecipStationList();

        for (k = 0; k < 5; k++) {

            for (i = 0; i < 6000; i++) {

                if (bad_values[i].used == 0) {
                    continue;
                }

                if (bad_values[i].day != iday || bad_values[i].quart != k) {
                    continue;
                }

                for (j = 0; j < rp.getNumPstations(); j++) {

                    if ((bad_values[i].hb5.equals(DailyQcUtils.precip_stations
                            .get(j).hb5))
                            && bad_values[i].parm.charAt(4) == DailyQcUtils.precip_stations
                                    .get(j).parm.charAt(4)) {

                        if (DailyQcUtils.pdata[iday].stn[j].frain[k].data == bad_values[i].fvalue) {

                            DailyQcUtils.pdata[iday].stn[j].frain[k].data = bad_values[i].fvalue;

                            DailyQcUtils.pdata[iday].stn[j].frain[k].qual = 1;

                        }

                        break;

                    }

                }
            }

        }
        return;
    }

}
