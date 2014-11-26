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
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.DailyQcUtils.Tdata;

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

public class BadTValues {
    BufferedReader in = null;
    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    public void read_bad_tvalues(String tpointd, int m) {

        Bad_Daily_Values bad_tvalues[] = dqc.bad_tvalues;
        int i;
        Scanner s = null;

        try {

            in = new BufferedReader(new FileReader(tpointd));
            for (i = 0; i < 6000; i++) {

                if (bad_tvalues[i].used == 1) {
                    continue;
                }

                String vals = in.readLine();

                if (vals == null) {
                    break;
                }

                // ier=sscanf(ibuf,"%s %s %d %f",hb5,pc,&iquart,&fvalue);
                s = new Scanner(vals);

                bad_tvalues[i].used = 1;
                bad_tvalues[i].hb5 = s.next();
                bad_tvalues[i].parm = s.next();
                bad_tvalues[i].day = m;
                bad_tvalues[i].quart = (int) s.nextDouble();
                bad_tvalues[i].fvalue = s.nextFloat();
            }
            in.close();
        } catch (FileNotFoundException e) {
            System.out.println("File not found " + tpointd);
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

    public void purge_bad_tvalues(int iday) {
        Bad_Daily_Values bad_tvalues[] = dqc.bad_tvalues;
        int i, k;

        for (k = 0; k < 6; k++) {

            for (i = 0; i < 6000; i++) {

                if (bad_tvalues[i].used == 1 && bad_tvalues[i].day == iday
                        && bad_tvalues[i].quart == k) {
                    bad_tvalues[i].used = 0;
                }
            }
        }
        return;
    }

    public int get_bad_tvalues(int iday, int iquart)

    {
        Bad_Daily_Values bad_tvalues[] = dqc.bad_tvalues;
        ReadTemperatureStationList rt = new ReadTemperatureStationList();
        int i, j, h;

        for (i = 0; i < 6000; i++) {

            if (bad_tvalues[i].used == 0) {
                continue;
            }

            if (bad_tvalues[i].day != iday || bad_tvalues[i].quart != iquart) {
                continue;
            }

            for (j = 0; j < rt.getNumTstations(); j++) {

                if (bad_tvalues[i].hb5.equals(dqc.temperature_stations
                        .get(j).hb5)
                        && bad_tvalues[i].parm.charAt(4) == dqc.temperature_stations
                                .get(j).parm.charAt(4)) {

                    if (dqc.tdata[iday].tstn[j].tlevel2[iquart].qual == 5
                            && bad_tvalues[i].fvalue != dqc.tdata[iday].tstn[j].tlevel1[iquart].data
                            && dqc.tdata[iday].tstn[j].tlevel1[iquart].data >= 0) {

                        /* eliminate all bad values for current month */
                        for (h = 0; h < 6000; h++) {

                            if (bad_tvalues[i].used == 0) {
                                continue;
                            }

                            if (bad_tvalues[i].day != iday
                                    || bad_tvalues[i].quart != iquart) {
                                continue;
                            }

                            bad_tvalues[h].used = 0;

                        }

                        /* swap in level 1 data */

                        return 1;

                    }

                    else {

                        dqc.tdata[iday].tstn[j].tlevel2[iquart].qual = 1;
                        dqc.tdata[iday].tstn[j].tlevel2[iquart].data = bad_tvalues[i].fvalue;
                    }
                }
            }
        }
        return 0;
    }

    public void write_bad_tvalues(String fname, int iday) {

        String ibuf;
        int i;
        File bfile = new File(fname);
        BufferedWriter out = null;
        Bad_Daily_Values bad_tvalues[] = dqc.bad_tvalues;

        try {
            out = new BufferedWriter(new FileWriter(bfile));

            for (i = 0; i < 6000; i++) {

                if (bad_tvalues[i].used == 0) {
                    continue;
                }

                if (iday == bad_tvalues[i].day) {

                    // ier=sprintf(ibuf,"%s %s %d %f\n",bad_values[i].hb5,bad_values[i].parm,
                    // bad_values[i].quart,bad_values[i].fvalue);

                    ibuf = String.format("%s %s %d %f", bad_tvalues[i].hb5,
                            bad_tvalues[i].parm, bad_tvalues[i].quart,
                            bad_tvalues[i].fvalue);
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

    public void update_bad_tvalues(int iday) {
        int i, j, h, k;
        char parm[] = new char[10];
        Bad_Daily_Values bad_tvalues[] = dqc.bad_tvalues;
        ReadTemperatureStationList rt = new ReadTemperatureStationList();

        for (i = 0; i < 6000; i++) {
            if (bad_tvalues[i].used == 0) {
                continue;
            }

            if (bad_tvalues[i].day != iday) {
                continue;
            }

            bad_tvalues[i].used = 0;

        }

        for (j = 0; j < rt.getNumTstations(); j++) {

            for (k = 0; k < 6; k++) {

                if (dqc.tdata[iday].tstn[j].tlevel2[k].qual != 1) {
                    continue;
                }

                for (h = 0; h < 6000; h++) {

                    if (bad_tvalues[h].used != 0) {
                        continue;
                    }

                    bad_tvalues[h].used = 1;

                    bad_tvalues[h].fvalue = dqc.tdata[iday].tstn[j].tlevel1[k].data;

                    bad_tvalues[h].hb5 = dqc.temperature_stations
                            .get(j).hb5;

                    parm = dqc.temperature_stations.get(j).parm
                            .toCharArray();

                    if (k < 4) {
                        parm[5] = 'Z';
                    } else if (k == 4) {
                        parm[5] = 'X';
                    }

                    if (k == 6) {
                        parm[5] = 'N';
                    }

                    bad_tvalues[h].parm = parm.toString();
                    bad_tvalues[h].day = iday;
                    bad_tvalues[h].quart = k;

                    break;
                }
            }
        }
        return;
    }

    public void restore_bad_tvalues(int iday,
            ArrayList<Station> temperature_stations, int max_tstations) {

        int i, j, k;

        for (k = 0; k < 6; k++) {

            for (i = 0; i < 6000; i++) {

                if (dqc.bad_tvalues[i].used == 0) {
                    continue;
                }

                if (dqc.bad_tvalues[i].day != iday
                        || dqc.bad_tvalues[i].quart != k) {
                    continue;
                }

                for (j = 0; j < max_tstations; j++) {

                    if ((dqc.bad_tvalues[i].hb5
                            .equalsIgnoreCase(temperature_stations.get(j).hb5) && dqc.bad_tvalues[i].parm
                            .charAt(4) == temperature_stations.get(j).parm
                            .charAt(4))) {

                        dqc.tdata[iday].tstn[j].tlevel2[k].data = dqc.bad_tvalues[i].fvalue;
                        dqc.tdata[iday].tstn[j].tlevel2[k].qual = 1;

                        break;

                    }

                }
            }

        }

        return;

    }

    public int is_tbad(int iday, int iquart, String hb5, String parm)

    {
        Bad_Daily_Values bad_tvalues[] = dqc.bad_tvalues;
        ReadTemperatureStationList rt = new ReadTemperatureStationList();
        int i, j;

        for (i = 0; i < 6000; i++) {

            if (bad_tvalues[i].used == 0) {
                continue;
            }

            if (bad_tvalues[i].day != iday || bad_tvalues[i].quart != iquart) {
                continue;
            }

            for (j = 0; j < rt.getNumTstations(); j++) {

                if (bad_tvalues[i].hb5.equals(hb5)
                        && bad_tvalues[i].parm.charAt(4) == parm.charAt(4)) {
                    return 1;
                }

            }
        }

        return 0;

    }

    public void post_bad_tvalues(int iday) {

        int i, j, k;
        Bad_Daily_Values bad_tvalues[] = dqc.bad_tvalues;
        Tdata tdata[] = dqc.tdata;
        ReadTemperatureStationList rt = new ReadTemperatureStationList();

        for (k = 0; k < 6; k++) {

            for (i = 0; i < 6000; i++) {

                if (bad_tvalues[i].used == 0) {
                    continue;
                }

                if (bad_tvalues[i].day != iday || bad_tvalues[i].quart != k) {
                    continue;
                }

                for (j = 0; j < rt.getNumTstations(); j++) {

                    if ((bad_tvalues[i].hb5
                            .equals(dqc.temperature_stations.get(j).hb5))
                            && bad_tvalues[i].parm.charAt(4) == dqc.temperature_stations
                                    .get(j).parm.charAt(4)) {

                        if (tdata[iday].tstn[j].tlevel2[k].data == bad_tvalues[i].fvalue) {

                            tdata[iday].tstn[j].tlevel2[k].data = bad_tvalues[i].fvalue;

                            tdata[iday].tstn[j].tlevel2[k].qual = 1;
                        }

                        break;
                    }
                }
            }
        }
        return;
    }
}
