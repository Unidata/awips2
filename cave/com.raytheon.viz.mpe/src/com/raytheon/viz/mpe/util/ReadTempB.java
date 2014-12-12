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
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Scanner;
import java.util.TimeZone;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
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

public class ReadTempB {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReadTempB.class);

    BufferedReader in = null;

    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    boolean auto_dailyqc_flag = dqc.isAuto_dailyqc_flag();

//    Tdata tdata[] = DailyQcUtils.tdata;

    int retval = 1;

    int number_found[] = new int[6];

    @SuppressWarnings("unused")
    private int rier;

    BadTValues bt = new BadTValues();

    public int read_t_b(String tpointb, Date time, int i, Tdata[] tdata,
            ArrayList<Station> temperature_stations, int numTstations) {

        int j;
        int k;
        int m;
        int qual;
        int offset = 0;
        int kread;
        String kbuf = "";
        String buf = "";
        String hb5 = "";
        char pc;
        String parmbuf = "";
        int maxk = 0, startk = 0;
        int uflag[] = new int[6];
        int reset_flag = 0;
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        String dt = sdf.format(time);
        /* Store the time in ticks of the data. */
        tdata[i].data_time = time;
        /* Store the GMT time as a string of format YYYYMMDD. */
        tdata[i].ztime = dt;
        int fe = 1;
        File tb = new File(tpointb);
        if (tb.lastModified() == 0 || tb.length() == 0) {
            fe = 0;
        }

        if (fe == 0) {
            retval = reset_t(i, numTstations);

        } else {
            try {

                in = new BufferedReader(new FileReader(tpointb));

                for (j = 0; j < 6; j++) {
                    number_found[j] = 0;
                    uflag[j] = 0;
                    tdata[i].used[j] = 1;
                    tdata[i].level[j] = 2;
                }
                for (k = 0; k < numTstations; k++) {
                    for (m = 0; m < 6; m++) {
                        tdata[i].tstn[k].tlevel2[m].data = -1;
                        tdata[i].tstn[k].tlevel2[m].qual = -1;
                    }
                }

                int p = 1;
                int qq = 0;
                String q = "";

                /* The file could not be opened. Maybe it does not exist. */
                if (fe == 0) {
                    retval = reset_t(i, numTstations);
                } else {
                    bad: while (in.ready()) {

                        kbuf = in.readLine().trim();
                        Scanner s = new Scanner(kbuf);

                        if (kbuf.charAt(0) == ':') {
                            continue;
                        }
                        s.next();
                        hb5 = s.next();
                        s.next();
                        parmbuf = s.next();

                        if (hb5 == null) {
                            /* Error reading record. */
                            continue;
                        }
                        int e = parmbuf.indexOf('/');
                        pc = parmbuf.charAt(e + 6);

                        if (pc == 'X') {
                            offset = 7;
                            kread = 1;
                            startk = 4;
                            maxk = 5;
                        } else if (pc == 'N') {
                            offset = 7;
                            kread = 1;
                            startk = 5;
                            maxk = 6;
                        } else {
                            offset = 0;
                            kread = 3;
                            startk = 0;
                            maxk = 4;
                        }

                        pc = parmbuf.charAt(e + 5);
                        for (j = 0; j < numTstations; j++) {

                            /*
                             * Try to find a matching station id and source in
                             * the stations file.
                             */
                            if ((hb5.equals(temperature_stations.get(j).hb5))
                                    && (pc == temperature_stations.get(j).parm
                                            .charAt(4))) {
                                break;
                            }
                        }

                        if (j == numTstations) {
                            /*
                             * A matching station id and source could not be
                             * found. Read the next record.
                             */
                            continue;
                        }

                        q = kbuf;
                        qq = 0;

                        for (k = 0; k < kread; k++) {

                            p = q.indexOf('/');
                            if (p < 0) {
                                break;
                            }
                            qq = p + 1;
                            q = q.substring(qq);
                        }
                        if (k != kread) {
                            continue;
                        } /* missing */
                        qq = 0;
                        for (k = startk; k < maxk; k++) {
                            tdata[i].tstn[j].tlevel2[k].qual = 0;

                            Scanner a = new Scanner(q);
                            if (((q.indexOf('/', qq) < 0) && ((q.isEmpty())))) {
                                continue bad;
                            }

                            qq = q.indexOf('/');
                            p = 0;

                            a.close();
                            buf = q.substring(offset);
                            a = new Scanner(buf);
                            int pp = 0;
                            /*
                             * Can't find a floating point value. Check if the
                             * value is report missing.
                             */
                            if (a.hasNext("m") || a.hasNext("M")) {
                                if (auto_dailyqc_flag == false) {
                                    if (tdata[i].tstn[j].tlevel1[k].data > -1) {
                                        reset_flag = 1;
                                    }
                                    tdata[i].tstn[j].tlevel2[k].data = -1;
                                    tdata[i].tstn[j].tlevel2[k].qual = -1;
                                } else {
                                    tdata[i].tstn[j].tlevel2[k].data = -1;
                                    tdata[i].tstn[j].tlevel2[k].qual = -1;
                                }
                            } else {
                                char r;
                                buf = buf.trim();
                                number_found[k]++;
                                String val;
                                p = 0;
                                pp = buf.indexOf('/', p);
                                if (pp < 0) {
                                    val = buf.substring(p, buf.length());
                                    val = val.trim();
                                    if (val.length() == 0) {
                                        continue;
                                    }
                                    r = val.charAt(val.length() - 1);
                                    val = val.substring(0, val.length() - 1);
                                } else {
                                    val = buf.substring(0, pp);
                                    val = val.trim();
                                    if (val.length() == 0) {
                                        continue;
                                    }
                                    r = val.charAt(val.length() - 1);
                                    val = val.substring(0, val.length() - 1);
                                }

                                tdata[i].tstn[j].tlevel2[k].data = Float
                                        .parseFloat(val.trim());

                                /* Process the quality flag. */
                                qual = 8;

                                while (r != 0) {

                                    if (Character.isLetter(r)) {

                                        if (r == 'S' || r == 'A') {
                                            qual = 0;
                                        } else if (r == 'F') {
                                            qual = 1;
                                        } else if (r == 'W') {
                                            qual = 2;
                                        } else if (r == 'Q') {
                                            qual = 3;
                                        } else if (r == 'V') {
                                            qual = 8;
                                        } else if (r == 'E') {
                                            qual = 5;
                                        } else if (r == 'L') {
                                            qual = 6;
                                        }
                                        tdata[i].tstn[j].tlevel1[k].qual = (short) qual;

                                        if (auto_dailyqc_flag == false) {
                                            if ((qual == 5)
                                                    && tdata[i].tstn[j].tlevel1[k].data >= 0) {

                                                int ier = bt.is_tbad(i, k,
                                                        temperature_stations
                                                                .get(j).hb5,
                                                        temperature_stations
                                                                .get(j).parm);

                                                if (ier == 0) {

                                                    System.out
                                                            .println(String
                                                                    .format("New data overwriting missing %s %s %d\n",
                                                                            temperature_stations
                                                                                    .get(j).hb5,
                                                                            temperature_stations
                                                                                    .get(j).parm,
                                                                            (int) tdata[i].tstn[j].tlevel1[k].data));

                                                    rier = -2;
                                                    reset_flag = 1;

                                                }

                                            }
                                            if ((qual != 5 && qual != 2)
                                                    && tdata[i].tstn[j].tlevel1[k].data >= 0
                                                    && tdata[i].tstn[j].tlevel1[k].data != tdata[i].tstn[j].tlevel2[k].data) {

                                                System.out
                                                        .println(String
                                                                .format("New data overwriting old %s %s %d %d\n",
                                                                        temperature_stations
                                                                                .get(j).hb5,
                                                                        temperature_stations
                                                                                .get(j).parm,
                                                                        (int) tdata[i].tstn[j].tlevel1[k].data,
                                                                        (int) tdata[i].tstn[j].tlevel2[k].data));

                                                rier = -2;
                                                reset_flag = 1;

                                            }

                                            if (qual != 5
                                                    && qual != 6
                                                    && qual != 2
                                                    && tdata[i].tstn[j].tlevel1[k].data < -98
                                                    && tdata[i].tstn[j].tlevel2[k].data >= 0) {

                                                System.out
                                                        .println(String
                                                                .format("data set bad level 1 overwriting level 2 %s %s %d\n",
                                                                        temperature_stations
                                                                                .get(j).hb5,
                                                                        temperature_stations
                                                                                .get(j).parm,
                                                                        (int) tdata[i].tstn[j].tlevel1[k].data));

                                                rier = -2;
                                                reset_flag = 1;

                                            }
                                        }
                                        break;
                                    } else {
                                        r++;
                                    }
                                }
                            }
                            p = q.indexOf('/', p);
                            qq = p + 1;
                            q = q.substring(qq);
                            qq = 0;
                            a.close();
                        }
                        if (auto_dailyqc_flag == false) {
                            if (reset_flag == 1) {
                                retval = reset_t(i, numTstations);
                            }
                        }
                    }

                    in.close();
                }
                if (auto_dailyqc_flag == false) {
                    for (m = 0; m < 6; m++) {

                        if (uflag[m] == 1) {

                            number_found[m] = 0;

                            for (k = 0; k < numTstations; k++) {

                                tdata[i].tstn[k].tlevel2[m].data = tdata[i].tstn[k].tlevel1[m].data;
                                tdata[i].tstn[k].tlevel2[m].qual = tdata[i].tstn[k].tlevel1[m].qual;

                                if (tdata[i].tstn[k].tlevel2[m].data > -1) {
                                    number_found[m]++;
                                }

                            }

                            tdata[i].level[m] = 1;

                        }

                    }

                    for (j = 0; j < 6; j++) {

                        if (number_found[j] == 0) {

                            tdata[i].used[j] = 0;
                            tdata[i].level[j] = 1;

                        }

                        int ier = bt.get_bad_tvalues(i, m);
                        if (ier == 1) {

                            System.out
                                    .println("new data overwriting old bad data\n");
                            retval = reset_t(i, numTstations);
                        }

                    }// while
                    return retval;
                } else {
                    for (j = 0; j < 6; j++) {

                        if (number_found[j] == 0) {

                            tdata[i].used[j] = 0;
                            tdata[i].level[j] = 1;

                        }
                    }
                    return 1;
                }

            } catch (FileNotFoundException e) {
                System.out.println("File not found " + tpointb);
                for (k = 0; k < 6; k++) {
                    tdata[i].used[k] = 0;
                    tdata[i].level[k] = 1;
                }
                return -1;
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                return -1;
            } catch (NumberFormatException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                return -1;
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
        return retval;
    }

    private int reset_t(int i, int numTstations) {
        for (int k = 0; k < numTstations; k++) {

            for (int m = 0; m < 6; m++) {

                dqc.tdata[i].tstn[k].tlevel2[m].data = dqc.tdata[i].tstn[k].tlevel1[m].data;

                dqc.tdata[i].tstn[k].tlevel2[m].qual = dqc.tdata[i].tstn[k].tlevel1[m].qual;

                if (dqc.tdata[i].tstn[k].tlevel2[m].data >= 0) {
                    number_found[m]++;
                }

            }

        }

        for (int m = 0; m < 6; m++) {

            if (number_found[m] == 0) {
                dqc.tdata[i].used[m] = 0;
                dqc.tdata[i].level[m] = 1;
            }

        }

        bt.post_bad_tvalues(i);

        retval = -1;
        return retval;
    }

    /**
     * @param tpointb
     * @param time
     * @param m
     * @param tdata2
     * @param temperature_stations
     * @param numTstations
     * @return
     */
    public int read_t_b_autodailyqc(String tpointb, Date time, int m,
            Tdata[] tdata2, ArrayList<Station> temperature_stations,
            int numTstations) {
        // TODO Auto-generated method stub
        return 0;
    }

}
