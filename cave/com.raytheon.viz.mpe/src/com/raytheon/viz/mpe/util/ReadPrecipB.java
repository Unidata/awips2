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
 * Mar 6, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class ReadPrecipB {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReadPrecipB.class);

    DailyQcUtils qc = new DailyQcUtils();

    boolean auto_dailyqc_flag = qc.isAuto_dailyqc_flag();

    BufferedReader in = null;

    BadValues bv = new BadValues();

    int number_found[] = new int[5];

    int rier = -1;

    int retval = 1;

    public int read_precip_b(String precb, Date time, int i, Pdata[] pdata,
            ArrayList<Station> precip_stations, int numPstations) {
        int j;
        int k;
        int m;
        int qual;
        String kbuf = "";
        String buf = "";
        String hb5 = "";
        int type = 0;
        char pc;
        String parmbuf = "";
        int maxk = 0, startk = 0;
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        String dt = sdf.format(time);
        /* Store the time in ticks of the data. */
        DailyQcUtils.pdata[i].data_time.setTime(time.getTime());
        /* Store the GMT time as a string of format YYYYMMDD. */
        DailyQcUtils.pdata[i].ztime = dt;
        int reset_flag = 0;
        int fe = 1;
        File pb = new File(precb);
        if (pb.lastModified() == 0 || pb.length() == 0) {
            fe = 0;
        }
        for (k = 0; k < numPstations; k++) {
            for (m = 0; m < 5; m++) {
                DailyQcUtils.pdata[i].stn[k].frain[m].data = -1;
                DailyQcUtils.pdata[i].stn[k].frain[m].qual = -1;
            }
        }

        /* The file could not be opened. Maybe it does not exist. */
        if (fe == 0) {
            retval = reset_p(i, numPstations);
        } else {

            try {

                in = new BufferedReader(new FileReader(precb));

                int p = 1;
                int qq = 0;
                String q = "";
                String sn = kbuf;
                Scanner s = new Scanner(sn);

                /* Otherwise, the level 2 file was found. */
                for (k = 0; k < 5; k++) {
                    number_found[k] = 0;
                    DailyQcUtils.pdata[i].used[k] = 1;
                    DailyQcUtils.pdata[i].level = 2;
                }

                /* initialize structure */

                /* Process the reports. */

                bad: while (in.ready()) {

                    kbuf = in.readLine().trim();
                    s = new Scanner(kbuf);

                    if (kbuf.charAt(0) == ':') {
                        continue;
                    }
                    /*
                     * Determine the type of the SHEF report the precip
                     * information is stored in. E is a timeseries, A is a
                     * single value.
                     */
                    if (kbuf.charAt(0) == '.' && kbuf.charAt(1) == 'E') {
                        type = 1;
                    }

                    if (kbuf.charAt(0) == '.' && kbuf.charAt(1) == 'A') {
                        type = 0;
                    }
                    /* Read the handbook5 identifier, the date and the data. */
                    // ier=sscanf(&kbuf[2],"%s %s %s",hb5,datbuf,parmbuf);
                    // s.skip("..");
                    s.next();
                    hb5 = s.next();
                    s.next();
                    parmbuf = s.next();

                    if (hb5 == null) {
                        /* Error reading record. */
                        continue;
                    }
                    int e = parmbuf.indexOf('/');
                    pc = parmbuf.charAt(e + 5);

                    for (j = 0; j < numPstations; j++) {

                        /*
                         * Try to find a matching station id and source in the
                         * stations file.
                         */
                        if ((hb5.equals(precip_stations.get(j).hb5))
                                && (pc == precip_stations.get(j).parm.charAt(4))) {
                            break;
                        }
                    }

                    if (j == numPstations) {
                        /*
                         * A matching station id and source could not be found.
                         * Read the next record.
                         */
                        continue;
                    }

                    if (type == 0) {
                        /* This is a type "A" SHEF Report. */
                        p = kbuf.indexOf('/');
                        if (p < 0) {
                            continue;
                        }

                        qq = kbuf.indexOf(' ', p);
                        if (qq < 0) {
                            continue;
                        }

                        q = kbuf.substring(qq).trim();
                        maxk = 5;
                        startk = 4;

                    }

                    else if (type == 1) {

                        /* This is a type "E" SHEF report. */
                        q = kbuf;
                        for (k = 0; k < 3; k++) {

                            p = q.indexOf('/');
                            if (p < 0) {
                                break;
                            }

                            qq = p + 1;
                            q = q.substring(qq);

                        }

                        if (k != 3) {
                            continue; /* missing */
                        }

                        maxk = 4;
                        startk = 0;

                    }
                    // Parse the value out of the Shef report.
                    for (k = startk; k < maxk; k++) {

                        // Set the quality flag to 0
                        DailyQcUtils.pdata[i].stn[j].frain[k].qual = 0;

                        /*
                         * Check if this record is completely unparsable. If it
                         * is, then skip it and read the next record.
                         */
                        Scanner a = new Scanner(q);
                        if (((q.indexOf('/', qq) < 0) && ((q.isEmpty())))) {
                            continue bad;
                        }

                        p = 0;

                        a.close();
                        buf = q;
                        a = new Scanner(buf);
                        /*
                         * Try to parse out the value from the level 2 precip
                         * SHEF report.
                         */
                        int c = buf.indexOf('/');
                        if (buf.indexOf('.') < 0
                                || (buf.indexOf('.') > c && c != -1)) {
                            if (a.hasNext("m") || a.hasNext("M")) {
                                if (auto_dailyqc_flag == false) {
                                    if (DailyQcUtils.pdata[i].stn[j].rrain[k].data >= 0) {
                                        reset_flag = 1;
                                    } else {
                                        /*
                                         * Set the data and quality flags to
                                         * missing.
                                         */
                                        DailyQcUtils.pdata[i].stn[j].frain[k].data = -1;

                                        DailyQcUtils.pdata[i].stn[j].frain[k].qual = -1;

                                    }
                                } else {
                                    /*
                                     * Set the data and quality flags to
                                     * missing.
                                     */
                                    DailyQcUtils.pdata[i].stn[j].frain[k].data = -1;

                                    DailyQcUtils.pdata[i].stn[j].frain[k].qual = -1;

                                }
                            }
                        } else {
                            buf = buf.trim();
                            number_found[k]++;
                            p = buf.indexOf('.');
                            String val;
                            if (p < 0) {
                                p = 0;
                                int pp = buf.indexOf('/', p);
                                if (pp < 0) {
                                    val = buf.substring(p, buf.length());
                                } else {
                                    val = buf.substring(p, pp - 1);
                                }
                            } else {
                                if (p == 1) {
                                    val = buf.substring(0, p + 3);
                                } else {
                                    val = buf.substring(0, p + 3);
                                }
                            }
                            val = val.substring(0, val.length());

                            /* Assign the value to the pdata structure. */
                            DailyQcUtils.pdata[i].stn[j].frain[k].data = Float
                                    .parseFloat(val.trim());

                            /* Process the quality flag. */
                            char r;
                            if (p == 0) {
                                r = buf.charAt(buf.length() - 1);
                            } else {
                                r = buf.charAt(p + 3);
                            }
                            qual = 8;
                            while (r != 0) {

                                if (r != ' ') {

                                    if (r == 'S') {
                                        qual = 0;
                                    } else if (r == 'F') {
                                        qual = 1;
                                    } else if (r == 'W') {
                                        qual = 2;
                                    } else if (r == 'Q') {
                                        qual = 3;
                                    } else if (r == 'D') {
                                        qual = 4;
                                    } else if (r == 'V') {
                                        qual = 8;
                                    } else if (r == 'E') {
                                        qual = 5;
                                    } else if (r == 'L') {
                                        qual = 6;
                                    } else if (r == 'A') {
                                        DailyQcUtils.pdata[i].stn[j].sflag[k] = 1;
                                        qual = 8;
                                    } else if (r == 'B') {
                                        DailyQcUtils.pdata[i].stn[j].sflag[k] = 1;
                                        qual = 0;
                                    } else if (r == 'C') {
                                        DailyQcUtils.pdata[i].stn[j].sflag[k] = 1;
                                        qual = 3;
                                    }

                                    /* Assign qual code to level 2 data array */
                                    DailyQcUtils.pdata[i].stn[j].frain[k].qual = (short) qual;

                                    if (auto_dailyqc_flag == false) {
                                        if ((qual == 5 || qual == 4)
                                                && DailyQcUtils.pdata[i].stn[j].rrain[k].data >= 0) {

                                            int ier = bv
                                                    .is_bad(i,
                                                            k,
                                                            precip_stations
                                                                    .get(j).hb5,
                                                            precip_stations
                                                                    .get(j).parm);

                                            if (ier == 0) {

                                                System.out
                                                        .println("New data overwriting missing "
                                                                + precip_stations
                                                                        .get(j).hb5
                                                                + ", "
                                                                + precip_stations
                                                                        .get(j).parm
                                                                + ", "
                                                                + DailyQcUtils.pdata[i].stn[j].rrain[k].data);
                                                rier = -2;
                                                reset_flag = 1;
                                            }

                                        }
                                        if (DailyQcUtils.pdata[i].stn[j].sflag[k] != 1) {

                                            if ((qual != 5 && qual != 4 && qual != 2)
                                                    && DailyQcUtils.pdata[i].stn[j].rrain[k].data >= 0
                                                    && DailyQcUtils.pdata[i].stn[j].rrain[k].data != DailyQcUtils.pdata[i].stn[j].frain[k].data) {

                                                System.out
                                                        .println("New data overwriting old "
                                                                + precip_stations
                                                                        .get(j).hb5
                                                                + ", "
                                                                + precip_stations
                                                                        .get(j).parm
                                                                + ", "
                                                                + DailyQcUtils.pdata[i].stn[j].rrain[k].data);
                                                rier = -2;
                                                reset_flag = 1;
                                            }
                                            if (qual != 5
                                                    && qual != 6
                                                    && qual != 4
                                                    && qual != 2
                                                    && DailyQcUtils.pdata[i].stn[j].rrain[k].data < 0
                                                    && DailyQcUtils.pdata[i].stn[j].frain[k].data >= 0) {

                                                System.out
                                                        .println("Data set bad level 1 overwriting level 2 "
                                                                + precip_stations
                                                                        .get(j).hb5
                                                                + ", "
                                                                + precip_stations
                                                                        .get(j).parm
                                                                + ", "
                                                                + DailyQcUtils.pdata[i].stn[j].rrain[k].data);

                                                rier = -2;
                                                reset_flag = 1;

                                            }
                                        } else {

                                            if (qual != 5
                                                    && DailyQcUtils.pdata[i].stn[j].srain[k].data >= 0
                                                    && DailyQcUtils.pdata[i].stn[j].srain[k].data != DailyQcUtils.pdata[i].stn[j].frain[k].data) {

                                                System.out
                                                        .println("New data overwriting old "
                                                                + precip_stations
                                                                        .get(j).hb5
                                                                + ", "
                                                                + precip_stations
                                                                        .get(j).parm
                                                                + ", "
                                                                + DailyQcUtils.pdata[i].stn[j].rrain[k].data);

                                                rier = -2;
                                                reset_flag = 1;

                                            }

                                            if (qual != 5
                                                    && qual != 6
                                                    && DailyQcUtils.pdata[i].stn[j].srain[k].data < 0
                                                    && DailyQcUtils.pdata[i].stn[j].frain[k].data >= 0) {

                                                System.out
                                                        .println("Data set bad level 1 overwriting level 2 "
                                                                + precip_stations
                                                                        .get(j).hb5
                                                                + ", "
                                                                + precip_stations
                                                                        .get(j).parm
                                                                + ", "
                                                                + DailyQcUtils.pdata[i].stn[j].rrain[k].data);

                                                rier = -2;
                                                reset_flag = 1;

                                            }

                                        }
                                    }

                                    break;

                                }

                                else {
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
                    if (reset_flag == 1) {
                        retval = reset_p(i, numPstations);
                    }

                }
                in.close();
                s.close();

                if (auto_dailyqc_flag == true) {
                    /* check if new data not avbl on level 2 comes in on level 1 */

                    for (k = 0; k < 5; k++) {

                        for (j = 0; j < numPstations; j++) {

                            if (DailyQcUtils.pdata[i].stn[j].rrain[k].data >= 0
                                    && DailyQcUtils.pdata[i].stn[j].frain[k].data < 0) {

                                System.out.println("New level 1 data "
                                        + precip_stations.get(j).hb5);
                                retval = reset_p(i, numPstations);

                            }

                        }

                    }

                    /* check for new data overwriting bad data */

                    for (m = 0; m < 5; m++) {

                        if (number_found[m] == 0) {
                            DailyQcUtils.pdata[i].used[m] = 0;
                        }

                        int ier = bv.get_bad_values(i, m);
                        if (ier == 1) {

                            System.out
                                    .println("New data overwriting old bad data\n");
                            retval = reset_p(i, numPstations);

                        }
                    }

                    return retval;
                } else {
                    for (m = 0; m < 5; m++) {

                        if (number_found[m] == 0) {
                            DailyQcUtils.pdata[i].used[m] = 0;
                        }
                    }
                    return 1;
                }
            } catch (FileNotFoundException e) {
                // TODO Auto-generated catch block
                System.out.println("File not found " + precb);
                for (k = 0; k < 5; k++) {
                    DailyQcUtils.pdata[i].used[k] = 0;
                    DailyQcUtils.pdata[i].level = 1;
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

    private int reset_p(int day, int numPstations) {
        int i = day;
        DailyQcUtils.pdata[i].level = 1;

        /*----------------------------------------------*/
        /* make qual code = Screened sticky */
        /*----------------------------------------------*/

        for (int k = 0; k < numPstations; k++) {

            for (int m = 0; m < 5; m++) {

                if (DailyQcUtils.pdata[i].stn[k].sflag[m] != 1) {

                    // added by zhan for DR # 8960
                    if (DailyQcUtils.pdata[i].stn[k].frain[m].qual == 0
                            && DailyQcUtils.pdata[i].stn[k].frain[m].data == DailyQcUtils.pdata[i].stn[k].rrain[m].data) {
                        DailyQcUtils.pdata[i].stn[k].frain[m].qual = 0;
                    }
                    // changed by zhan for DR # 8960
                    // "if" is changed to "else if"
                    else if (DailyQcUtils.pdata[i].stn[k].frain[m].qual != 2) {
                        DailyQcUtils.pdata[i].stn[k].frain[m].data = DailyQcUtils.pdata[i].stn[k].rrain[m].data;
                        DailyQcUtils.pdata[i].stn[k].frain[m].qual = DailyQcUtils.pdata[i].stn[k].rrain[m].qual;
                    }

                    if (DailyQcUtils.pdata[i].stn[k].frain[m].data >= 0) {
                        number_found[m]++;
                    }

                }

                else {

                    DailyQcUtils.pdata[i].stn[k].frain[m].data = DailyQcUtils.pdata[i].stn[k].srain[m].data;
                    DailyQcUtils.pdata[i].stn[k].frain[m].qual = 8;

                    if (DailyQcUtils.pdata[i].stn[k].frain[m].data >= 0) {
                        number_found[m]++;
                    }

                }

            }

        }

        for (int j = 0; j < 5; j++) {

            if (number_found[j] == 0) {
                DailyQcUtils.pdata[i].used[j] = 0;
                // DailyQcUtils.pdata[i].level = 1;
            }

        }
        bv.post_bad_values(i);
        return rier;
    }
}
