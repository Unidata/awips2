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

public class ReadPrecipA {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReadPrecipA.class);

    BufferedReader in = null;

    public int read_precip_a(String preca, Date time, int i, Pdata[] pdata,
            ArrayList<Station> precip_stations, int numPstations) {
        int j;
        int k;
        int m;
        int qual;
        String kbuf = "";
        String buf = "";
        String hb5 = "";
        int number_found[] = new int[5];
        int type = 0;
        char pc;
        String parmbuf = "";
        int maxk = 0, startk = 0;
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        String dt = sdf.format(time);
        /* Store the time in ticks of the data. */
        pdata[i].data_time = time;
        /* Store the GMT time as a string of format YYYYMMDD. */
        pdata[i].ztime = dt;
        // logMessage("ztime %d\n", pdata[i].ztime);

        try {

            in = new BufferedReader(new FileReader(preca));

            for (j = 0; j < 5; j++) {
                number_found[j] = 0;
                pdata[i].used[j] = 1;
                pdata[i].level = 1;
            }
            for (k = 0; k < numPstations; k++) {
                for (m = 0; m < 5; m++) {
                    pdata[i].stn[k].rrain[m].data = -1;
                    pdata[i].stn[k].rrain[m].qual = -1;
                }
            }

            int p = 1;
            int qq = 0;
            String q = "";
            bad: while (in.ready()) {

                kbuf = in.readLine().trim();
                Scanner s = new Scanner(kbuf);

                if (kbuf.charAt(0) == ':') {
                    continue;
                }
                /*
                 * Determine the type of the SHEF report the precip information
                 * is stored in. E is a timeseries, A is a single value.
                 */
                if (kbuf.charAt(0) == '.' && kbuf.charAt(1) == 'E') {
                    type = 1;
                }

                if (kbuf.charAt(0) == '.' && kbuf.charAt(1) == 'A') {
                    type = 0;
                }
                /* Read the handbook5 identifier, the date and the data. */
                // s.skip("..");
                // ier=sscanf(&kbuf[2],"%s %s %s",hb5,datbuf,parmbuf);
                s.next();
                hb5 = s.next();
                s.next();
                parmbuf = s.next();

                if (hb5 == null) {
                    /* Error reading record. */
                    continue;
                }
                int e = parmbuf.indexOf('/');
                if (e == -1) {
                    continue;
                }
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
                     * A matching station id and source could not be found. Read
                     * the next record.
                     */
                    continue;
                }

                if (type == 0) {
                    /* This is a type "A" SHEF Report. */
                    q = kbuf;
                    p = kbuf.indexOf('/');
                    if (p < 0) {
                        continue;
                    }

                    qq = kbuf.indexOf(' ', p);
                    if (qq < 0) {
                        continue;
                    }
                    q = q.substring(qq);

                    maxk = 5;
                    startk = 4;

                }

                else if (type == 1) {

                    /* This is a type "E" SHEF report. */
                    q = kbuf;
                    qq = 0;
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
                for (k = startk; k < maxk; k++) {

                    pdata[i].stn[j].rrain[k].qual = 0;

                    Scanner a = new Scanner(q);
                    if (((q.indexOf('/', qq) < 0) && ((q.isEmpty())))
                            || q.length() <= 0 || q == null) {
                        continue bad;
                    }

                    p = 0;

                    a.close();
                    buf = q;
                    a = new Scanner(buf);

                    int c = buf.indexOf('/');
                    if (buf.indexOf('.') < 0
                            || (buf.indexOf('.') > c && c != -1)) {

                        /*
                         * Can't find a floating point value. Check if the value
                         * is report missing.
                         */
                        if (a.hasNext("m") || a.hasNext("M")) {
                            qq = q.indexOf('m');
                            /* The value is reported missing. */
                            pdata[i].stn[j].rrain[k].data = -99;
                            pdata[i].stn[j].rrain[k].qual = -99;
                        } else {
                            /* The value is bad. */
                            pdata[i].stn[j].rrain[k].data = -2;
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
                            val = buf.substring(0, p + 3);
                        }
                        val = val.substring(0, val.length());
                        /* Assign the value to the pdata structure. */
                        pdata[i].stn[j].rrain[k].data = Float.parseFloat(val
                                .trim());

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
                                    qual = 8;
                                } else if (r == 'F') {
                                    qual = 1;
                                } else if (r == 'L') {
                                    qual = 2;
                                } else if (r == 'Q') {
                                    qual = 3;
                                } else if (r == 'D') {
                                    qual = 4;
                                } else if (r == 'V') {
                                    qual = 8;
                                } else if (r == 'E') {
                                    qual = 5;
                                } else if (r == 'T') {
                                    qual = 6;
                                }

                                pdata[i].stn[j].rrain[k].qual = (short) qual;

                                break;

                            } else {
                                r++;
                            }

                        }

                    }

                    // p = q.indexOf(' ', p);
                    p = q.indexOf('/', p);
                    qq = p + 1;
                    q = q.substring(qq);
                    qq = 0;
                    a.close();

                }
                s.close();
            }

            in.close();

            for (j = 0; j < 5; j++) {
                if (number_found[j] == 0) {
                    pdata[i].used[j] = 0;
                }
            }

        } catch (FileNotFoundException e) {
            System.out.println("File not found " + preca);
            for (k = 0; k < 5; k++) {
                pdata[i].used[k] = 0;
            }
            pdata[i].level = 0;
            return -1;
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return -1;
        } catch (NumberFormatException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
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
        return 1;
    }
}
