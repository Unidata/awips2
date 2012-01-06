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
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.DailyQcUtils.Zdata;

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

public class ReadZlevelB {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReadZlevelB.class);

    BufferedReader in = null;

    DailyQcUtils qc = new DailyQcUtils();

    boolean auto_dailyqc_flag = qc.isAuto_dailyqc_flag();

    public int read_zlevel_b(String zpointb, Date time, int i, Zdata[] zdata,
            ArrayList<Station> freezing_stations, int numZstations) {
        int j;
        int k;
        int m;
        int qual;
        String kbuf = "";
        String buf = "";
        String hb5 = "";
        int number_found[] = new int[5];
        char pc;
        String parmbuf = "";
        int maxk = 0, startk = 0;
        int uflag[] = new int[4];
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        String dt = sdf.format(time);
        /* Store the time in ticks of the data. */
        zdata[i].data_time = time;
        /* Store the GMT time as a string of format YYYYMMDD. */
        zdata[i].ztime = dt;

        try {

            in = new BufferedReader(new FileReader(zpointb));
            for (j = 0; j < 4; j++) {
                number_found[j] = 0;
                uflag[j] = 0;
            }

            for (k = 0; k < numZstations; k++) {
                for (m = 0; m < 5; m++) {
                    zdata[i].zstn[k].zlevel2[m].data = -1;
                    zdata[i].zstn[k].zlevel2[m].qual = -1;
                }
            }
            for (k = 0; k < 5; k++) {
                zdata[i].used[k] = 1;
                zdata[i].level[k] = 2;
            }
            /* initialize structure */
            int p = 1;
            int qq = 0;
            String q = "";

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
                pc = parmbuf.charAt(e + 5);
                if (e < 0) {
                    continue;
                }
                for (j = 0; j < numZstations; j++) {
                    if (hb5.equals(freezing_stations.get(j).hb5)
                            && pc == freezing_stations.get(j).parm.charAt(4)) {
                        break;
                    }
                }
                if (j == numZstations) {
                    continue;
                }
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
                    continue;
                } /* missing */

                maxk = 4;
                startk = 0;
                for (k = startk; k < maxk; k++) {

                    zdata[i].zstn[j].zlevel2[k].qual = 0;

                    Scanner a = new Scanner(q);
                    if (((q.indexOf('/', qq) < 0) && ((q.isEmpty())))) {
                        continue bad;
                    }

                    p = 0;

                    a.close();
                    buf = q;
                    a = new Scanner(buf);
                    int c = buf.indexOf('/');
                    int pp = 0;
                    if (buf.indexOf('.') < 0 || buf.indexOf('.') > c) {

                        /*
                         * Can't find a floating point value. Check if the value
                         * is report missing.
                         */
                        if (a.hasNext("m") || a.hasNext("M")) {
                            uflag[k] = 1;
                        }
                    } else {
                        char r;
                        buf = buf.trim();
                        number_found[k]++;

                        p = buf.indexOf('.');
                        pp = buf.indexOf('/', p);
                        String val;
                        if (p < 0) {
                            p = 0;
                            if (pp < 0) {
                                val = buf.substring(p, buf.length());
                                val = val.trim();
                                r = val.charAt(val.length() - 1);
                                val = val.substring(0, val.length() - 1);
                            } else {
                                val = buf.substring(p, pp);
                                val = val.trim();
                                r = val.charAt(val.length() - 1);
                                val = val.substring(0, val.length() - 1);
                            }
                        } else {
                            if (pp < 0) {
                                val = buf.substring(p, buf.length());
                                val = val.trim();
                                r = val.charAt(val.length() - 1);
                                val = val.substring(0, val.length() - 1);
                            } else {
                                val = buf.substring(0, pp);
                                val = val.trim();
                                r = val.charAt(val.length() - 1);
                                val = val.substring(0, val.length() - 1);
                            }
                        }
                        zdata[i].zstn[j].zlevel2[k].data = Float.parseFloat(val
                                .trim());

                        qual = 8;
                        while (r != 0) {

                            if (r != ' ') {

                                if (r == 'S') {
                                    qual = 8;
                                } else if (r == 'E') {
                                    qual = 5;
                                } else if (r == 'W') {
                                    qual = 2;
                                } else if (r == 'F') {
                                    qual = 1;
                                }

                                zdata[i].zstn[j].zlevel2[k].qual = (short) qual;
                                if (auto_dailyqc_flag == false) {
                                    if ((qual == 8)
                                            && (zdata[i].zstn[j].zlevel2[k].data != zdata[i].zstn[j].zlevel1[k].data)) {
                                        float l2 = zdata[i].zstn[j].zlevel2[k].data;
                                        float l1 = zdata[i].zstn[j].zlevel1[k].data;
                                        System.out
                                                .println("Qual = 8 and L2 != L1 "
                                                        + l2 + " != " + l1);
                                        uflag[k] = 1;
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

            }// while
            in.close();
            if (auto_dailyqc_flag) {
                for (m = 0; m < 4; m++) {

                    if (uflag[m] == 1) {

                        number_found[m] = 0;

                        for (k = 0; k < numZstations; k++) {

                            zdata[i].zstn[k].zlevel2[m].data = zdata[i].zstn[k].zlevel1[m].data;

                            zdata[i].zstn[k].zlevel2[m].qual = zdata[i].zstn[k].zlevel1[m].qual;

                            if (zdata[i].zstn[k].zlevel2[m].data >= 0) {
                                number_found[m]++;
                            }

                        }

                        zdata[i].level[m] = 1;

                    }

                }
            }
            for (j = 0; j < 4; j++) {

                if (number_found[j] == 0) {

                    zdata[i].used[j] = 0;
                    zdata[i].level[j] = 1;

                }

            }
            return 1;
        } catch (FileNotFoundException e) {
            System.out.println("File not found " + zpointb);
            for (k = 0; k < numZstations; k++) {
                for (m = 0; m < 4; m++) {
                    zdata[i].zstn[k].zlevel2[m].data = zdata[i].zstn[k].zlevel1[m].data;
                    zdata[i].zstn[k].zlevel2[m].qual = zdata[i].zstn[k].zlevel1[m].qual;
                    if (zdata[i].zstn[k].zlevel2[m].data >= 0) {
                        number_found[m]++;
                    }
                }
            }

            for (m = 0; m < 4; m++) {
                if (number_found[m] == 0) {
                    zdata[i].used[m] = 0;
                } else {
                    zdata[i].level[m] = 1;
                    zdata[i].used[m] = 1;
                }
            }
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
    }

    /**
     * @param zpointb
     * @param time
     * @param m
     * @param zdata
     * @param freezing_stations
     * @param numZstations
     * @return
     */
    public int read_zlevel_b_autodailyqc(String zpointb, Date time, int m,
            Zdata[] zdata, ArrayList<Station> freezing_stations,
            int numZstations) {
        // TODO Auto-generated method stub
        return 0;
    }
}
