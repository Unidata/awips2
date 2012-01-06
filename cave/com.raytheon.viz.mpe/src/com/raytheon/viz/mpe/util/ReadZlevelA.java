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

public class ReadZlevelA {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReadZlevelA.class);

    BufferedReader in = null;

    public int read_zlevel_a(String zpointa, Date time, int i, Zdata[] zdata,
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
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        String dt = sdf.format(time);
        /* Store the time in ticks of the data. */
        zdata[i].data_time = time;
        /* Store the GMT time as a string of format YYYYMMDD. */
        zdata[i].ztime = dt;

        for (k = 0; k < numZstations; k++) {
            for (m = 0; m < 5; m++) {
                zdata[i].zstn[k].zlevel1[m].data = -99;
                zdata[i].zstn[k].zlevel1[m].qual = -99;
            }
        }

        try {

            in = new BufferedReader(new FileReader(zpointa));
            for (j = 0; j < 5; j++) {
                number_found[j] = 0;
                zdata[i].used[j] = 1;
                zdata[i].level[j] = 1;
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

                    zdata[i].zstn[j].zlevel1[k].qual = 0;

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
                        if (a.hasNext("m") || a.hasNext("M")) {
                            zdata[i].zstn[j].zlevel1[k].data = -99;
                            zdata[i].zstn[j].zlevel1[k].qual = -99;
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

                        zdata[i].zstn[j].zlevel1[k].data = Float.parseFloat(val
                                .trim());

                        qual = 8;
                        while (r != 0) {

                            if (r != ' ') {

                                if (r == 'S') {
                                    qual = 8;
                                }

                                zdata[i].zstn[j].zlevel1[k].qual = (short) qual;

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
            }
            in.close();

            for (j = 0; j < 4; j++) {
                if (number_found[j] == 0) {
                    zdata[i].used[j] = 0;
                } else {
                    zdata[i].used[j] = 1;
                }
            }
            return 1;
        } catch (FileNotFoundException e) {
            System.out.println("File not found " + zpointa);

            for (k = 0; k < 5; k++) {
                zdata[i].used[k] = 0;
                zdata[i].level[k] = 0;
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
}
