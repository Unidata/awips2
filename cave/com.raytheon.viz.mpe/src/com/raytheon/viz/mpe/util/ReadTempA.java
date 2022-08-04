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
 * Mar 10, 2016  19625     snaples     Moved array initialization outside of try.
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class ReadTempA {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ReadTempA.class);

    BufferedReader in = null;

    public int read_t_a(String tpointa, Date time, int i, Tdata[] tdata,
            ArrayList<Station> temperature_stations, int numTstations) {

        int j;
        int k;
        int m;
        int qual;
        int offset = 0;
        int kread = 0;
        String kbuf = "";
        String buf = "";
        String hb5 = "";
        int number_found[] = new int[6];
        char pc;
        String parmbuf = "";
        int maxk = 0, startk = 0;
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
        String dt = sdf.format(time);
        /* Store the time in ticks of the data. */
        tdata[i].data_time = time;
        /* Store the GMT time as a string of format YYYYMMDD. */
        tdata[i].ztime = dt;
        
        for (j = 0; j < 6; j++) {
            number_found[j] = 0;
            tdata[i].used[j] = 1;
            tdata[i].level[j] = 1;
        }
        for (k = 0; k < numTstations; k++) {
            for (m = 0; m < 6; m++) {
                tdata[i].tstn[k].tlevel1[m].data = -99;
                tdata[i].tstn[k].tlevel1[m].qual = -99;
            }
        }

        try {
            in = new BufferedReader(new FileReader(tpointa));
            int p = 1;
            int qq = 0;
            String q = "";
            bad: while (in.ready()) {
                kbuf = in.readLine().trim();
                Scanner s = new Scanner(kbuf);

                if (kbuf.charAt(0) == ':') {
                    continue;
                }

                /* Read the handbook5 identifier, the date and the data. */
                // ier=sscanf(kbuf[2],"%s %s %s",hb5,datbuf,parmbuf);
                s.next();
                hb5 = s.next();
                s.next();
                parmbuf = s.next();

                if (hb5 == null) {
                    /* Error reading record. */
                    continue;
                }
                int e = parmbuf.indexOf('/');
                if (e < 0) {
                    continue;
                }
                if (parmbuf.charAt(e + 6) == 'X') {
                    offset = 7;
                    kread = 1;
                    startk = 4;
                    maxk = 5;
                } else if (parmbuf.charAt(e + 6) == 'N') {
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
                     * Try to find a matching station id and source in the
                     * stations file.
                     */
                    if ((hb5.equals(temperature_stations.get(j).hb5))
                            && (pc == temperature_stations.get(j).parm
                                    .charAt(4))) {
                        break;
                    }
                }

                if (j == numTstations) {
                    /*
                     * A matching station id and source could not be found. Read
                     * the next record.
                     */
                    continue;
                }
                q = kbuf;
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
                for (k = startk; k < maxk; k++) {

                    tdata[i].tstn[j].tlevel1[k].qual = 0;

                    Scanner a = new Scanner(q);
                    if (((q.indexOf('/', qq) < 0) && ((q.isEmpty())))) {
                        continue bad;
                    }

                    a.close();
                    q = q.substring(offset);
                    buf = q;
                    a = new Scanner(buf);
                    p = 0;
                    int pp = 0;
                    /*
                     * Can't find a floating point value. Check if the value is
                     * report missing.
                     */
                    if (a.hasNext("m") || a.hasNext("M")) {
                        qq = q.indexOf('m');
                        /* The value is reported missing. */
                        tdata[i].tstn[j].tlevel1[k].data = -99;
                        tdata[i].tstn[j].tlevel1[k].qual = -99;

                    } else {
                        buf = buf.trim();
                        number_found[k]++;
                        String val;
                        char r;
                        p = 0;
                        pp = buf.indexOf('/', p);
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
                        tdata[i].tstn[j].tlevel1[k].data = Float.parseFloat(val
                                .trim());
                        qual = 8;

                        while (r != 0) {

                            if (Character.isLetter(r)) {

                                if (r == 'S' || r == 'A') {
                                    qual = 8;
                                }

                                else if (r == 'F') {
                                    qual = 1;
                                }

                                else if (r == 'V') {
                                    qual = 8;
                                }

                                else if (r == 'E') {
                                    qual = 5;
                                }

                                tdata[i].tstn[j].tlevel1[k].qual = (short) qual;
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
            for (j = 0; j < 6; j++) {
                if (number_found[j] == 0) {
                    tdata[i].used[j] = 0;
                }
            }
            return 1;
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            System.out.println("File not found " + tpointa);
            ;
            for (k = 0; k < 6; k++) {
                tdata[i].used[k] = 0;
                tdata[i].level[k] = 0;
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
