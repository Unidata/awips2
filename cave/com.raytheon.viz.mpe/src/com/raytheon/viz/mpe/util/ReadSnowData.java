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
import java.util.ArrayList;
import java.util.Scanner;

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
 * Mar 5, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class ReadSnowData {

    int j, k, ier, m, qual;

    char cbuf[] = new char[100];

    char p, q;

    char buf[] = new char[100];

    String hb5 = "";

    char pc;

    char datbuf[] = new char[50];

    char parmbuf[] = new char[50];

    int maxk, startk;

    BufferedReader in = null;

    public int read_snow(String prece, ArrayList<Station> precip_stations,
            int numPstations, int i) {
        Pdata pdata[] = DailyQcUtils.pdata;

        try {

            for (k = 0; k < numPstations; k++) {
                for (int m = 0; m < 5; m++) {
                    pdata[i].stn[k].srain[m].data = -99;
                    pdata[i].stn[k].srain[m].qual = -99;
                    pdata[i].stn[k].sflag[m] = -1;
                }
            }
            in = new BufferedReader(new FileReader(prece));

            int p = 1;
            String sn = cbuf.toString();
            Scanner s = new Scanner(sn);
            bad: while (p != -1) {

                in.read(cbuf, 0, 100);

                if (cbuf[0] == ':') {
                    continue;
                }

                // ier = sscanf (cbuf[2], "%s %s %s", hb5, datbuf, parmbuf);
                if (s.hasNext() == false) {
                    continue;
                }
                hb5 = s.next();
                datbuf = s.next().toCharArray();
                parmbuf = s.next().toCharArray();

                int q = parmbuf.toString().indexOf('/');
                char c = ' ';
                if (q >= 0) {
                    c = parmbuf[q];
                }
                if (c < 0) {
                    continue;
                }
                char pc = parmbuf[q + 5];

                for (j = 0; j < numPstations; j++) {
                    if ((precip_stations.get(j).hb5.equals(hb5) && (pc == precip_stations
                            .get(j).parm.charAt(4)))) {
                        break;
                    }
                }
                if (j == numPstations) {
                    continue;
                }
                int u = cbuf.toString().indexOf('/');
                if (u < 0) {
                    continue;
                }

                q = cbuf.toString().indexOf(' ', u);
                if (q < 0) {
                    continue;
                }
                maxk = 5;
                startk = 4;
                for (k = startk; k < maxk; k++) {

                    pdata[i].stn[j].srain[k].qual = 0;

                    if ((cbuf.toString().indexOf('/', q)) < 0
                            && (cbuf.toString().indexOf('\n', q)) < 0) {
                        continue bad;
                    }

                    u = 0;

                    buf = cbuf.toString().substring(q).toCharArray();

                    if ((buf.toString().indexOf('.')) < 0) {

                        if ((buf.toString().indexOf('m')) < 0
                                && (buf.toString().indexOf('M')) < 0) {

                            pdata[i].stn[j].srain[k].data = -1;
                            pdata[i].stn[j].srain[k].qual = -1;
                        }
                    }

                    else {

                        pdata[i].stn[j].srain[k].data = Float.parseFloat(buf
                                .toString());

                        qual = 8;

                        break;

                    }

                }

            }

            in.close();
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            System.out.println("File not found " + prece);
            return -1;
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
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

        // TODO Auto-generated method stub
        return 1;
    }
}
