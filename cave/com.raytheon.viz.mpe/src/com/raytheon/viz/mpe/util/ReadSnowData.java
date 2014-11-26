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
 * Nov 26, 2014  16889     snaples     Updated to fix SNOTEL display
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class ReadSnowData {
    
    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    int ier, qual;

    String cbuf = "";

    char p, q;

    String buf = "";

    String hb5 = "";
    
    String rec = "";

    char pc;

    String datbuf = "";

    String parmbuf = "";

    int maxk, startk;

    BufferedReader in = null;

    public int read_snow(String prece, ArrayList<Station> precip_stations,
            int numPstations, int i) {
//        Pdata pdata[] = DailyQcUtils.pdata;

        try {
            int j = 0;

            for (int k = 0; k < numPstations; k++) {
                for (int m = 0; m < 5; m++) {
                    dqc.pdata[i].stn[k].srain[m].data = -99;
                    dqc.pdata[i].stn[k].srain[m].qual = -99;
                    dqc.pdata[i].stn[k].sflag[m] = -1;
                }
            }
            in = new BufferedReader(new FileReader(prece));

//            int p = 1;
//            String sn = cbuf;
            bad: while (in.ready()) {

                cbuf = in.readLine().trim();
                Scanner s = new Scanner(cbuf);

                if (cbuf.length() < 2){
                    continue;
                }
                if (cbuf.charAt(0) == ':') {
                    continue;
                }

                // ier = sscanf (cbuf[2], "%s %s %s", hb5, datbuf, parmbuf);
                if (s.hasNext() == false) {
                    continue;
                }
                rec = s.next();
                hb5 = s.next();
                datbuf = s.next();
                parmbuf = s.next();

                int q = parmbuf.indexOf('/');
                char c = ' ';
                if (q >= 0) {
                    c = parmbuf.charAt(q);
                }
                if (c < 0) {
                    continue;
                }
                char pc = parmbuf.charAt(q + 5);

                for (j = 0; j < numPstations; j++) {
                    if ((precip_stations.get(j).hb5.equals(hb5) && (pc == precip_stations
                            .get(j).parm.charAt(4)))) {
                        break;
                    }
                }
                if (j == numPstations) {
                    continue;
                }
                int u = cbuf.indexOf('/');
                if (u < 0) {
                    continue;
                }

                q = cbuf.indexOf(' ', u);
                if (q < 0) {
                    continue;
                }
                maxk = 5;
                startk = 4;
                for (int kk = startk; kk < maxk; kk++) {

                    dqc.pdata[i].stn[j].srain[kk].qual = 0;

                    int slsh = cbuf.indexOf('/', q);
                    int nln = cbuf.indexOf('\n', q);
                    
                    System.out.println("index of slsh : "+ slsh);
                    System.out.println("index of nln : "+ nln);
                    if ((cbuf.indexOf('/', q) < 0) && (cbuf.isEmpty())){
                        continue bad;
                    }

                    u = 0;

                    buf = cbuf.substring(q).trim();

                    if ((buf.indexOf('.')) < 0) {

                        if ((buf.indexOf('m')) < 0
                                && (buf.indexOf('M')) < 0) {

                            dqc.pdata[i].stn[j].srain[kk].data = -1;
                            dqc.pdata[i].stn[j].srain[kk].qual = -1;
                        }
                    }

                    else {
                        String qbuf = buf.substring(0, buf.indexOf('.')+3);

                        dqc.pdata[i].stn[j].srain[kk].data = Float.parseFloat(qbuf);

                        dqc.pdata[i].stn[j].srain[kk].qual = 8;

                        break;

                    }

                }
                s.close();
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
