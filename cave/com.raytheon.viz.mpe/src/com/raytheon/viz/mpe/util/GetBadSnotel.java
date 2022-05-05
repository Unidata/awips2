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
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Scanner;
import java.util.TimeZone;

import com.raytheon.viz.mpe.util.DailyQcUtils.Station;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2009            snaples     Initial creation
 * Feb 5, 2015  17101      snaples     Updated max_stations to use size of dqc.precip_stations.
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class GetBadSnotel {

    BufferedReader in = null;
    
    DailyQcUtils dqc = DailyQcUtils.getInstance();

    public void get_bad_snotel(String bad_snow_file,
            ArrayList<Station> precip_stations) {

        int i, j, k;
        long zday;
        short reason;
        String buf = "";
        char[] cbuf = new char[1000];
        String hb5;
        int num_qc_days;

        /* Retrieve the number of days to QC data for. */
        num_qc_days = dqc.qcDays;

        int max_stations = dqc.precip_stations.size();
//        Pdata[] pdata = DailyQcUtils.pdata;

        for (j = 0; j < num_qc_days; j++) {

            for (i = 0; i < max_stations; i++) {

                for (k = 0; k < 5; k++) {
                    dqc.pdata[j].stn[i].snoflag[k] = 0;
                }

            }

        }

        try {
            in = new BufferedReader(new FileReader(bad_snow_file));
            Scanner s = new Scanner(buf);
            while (true) {

                in.read(cbuf, 0, 100);
                buf = new String(cbuf);

                if (buf != null) {
                    break;
                }
                // ier=sscanf(buf,"%s",hb5);
                hb5 = s.next();

                for (i = 0; i < max_stations; i++) {

                    if (hb5.equals(precip_stations.get(i).hb5)) {

                        for (;;) {

                            in.read(cbuf, 0, 100);
                            buf = new String(cbuf);

                            if (buf != null) {
                                break;
                            }

                            if (!(buf.indexOf("END") < 0)) {
                                break;
                            }

                            // ier=sscanf(buf,"%d %d",&zday,&reason);
                            zday = s.nextLong();
                            reason = s.nextShort();
                            SimpleDateFormat sdf = new SimpleDateFormat(
                                    "yyyyMMdd");
                            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

                            for (j = 0; j < num_qc_days; j++) {

                                Date zt = null;
                                try {
                                    zt = sdf.parse(dqc.pdata[j].ztime);
                                } catch (ParseException e) {
                                    // TODO Auto-generated catch block
                                    e.printStackTrace();
                                }
                                if (zt.getTime() == zday) {

                                    for (k = 0; k < 5; k++) {

                                        dqc.pdata[j].stn[i].snoflag[k] = reason;

                                    }

                                }

                            }

                        }

                        break;

                    }

                }
                s.close();
                in.close();
                return;
            }
        } catch (FileNotFoundException e) {
            System.out.println("File not found " + bad_snow_file);
        } catch (IOException e) {
            e.printStackTrace();
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

    }

}
