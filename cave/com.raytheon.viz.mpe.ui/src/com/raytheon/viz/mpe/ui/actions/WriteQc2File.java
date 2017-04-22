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
package com.raytheon.viz.mpe.ui.actions;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.mpe.util.DailyQcUtils;

/**
 * this routine writes a second point precip file this file contains the Level2
 * data values which have been changed from their original (Level 1) values
 * values with QC code = 5 (Estimated) are also excluded
 * 
 * input: type = "type" portion of the ts code
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 27, 2011            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class WriteQc2File {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WriteQc2File.class);
    
    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    private BufferedWriter fp = null;

    /**
     * @param type
     */
    public void writeQcFile(String type) {

        AppsDefaults apps_defaults = AppsDefaults.getInstance();
        String dqc_output_qc_flag = apps_defaults
                .getToken("mpe_dqc_output_qc_file");
        if (dqc_output_qc_flag != null && dqc_output_qc_flag.length() > 0) {
            if (dqc_output_qc_flag.equalsIgnoreCase("on")) {
            } else {
                return;
            }
        }

        Date old_time;
        StringBuilder fbuf = new StringBuilder();
        StringBuilder pbuf = new StringBuilder();
        StringBuilder mbuf = new StringBuilder();
        StringBuilder buf = new StringBuilder();
        int max_stations = dqc.precip_stations.size();
        int qcdays = dqc.qcDays;
        Calendar gm = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        try {
            /* define the "type" portion of the ts code */
            for (int j = 0; j < qcdays; j++) {

                old_time = dqc.pdata[j].data_time;

                if (old_time == null) {
                    continue;
                }

                gm.setTime(old_time);
                fbuf.append(String.format("%s%04d%02d%02d",
                        dqc.pcpn_qc_file, gm.get(Calendar.YEAR),
                        gm.get(Calendar.MONTH) + 1,
                        gm.get(Calendar.DAY_OF_MONTH)));
                System.out.println("Writing QC file " + fbuf);

                for (int m = 0; m < max_stations; m++) {

                    if (dqc.pdata[j].stn[m].frain[4].qual != 2
                            && dqc.pdata[j].stn[m].frain[4].qual != 1) {
                        runSixHourCase(j, m, type);
                        continue;
                    }

                    // Open file for writing
                    if (fp == null) {
                        File fo = new File(fbuf.toString());
                        fo.setReadable(true, false);
                        fo.setWritable(true, false);
                        fp = new BufferedWriter(new FileWriter(fo));
                    }
                    gm.setTime(dqc.pdata[j].data_time);
                    pbuf.setLength(0);
                    pbuf.append(String.format("PPD%s%s", type,
                            dqc.precip_stations.get(m).parm.charAt(4)));
                    buf.setLength(0);
                    buf.append(String.format(".AR %s %02d%02d%02d DH12/%s ",
                            dqc.precip_stations.get(m).hb5,
                            gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                            gm.get(Calendar.DAY_OF_MONTH), pbuf));
                    mbuf.setLength(0);
                    if (dqc.pdata[j].stn[m].frain[4].qual != 1) {
                        mbuf.append(String.format("%5.2f",
                                dqc.pdata[j].stn[m].frain[4].data));
                    } else {
                        mbuf.append(String.format("%s", "M"));
                    }

                    if (dqc.pdata[j].stn[m].sflag[4] != 1) {

                        if (dqc.pdata[j].stn[m].frain[4].qual == 0) {
                            mbuf.append("S");
                        } else if (dqc.pdata[j].stn[m].frain[4].qual == 2) {
                            mbuf.append("W");
                        } else if (dqc.pdata[j].stn[m].frain[4].qual == 3) {
                            mbuf.append("Q");
                        } else if (dqc.pdata[j].stn[m].frain[4].qual == 4) {
                            mbuf.append("D");
                        } else if (dqc.pdata[j].stn[m].frain[4].qual == 8) {
                            mbuf.append("V");
                        } else if (dqc.pdata[j].stn[m].frain[4].qual == 5) {
                            mbuf.append("E");
                        } else if (dqc.pdata[j].stn[m].frain[4].qual == 6) {
                            mbuf.append("L");
                        }
                    }

                    // SNOTEL data case

                    else if (dqc.pdata[j].stn[m].sflag[4] == 1) {

                        if (dqc.pdata[j].stn[m].frain[4].qual == 8) {
                            mbuf.append("A");
                        } else if (dqc.pdata[j].stn[m].frain[4].qual == 0) {
                            mbuf.append("B");
                        } else if (dqc.pdata[j].stn[m].frain[4].qual == 3) {
                            mbuf.append("C");
                        }
                    } // end SNOTEL

                    buf.append(mbuf.toString());
                    fp.write(buf.toString());
                    fp.newLine();

                    runSixHourCase(j, m, type);
                } // end of max_stations for loop
                if (fp != null) {
                    fp.flush();
                    fp.close();
                    fp = null;
                }
                if (fbuf.length() > 0) {
                    fbuf.setLength(0);
                }
            } // end of qcDays for loop
        } catch (IOException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } finally {
            try {
                if (fp != null) {
                    fp.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }

        }
    }

    /**
     * 
     */
    private void runSixHourCase(int j, int m, String type) {

        Date old_time;
        StringBuilder fbuf = new StringBuilder();
        StringBuilder pbuf = new StringBuilder();
        StringBuilder mbuf = new StringBuilder();
        StringBuilder buf = new StringBuilder();
        int hh = 0;
        Calendar gm = Calendar.getInstance(TimeZone.getTimeZone("GMT"));

        old_time = dqc.pdata[j].data_time;
        gm.setTime(old_time);
        System.out.println("Writing out 6 hour data for QC. ");
        for (int k = 0; k < 4; k++) {

            if (dqc.pdata[j].stn[m].frain[k].qual != 2
                    && dqc.pdata[j].stn[m].frain[k].qual != 1) {
                continue;
            }
            fbuf.setLength(0);
            if (fp == null) {
                fbuf.append(String.format("%s%04d%02d%02d",
                        dqc.pcpn_qc_file, gm.get(Calendar.YEAR),
                        gm.get(Calendar.MONTH) + 1,
                        gm.get(Calendar.DAY_OF_MONTH)));
                try {
                    File fo = new File(fbuf.toString());
                    fp = new BufferedWriter(new FileWriter(fo));
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }

            if (k == 0) {
                hh = 18;
            } else if (k == 1) {
                hh = 00;
            } else if (k == 2) {
                hh = 06;
            } else {
                hh = 12;
            }

            // we only want the first 6 hour period to have a date one day
            // earlier
            if (k == 0) {
                gm.setTime(old_time);
                gm.add(Calendar.SECOND, -86400);
            }
            pbuf.setLength(0);
            pbuf.append(String.format("PPQ%s%s", type,
                    dqc.precip_stations.get(m).parm.charAt(4)));
            buf.setLength(0);
            buf.append(String.format(".AR %s %02d%02d%02d DH%02d/%s ",
                    dqc.precip_stations.get(m).hb5,
                    gm.get(Calendar.YEAR), gm.get(Calendar.MONTH) + 1,
                    gm.get(Calendar.DAY_OF_MONTH), hh, pbuf));

            /*
             * we only want the first 6 hour period to have a date one day
             * earlier
             */
            /* resume the values for other 6 hour periods */
            if (k == 0) {
                gm.setTime(old_time);
                // gm.add(Calendar.SECOND, +86400);
            }
            mbuf.setLength(0);
            if (dqc.pdata[j].stn[m].frain[k].qual != 1) {
                mbuf.append(String.format("%5.2f",
                        dqc.pdata[j].stn[m].frain[k].data));
            } else {
                mbuf.append(String.format("%s", "M"));
            }

            if (dqc.pdata[j].stn[m].frain[k].qual == 8) {
                mbuf.append("V");
            } else if (dqc.pdata[j].stn[m].frain[k].qual == 2) {
                mbuf.append("W");
            } else if (dqc.pdata[j].stn[m].frain[k].qual == 3) {
                mbuf.append("Q");
            } else if (dqc.pdata[j].stn[m].frain[k].qual == 4) {
                mbuf.append("D");
            } else if (dqc.pdata[j].stn[m].frain[k].qual == 0) {
                mbuf.append("S");
            } else if (dqc.pdata[j].stn[m].frain[k].qual == 5) {
                mbuf.append("E");
            } else if (dqc.pdata[j].stn[m].frain[k].qual == 6) {
                mbuf.append("L");
            }

            buf.append(mbuf.toString());
            try {
                fp.write(buf.toString());
                fp.newLine();
            } catch (IOException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }
}
