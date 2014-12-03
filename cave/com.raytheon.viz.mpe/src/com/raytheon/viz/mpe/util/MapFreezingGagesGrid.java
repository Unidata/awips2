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
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Scanner;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.QCHRAP;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 2, 2009             snaples      Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class MapFreezingGagesGrid {

    private DailyQcUtils dqc = DailyQcUtils.getInstance();
    
    public boolean map_freeze_gages_to_grid(int smonth, int emonth,
            Hrap_Grid hrap_grid, String hrap_zgage_file, String currentQcArea,
            ArrayList<Station> freezing_stations,
            ArrayList<Station> precip_stations, int numZstations,
            int numPstations) {

        int newflag = 0;
        File gagefile = new File(hrap_zgage_file);
        File stationfile = new File(dqc.getStationListPath(currentQcArea));
        float sorted[] = new float[dqc.mpe_dqc_max_precip_neighbors];
        final float conv = .0174f;
        float dist, dist1, dist2;
        int i, k, l, m, h;
        long iir = gagefile.lastModified();
        long mir;
        if (stationfile.length() > 0) {
            mir = stationfile.lastModified();
        } else {
            return false;
        }
        BufferedReader in = null;
        BufferedWriter out = null;
        Coordinate irap = new Coordinate();
        QCHRAP hrap = new QCHRAP();
        Date sf = SimulatedTime.getSystemTime().getTime();
        // for (l = 0; l < 30; l++) {
        // sorted[l] = 9999999;
        // }

        if (iir == 0 || gagefile.length() == 0) {
            if (gagefile.length() == 0) {
                gagefile.delete();
            }
            newflag = 2;
        } else {
            try {

                in = new BufferedReader(new FileReader(hrap_zgage_file));
                String cbuf = "";
                cbuf = in.readLine().trim();
                Scanner s = new Scanner(cbuf);
                long dt = s.nextLong();
                Date ts = new Date();
                ts.setTime(dt * 1000);
                int oxor = s.nextInt();
                int oyor = s.nextInt();
                int omaxx = s.nextInt();
                int omaxy = s.nextInt();

                sf.setTime(mir);

                /*
                 * If the station file time and the time stamp in the hrap grid
                 * file are not equal of if the size of the HRAP forecast area
                 * has changed, then recreate the grid file.
                 */
                if (ts.before(sf) || oxor != hrap_grid.hrap_minx
                        || oyor != hrap_grid.hrap_miny
                        || omaxx != hrap_grid.maxi || omaxy != hrap_grid.maxj) {
                    newflag = 1;
                }

                s.close();
                in.close();
                in = null;
            } catch (FileNotFoundException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                try {
                    if (in != null) {
                        in.close();
                    }
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
        if (newflag != 0) {
            /*
             * The station file is newer than the grid file. Open the grid file
             * for update.
             */
            try {
                out = new BufferedWriter(new FileWriter(hrap_zgage_file));
                String header = sf.getTime() / 1000 + " " + hrap_grid.hrap_minx
                        + " " + hrap_grid.hrap_miny + " " + hrap_grid.maxi
                        + " " + hrap_grid.maxj;
                out.write(header);
                out.newLine();
                irap = new Coordinate();
                Coordinate ci = new Coordinate();

                for (i = 0; i < hrap_grid.maxi; i++) {
                    for (k = 0; k < hrap_grid.maxj; k++) {
                        Arrays.fill(sorted, 9999999);
                        irap.x = hrap_grid.hrap_minx + i;
                        irap.y = hrap_grid.hrap_miny + k;

                        ci = dqc.getHraptoLatLon(irap);
                        hrap.x = (float) ci.x;
                        hrap.y = (float) ci.y;

                        /* Recompute and store the 30 nearest neighbors. */
                        for (m = 0; m < numZstations; m++) {
                            Station zstation = freezing_stations.get(m);
                            dist1 = hrap.y - zstation.lat;
                            dist2 = (float) ((hrap.x - zstation.lon) * Math
                                    .cos((hrap.y + zstation.lat) / 2 * conv));

                            dist = (float) (Math.pow(dist1, 2) + Math.pow(
                                    dist2, 2));

                            for (l = 0; l < 5; l++) {

                                if (dist < sorted[l]) {

                                    for (h = 4; h > l; h--) {
                                        sorted[h] = sorted[h - 1];
                                        hrap_grid.gage[i][k].zindex[h] = hrap_grid.gage[i][k].zindex[h - 1];
                                    }

                                    sorted[l] = dist;

                                    hrap_grid.gage[i][k].zindex[l] = m;

                                    break;

                                }

                            }
                        }
                        StringBuilder sbrec = new StringBuilder();
                        String rec = "";
                        for (l = 0; l < 5; l++) {
                            rec = hrap_grid.gage[i][k].zindex[l] + " ";
                            sbrec.append(rec);
                        }
                        out.write(sbrec.toString());
                        out.newLine();
                    }
                }
                out.close();
                // Runtime sysex = Runtime.getRuntime();
                // sysex.exec("chmod 777 " + hrap_zgage_file);
                File outfile = new File(hrap_zgage_file);
                outfile.setReadable(true, false);
                outfile.setWritable(true, false);
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                try {
                    if (out != null) {
                        out.close();
                    }
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        } else if (newflag == 0) {
            /* The grid file exists. */
            m = 0;
            try {
                in = new BufferedReader(new FileReader(hrap_zgage_file));
                String cbuf = "";
                Scanner s = new Scanner(cbuf);
                in.readLine().trim();

                /* Read the 30 closest neighbors from the grid file. */
                for (i = 0; i < hrap_grid.maxi; i++) {
                    for (k = 0; k < hrap_grid.maxj; k++) {
                        cbuf = in.readLine().trim();
                        s = new Scanner(cbuf);
                        for (int mm = 0; mm < 5; mm++) {
                            hrap_grid.gage[i][k].zindex[mm] = s.nextInt();
                        }
                    }
                }
                s.close();
                in.close();
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                try {
                    if (in != null) {
                        in.close();
                    }
                } catch (FileNotFoundException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
        /*
         * Update the precip station list to contain the nearest freezing level
         * stations.
         */
        Station station = dqc.new Station();

        for (i = 0; i < numPstations; i++) {
            station = precip_stations.get(i);
            station.zindex = new short[5];

            for (l = 0; l < 5; l++) {
                sorted[l] = 9999999;
            }

            for (m = 0; m < numZstations; m++) {
                Station zstation = freezing_stations.get(m);
                if (i == m) {
                    continue;
                }

                dist1 = station.lat - zstation.lat;
                dist2 = station.lon - zstation.lon;
                dist = (float) (Math.pow(dist1, 2) + Math.pow(dist2, 2));

                for (l = 0; l < 5; l++) {
                    if (dist < sorted[l]) {

                        for (h = 4; h > l; h--) {
                            sorted[h] = sorted[h - 1];
                            station.zindex[h] = station.zindex[h - 1];
                        }

                        sorted[l] = dist;
                        station.zindex[l] = (short) m;

                        break;

                    }
                }
            }
            precip_stations.set(i, station);
        }
        return true;
    }
}
