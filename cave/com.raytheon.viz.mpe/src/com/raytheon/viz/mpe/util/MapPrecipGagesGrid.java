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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Scanner;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.util.DailyQcUtils.Coord;
import com.raytheon.viz.mpe.util.DailyQcUtils.Dval;
import com.raytheon.viz.mpe.util.DailyQcUtils.Gagem;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.Pcp;
import com.raytheon.viz.mpe.util.DailyQcUtils.QCHRAP;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.MeanMonthlyPrecip.Isoh;
import com.raytheon.viz.mpe.util.MeanMonthlyTemp.MaxMin;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 25, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class MapPrecipGagesGrid {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MapPrecipGagesGrid.class);

    int maxmin_used;

    int isohyets_used;

    int i, k, kk, mini, minj, maxi, maxj, l, m, h;

    QCHRAP hrap;

    Coordinate irap;

    Dval dval;

    Isoh isoh;

    MaxMin maxmin;

    Pcp pcp = DailyQcUtils.pcp;

    Pcp spf = DailyQcUtils.spf;

    Pcp tpf = DailyQcUtils.tpf;

    // Hrap_Grid hrap_grid = new Hrap_Grid();

    long r, s;

    float sorted[] = new float[DailyQcUtils.mpe_dqc_max_precip_neighbors];

    float dist, dist1, dist2, distance, value;

    AppsDefaults appsDefaults = AppsDefaults.getInstance();

    SimpleDateFormat sdf = new SimpleDateFormat();

    private float lat;

    private float lon;

    private float conv = .0174f;

    private float elevation;

    Topo topo = DailyQcUtils.topo;

    public Hrap_Grid map_precip_gages_to_grid(int smonth, int emonth,
            String hrap_gage_file, String currentQcArea,
            ArrayList<Station> precip_stations, int numPstations) {

        topo = null;
        hrap = null;
        hrap = new QCHRAP();
        // topo = new Topo();
        DailyQcUtils.Hrap_Grid hrap_grid = new Hrap_Grid();
        int newflag;
        TopoCoord tc = new TopoCoord();
        tc.get_topo_coord();
        MeanMonthlyPrecip mmp = new MeanMonthlyPrecip();
        MeanMonthlyTemp mmt = new MeanMonthlyTemp();
        isoh = mmp.getIsoh();
        maxmin = mmt.getMaxmin();
        if (DailyQcUtils.topo == null) {
            statusHandler
                    .handle(Priority.PROBLEM,
							"Could not retrieve topo information ... Could not create map precipitation gages to HRAP grid. Check if topo file exists.");
            return null;
        }
        topo = DailyQcUtils.topo;
        /* Initialize the array of precipitation flags here. */
        for (i = 0; i < 500; i++) {
            DailyQcUtils.pcp_in_use[i] = -1;
        }

        /* Initialize the maximum and minimum values. */
        maxi = -1;
        maxj = -1;
        mini = 999999;
        minj = 99999;

        /* Use the coordinates of the MPE forecast area here. */
        /* need longitude of 4 corners of display */
        /* Get the coordinates of 4 corners of the MPE forecast area. */
        /* These coordinates are from the read_geo_data routine. */
        hrap_grid.hrap_minx = (int) MPEDataManager.getInstance()
                .getHRAPExtent().getMinX();
        hrap_grid.hrap_miny = (int) MPEDataManager.getInstance()
                .getHRAPExtent().getMinY();
        // hrap_grid.maxi = (int) MPEDataManager.getInstance().getHRAPExtent()
        // .getMaxX();
        hrap_grid.maxi = MPEDataManager.getInstance().getHRAPExtent().width;
        hrap_grid.maxj = MPEDataManager.getInstance().getHRAPExtent().height;

        mini = (int) MPEDataManager.getInstance().getHRAPExtent().getMinX();
        minj = (int) MPEDataManager.getInstance().getHRAPExtent().getMinY();
        maxi = hrap_grid.maxi;
        maxj = hrap_grid.maxj;

        pcp.value = new int[maxi][maxj];
        tpf.value = new int[(maxi) + 1000][maxj];
        spf.value = new int[maxi][maxj];
        hrap_grid.coord = new Coord[maxi][maxj];
        hrap_grid.isoh = new int[12][maxi][maxj];
        hrap_grid.gage = new Gagem[maxi][maxj];
        hrap_grid.max = new int[12][maxi][maxj];
        hrap_grid.min = new int[12][maxi][maxj];
        hrap_grid.owner = new int[maxi][maxj];
        hrap_grid.elev = new int[maxi][maxj];
        newflag = 0;
        File gagefile = new File(hrap_gage_file);
        DailyQcUtils du = new DailyQcUtils();
        File stationfile = new File(du.getStationListPath(currentQcArea));
        long iir = gagefile.lastModified();
        long mir;
        if (stationfile.length() > 0) {
            mir = stationfile.lastModified();
        } else {
            return null;
        }

        BufferedReader in = null;
        BufferedWriter out = null;
        Date sf = SimulatedTime.getSystemTime().getTime();

        if (iir == 0 || gagefile.length() == 0) {
            if (gagefile.length() == 0) {
                gagefile.delete();
            }
            newflag = 2;
        } else {
            try {

                in = new BufferedReader(new FileReader(hrap_gage_file));
                String cbuf = "";
                cbuf = in.readLine();
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
                if (ts.before(sf) || oxor != mini || oyor != minj
                        || omaxx != maxi || omaxy != maxj) {
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
        try {
            // TODO ****** All need this ******
            irap = new Coordinate();
            Coordinate ci = new Coordinate();
            for (i = 0; i < hrap_grid.maxi; i++) {
                for (k = 0; k < hrap_grid.maxj; k++) {
                    /* Work in the absolute HRAP Coordinate System. */
                    irap.x = (short) (mini + i);
                    irap.y = (short) (minj + k);

                    ci = DailyQcUtils.getHraptoLatLon(irap);
                    if (ci.x < 0) {
                        ci.x = ci.x * -1;
                    }
                    hrap.x = (float) ci.x;
                    hrap.y = (float) ci.y;

                    hrap_grid.gage[i][k] = new Gagem();
                    hrap_grid.coord[i][k] = new Coord(i, k, hrap.y, hrap.x);

                    /*
                     * Initialize the elevation associated this HRAP grid cell.
                     */
                    int iy = (int) ((topo.max_lat - hrap.y) / topo.delta_lat);
                    int ix = (int) ((topo.max_lon - hrap.x) / topo.delta_lon);
                    // if (topo.max_lon < 0 && hrap.x < 0) {
                    // ix = ix * -1;
                    // }

                    if (topo == null || ix <= 0 || iy <= 0
                            || ix >= topo.maxi - 1 || iy >= topo.maxj - 1) {
                        hrap_grid.elev[i][k] = -1;
                    } else {
                        distance = 0.0f;
                        value = 0.0f;

                        for (kk = ix; kk <= ix + 1; kk++) {
                            for (int jj = iy; jj <= iy + 1; jj++) {

                                lat = topo.max_lat - jj * topo.delta_lat;
                                lon = topo.max_lon - kk * topo.delta_lon;
                                dist2 = (float) ((hrap.x - lon) * Math
                                        .cos((lat + hrap.y) / 2 * conv));
                                dist1 = hrap.y - lat;
                                dist = (float) (Math.pow(dist1, 2) + Math.pow(
                                        dist2, 2));

                                if (dist < 0.000001f) {
                                    dist = 0.000001f;
                                }

                                dist = 1 / dist;

                                value = value + dist * topo.value[kk][jj];
                                distance = distance + dist;

                            }
                        }

                        elevation = (value * 32.1f) / distance;
                        hrap_grid.elev[i][k] = (int) elevation;

                    }

                    /* Retrieve seasonal isohyet for this point. */
                    // Arrays.fill(sorted, 9999999f);
                    // TODO ******** end all need this *********

                    // TODO All again
                    /*
                     * For this HRAP grid cell, load the monthly mean
                     * precipitation and max/min temperature information.
                     */
                    for (h = 0; h < 12; ++h) {
                        int ier = du.is_good(h, smonth, emonth);

                        if (ier == -1) {
                            continue;
                        }

                        hrap_grid.isoh[h][i][k] = -1;
                        hrap_grid.max[h][i][k] = -9999;
                        hrap_grid.min[h][i][k] = -9999;

                        if (DailyQcUtils.isohyets_used != -1) {
                            /*
                             * The precipitation PRISM data have been read in.
                             */
                            /*
                             * Much of this code will go away. The PRISM data
                             * are already in HRAP format.
                             */
                            hrap_grid.isoh[h][i][k] = isoh.value[h][k][i];
                        }

                        if (DailyQcUtils.maxmin_used != -1) {
                            /*
                             * The max/min temperature PRISM data are available.
                             */
                            hrap_grid.max[h][i][k] = maxmin.maxvalue[h][k][i];
                            hrap_grid.min[h][i][k] = maxmin.minvalue[h][k][i];

                        }
                    }
                }// End all again
            }
            /*
             * The station file is newer than the grid file. Open the grid file
             * for update.
             */
            if (newflag != 0) {
                System.out.println("DQC: Creating new precip neighbors list. ");
                // newflag != 0
                /*
                 * The hrap gage file either does not exist or it is old.
                 */
                out = new BufferedWriter(new FileWriter(hrap_gage_file));
                String header = sf.getTime() / 1000 + " " + mini + " " + minj
                        + " " + maxi + " " + maxj;
                out.write(header);
                out.newLine();
                for (i = 0; i < maxi; i++) {
                    for (k = 0; k < maxj; k++) {
                        /* Work in the absolute HRAP Coordinate System. */
                        irap.x = (short) (mini + i);
                        irap.y = (short) (minj + k);

                        ci = DailyQcUtils.getHraptoLatLon(irap);
                        hrap.x = (float) ci.x;
                        hrap.y = (float) ci.y;
                        Arrays.fill(sorted, 9999999f);

                        for (m = 0; m < numPstations; m++) {

                            /*
                             * Loop over the stations. Find the closest
                             * neighbors.
                             */
                            Station pstation = precip_stations.get(m);
                            dist1 = hrap.y - pstation.lat;
                            dist2 = (float) Math
                                    .abs(((hrap.x - pstation.lon) * Math
                                            .cos((hrap.y + pstation.lat) / 2
                                                    * conv)));
                            dist = (float) Math.abs((Math.pow(dist1, 2) + Math
                                    .pow(dist2, 2)));

                            for (l = 0; l < DailyQcUtils.mpe_dqc_max_precip_neighbors; l++) {
                                if (dist < sorted[l]) {
                                    for (h = DailyQcUtils.mpe_dqc_max_precip_neighbors - 1; h > l; h--) {
                                        sorted[h] = sorted[h - 1];
                                        hrap_grid.gage[i][k].index[h] = hrap_grid.gage[i][k].index[h - 1];
                                    }

                                    sorted[l] = dist;
                                    hrap_grid.gage[i][k].index[l] = m;

                                    break;

                                }

                            }

                        }

                        /*
                         * For this station write the closest neighbors to the
                         * grid file.
                         */
                        StringBuilder gagerec = new StringBuilder();
                        String gages = "";
                        for (l = 0; l < DailyQcUtils.mpe_dqc_max_precip_neighbors; l++) {
                            gages = hrap_grid.gage[i][k].index[l] + " ";
                            gagerec.append(gages);
                        }
                        out.write(gagerec.toString());
                        out.newLine();
                    }
                }
                System.out
                        .println("DQC: Done creating new precip neighbors list. ");
            } // end newflag != 0
            else if (newflag == 0) {
                /* The grid file exists. */
                m = 0;
                in = new BufferedReader(new FileReader(hrap_gage_file));
                String cbuf = "";
                Scanner s = new Scanner(cbuf);
                in.readLine();
                for (i = 0; i < hrap_grid.maxi; i++) {
                    for (k = 0; k < hrap_grid.maxj; k++) {
                        cbuf = in.readLine();
                        s = new Scanner(cbuf);
                        for (int mm = 0; mm < DailyQcUtils.mpe_dqc_max_precip_neighbors; mm++) {
                            /*
                             * Initialize the list of closest stations index.
                             */
                            int lsl = s.nextInt();
                            hrap_grid.gage[i][k].index[mm] = lsl;
                        }
                    }
                }

            }

            if (out != null) {
                out.flush();
                out.close();
                File outfile = new File(hrap_gage_file);
                outfile.setReadable(true, false);
                outfile.setWritable(true, false);
            }
            if (in != null) {
                in.close();
            }

            return hrap_grid;
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } finally {
            try {
                if (out != null) {
                    out.close();
                }
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        return hrap_grid;
    }

    public static class Lcoord {
        float lon; /* x */

        float lat; /* y */
    }

    public static class Topo {
        Lcoord coord[][];

        int value[][];

        int maxi;

        int maxj;

        float max_lat;

        float max_lon;

        float total_lat;

        float total_lon;

        float delta_lat;

        float delta_lon;

        char color[][];
    }

}
