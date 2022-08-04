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
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Scanner;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.mpe.core.MPEDataManager;
import com.raytheon.viz.mpe.util.DailyQcUtils.Coord;
import com.raytheon.viz.mpe.util.DailyQcUtils.Gagem;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;
import com.raytheon.viz.mpe.util.DailyQcUtils.QCHRAP;
import com.raytheon.viz.mpe.util.DailyQcUtils.Station;
import com.raytheon.viz.mpe.util.MeanMonthlyPrecip.Isoh;
import com.raytheon.viz.mpe.util.MeanMonthlyTemp.MaxMin;
import org.locationtech.jts.geom.Coordinate;

/**
 * Maps precipitation gages to the grid.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 25, 2009            snaples     Initial creation
 * Oct 03, 2017  6407      bkowal      Eliminated warnings.
 * 
 * </pre>
 * 
 * @author snaples
 */

public class MapPrecipGagesGrid {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    private float sorted[] = new float[DailyQcUtils.mpe_dqc_max_precip_neighbors];

    private float conv = .0174f;

    public Hrap_Grid map_precip_gages_to_grid(int smonth, int emonth,
            String hrap_gage_file, String currentQcArea,
            List<Station> precip_stations, int numPstations) {
        QCHRAP hrap = new QCHRAP();
        dqc.setHrap_grid(new Hrap_Grid());
        int newflag;
        TopoCoord tc = new TopoCoord();
        tc.get_topo_coord();
        MeanMonthlyPrecip mmp = new MeanMonthlyPrecip();
        MeanMonthlyTemp mmt = new MeanMonthlyTemp();
        Isoh isoh = mmp.getIsoh();
        MaxMin maxmin = mmt.getMaxmin();
        if (dqc.topo == null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not retrieve topo information ... Could not create map precipitation gages to HRAP grid. Check if topo file exists.");
            return null;
        }
        /* Initialize the array of precipitation flags here. */
        for (int i = 0; i < 500; i++) {
            DailyQcUtils.pcp_in_use[i] = -1;
        }

        /* Initialize the maximum and minimum values. */
        int maxi = -1;
        int maxj = -1;
        int mini = 999_999;
        int minj = 99_999;

        /* Use the coordinates of the MPE forecast area here. */
        /* need longitude of 4 corners of display */
        /* Get the coordinates of 4 corners of the MPE forecast area. */
        /* These coordinates are from the read_geo_data routine. */
        DailyQcUtils.getHrap_grid().hrap_minx = (int) MPEDataManager
                .getInstance().getHRAPExtent().getMinX();
        DailyQcUtils.getHrap_grid().hrap_miny = (int) MPEDataManager
                .getInstance().getHRAPExtent().getMinY();
        DailyQcUtils.getHrap_grid().maxi = MPEDataManager.getInstance()
                .getHRAPExtent().width;
        DailyQcUtils.getHrap_grid().maxj = MPEDataManager.getInstance()
                .getHRAPExtent().height;

        mini = (int) MPEDataManager.getInstance().getHRAPExtent().getMinX();
        minj = (int) MPEDataManager.getInstance().getHRAPExtent().getMinY();
        maxi = DailyQcUtils.getHrap_grid().maxi;
        maxj = DailyQcUtils.getHrap_grid().maxj;

        DailyQcUtils.pcp.value = new int[maxi][maxj];
        DailyQcUtils.tpf.value = new int[(maxi) + 1000][maxj];
        DailyQcUtils.spf.value = new int[maxi][maxj];
        DailyQcUtils.getHrap_grid().coord = new Coord[maxi][maxj];
        DailyQcUtils.getHrap_grid().isoh = new int[12][maxi][maxj];
        DailyQcUtils.getHrap_grid().gage = new Gagem[maxi][maxj];
        DailyQcUtils.getHrap_grid().max = new int[12][maxi][maxj];
        DailyQcUtils.getHrap_grid().min = new int[12][maxi][maxj];
        DailyQcUtils.getHrap_grid().owner = new int[maxi][maxj];
        DailyQcUtils.getHrap_grid().elev = new int[maxi][maxj];
        newflag = 0;
        File gagefile = new File(hrap_gage_file);
        File stationfile = new File(dqc.getStationListPath(currentQcArea));
        long iir = gagefile.lastModified();
        long mir;
        if (stationfile.length() > 0) {
            mir = stationfile.lastModified();
        } else {
            return null;
        }

        Date sf = SimulatedTime.getSystemTime().getTime();
        if (iir == 0 || gagefile.length() == 0) {
            if (gagefile.length() == 0) {
                gagefile.delete();
            }
            newflag = 2;
        } else {
            try (BufferedReader in = new BufferedReader(
                    new FileReader(hrap_gage_file))) {
                String cbuf = in.readLine();
                try (Scanner s = new Scanner(cbuf)) {
                    long dt = s.nextLong();
                    Date ts = new Date();
                    ts.setTime(dt * 1000);
                    int oxor = s.nextInt();
                    int oyor = s.nextInt();
                    int omaxx = s.nextInt();
                    int omaxy = s.nextInt();
                    sf.setTime(mir);

                    /*
                     * If the station file time and the time stamp in the hrap
                     * grid file are not equal of if the size of the HRAP
                     * forecast area has changed, then recreate the grid file.
                     */
                    if (ts.before(sf) || oxor != mini || oyor != minj
                            || omaxx != maxi || omaxy != maxj) {
                        newflag = 1;
                    }
                }
            } catch (IOException e) {
                statusHandler
                        .error("Failed to read the Precip Neighbors List file: "
                                + hrap_gage_file + ".", e);
                return null;
            }
        }

        // TODO ****** All need this ******
        Coordinate irap = new Coordinate();
        Coordinate ci = new Coordinate();
        for (int i = 0; i < DailyQcUtils.getHrap_grid().maxi; i++) {
            for (int k = 0; k < DailyQcUtils.getHrap_grid().maxj; k++) {
                /* Work in the absolute HRAP Coordinate System. */
                irap.x = (short) (mini + i);
                irap.y = (short) (minj + k);

                ci = dqc.getHraptoLatLon(irap);
                if (ci.x < 0) {
                    ci.x = ci.x * -1;
                }
                hrap.x = (float) ci.x;
                hrap.y = (float) ci.y;

                DailyQcUtils.getHrap_grid().gage[i][k] = new Gagem();
                DailyQcUtils.getHrap_grid().coord[i][k] = new Coord(hrap.y,
                        hrap.x);

                /*
                 * Initialize the elevation associated this HRAP grid cell.
                 */
                int iy = (int) ((dqc.topo.max_lat - hrap.y)
                        / dqc.topo.delta_lat);
                int ix = (int) ((dqc.topo.max_lon - hrap.x)
                        / dqc.topo.delta_lon);

                if (dqc.topo == null || ix <= 0 || iy <= 0
                        || ix >= dqc.topo.maxi - 1 || iy >= dqc.topo.maxj - 1) {
                    DailyQcUtils.getHrap_grid().elev[i][k] = -1;
                } else {
                    float distance = 0.0f;
                    float value = 0.0f;

                    for (int kk = ix; kk <= ix + 1; kk++) {
                        for (int jj = iy; jj <= iy + 1; jj++) {
                            float lat = dqc.topo.max_lat
                                    - jj * dqc.topo.delta_lat;
                            float lon = dqc.topo.max_lon
                                    - kk * dqc.topo.delta_lon;
                            float dist2 = (float) ((hrap.x - lon)
                                    * Math.cos((lat + hrap.y) / 2 * conv));
                            float dist1 = hrap.y - lat;
                            float dist = (float) (Math.pow(dist1, 2)
                                    + Math.pow(dist2, 2));

                            if (dist < 0.000001f) {
                                dist = 0.000001f;
                            }

                            dist = 1 / dist;

                            value = value + dist * dqc.topo.value[kk][jj];
                            distance = distance + dist;
                        }
                    }

                    float elevation = (value * 32.1f) / distance;
                    DailyQcUtils.getHrap_grid().elev[i][k] = (int) elevation;

                }

                /* Retrieve seasonal isohyet for this point. */
                // TODO ******** end all need this *********

                // TODO All again
                /*
                 * For this HRAP grid cell, load the monthly mean precipitation
                 * and max/min temperature information.
                 */
                for (int h = 0; h < 12; ++h) {
                    int ier = dqc.is_good(h, smonth, emonth);

                    if (ier == -1) {
                        continue;
                    }

                    DailyQcUtils.getHrap_grid().isoh[h][i][k] = -1;
                    DailyQcUtils.getHrap_grid().max[h][i][k] = -9999;
                    DailyQcUtils.getHrap_grid().min[h][i][k] = -9999;

                    if (dqc.isohyets_used) {
                        /*
                         * The precipitation PRISM data have been read in.
                         */
                        /*
                         * Much of this code will go away. The PRISM data are
                         * already in HRAP format.
                         */
                        DailyQcUtils
                                .getHrap_grid().isoh[h][i][k] = isoh.value[h][k][i];
                    }

                    if (dqc.maxmin_used) {
                        /*
                         * The max/min temperature PRISM data are available.
                         */
                        DailyQcUtils
                                .getHrap_grid().max[h][i][k] = maxmin.maxvalue[h][k][i];
                        DailyQcUtils
                                .getHrap_grid().min[h][i][k] = maxmin.minvalue[h][k][i];

                    }
                }
            } // End all again
        }
        /*
         * The station file is newer than the grid file. Open the grid file for
         * update.
         */
        if (newflag != 0) {
            statusHandler.info("Creating a new Precip Neighbors List file: "
                    + hrap_gage_file + " ...");
            /*
             * The hrap gage file either does not exist or it is old.
             */
            try (BufferedWriter out = new BufferedWriter(
                    new FileWriter(hrap_gage_file))) {
                String header = sf.getTime() / 1000 + " " + mini + " " + minj
                        + " " + maxi + " " + maxj;
                out.write(header);
                out.newLine();
                for (int i = 0; i < maxi; i++) {
                    for (int k = 0; k < maxj; k++) {
                        /* Work in the absolute HRAP Coordinate System. */
                        irap.x = (short) (mini + i);
                        irap.y = (short) (minj + k);

                        ci = dqc.getHraptoLatLon(irap);
                        hrap.x = (float) ci.x;
                        hrap.y = (float) ci.y;
                        Arrays.fill(sorted, 9999999f);

                        for (int m = 0; m < numPstations; m++) {
                            /*
                             * Loop over the stations. Find the closest
                             * neighbors.
                             */
                            Station pstation = precip_stations.get(m);
                            float dist1 = hrap.y - pstation.lat;
                            float dist2 = (float) Math
                                    .abs(((hrap.x - pstation.lon)
                                            * Math.cos((hrap.y + pstation.lat)
                                                    / 2 * conv)));
                            float dist = (float) Math.abs(
                                    (Math.pow(dist1, 2) + Math.pow(dist2, 2)));

                            for (int l = 0; l < DailyQcUtils.mpe_dqc_max_precip_neighbors; l++) {
                                if (dist < sorted[l]) {
                                    for (int h = DailyQcUtils.mpe_dqc_max_precip_neighbors
                                            - 1; h > l; h--) {
                                        sorted[h] = sorted[h - 1];
                                        DailyQcUtils
                                                .getHrap_grid().gage[i][k].index[h] = DailyQcUtils
                                                        .getHrap_grid().gage[i][k].index[h
                                                                - 1];
                                    }

                                    sorted[l] = dist;
                                    DailyQcUtils
                                            .getHrap_grid().gage[i][k].index[l] = m;

                                    break;
                                }
                            }
                        }

                        /*
                         * For this station write the closest neighbors to the
                         * grid file.
                         */
                        StringBuilder gagerec = new StringBuilder();
                        for (int l = 0; l < DailyQcUtils.mpe_dqc_max_precip_neighbors; l++) {
                            String gages = DailyQcUtils
                                    .getHrap_grid().gage[i][k].index[l] + " ";
                            gagerec.append(gages);
                        }
                        out.write(gagerec.toString());
                        out.newLine();
                    }
                }
            } catch (IOException e) {
                statusHandler
                        .error("Failed to write the Precip Neighbors List file: "
                                + hrap_gage_file + ".", e);
                return null;
            }
            statusHandler
                    .info("Successfully created the new Precip Neighbors List file: "
                            + hrap_gage_file + ".");
        } // end newflag != 0
        else if (newflag == 0) {
            /* The grid file exists. */
            try (BufferedReader in = new BufferedReader(
                    new FileReader(hrap_gage_file))) {
                // Skip the first line.
                in.readLine();
                for (int i = 0; i < DailyQcUtils.getHrap_grid().maxi; i++) {
                    for (int k = 0; k < DailyQcUtils.getHrap_grid().maxj; k++) {
                        String cbuf = in.readLine();
                        try (Scanner s = new Scanner(cbuf)) {
                            for (int mm = 0; mm < DailyQcUtils.mpe_dqc_max_precip_neighbors; mm++) {
                                /*
                                 * Initialize the list of closest stations
                                 * index.
                                 */
                                int lsl = s.nextInt();
                                DailyQcUtils
                                        .getHrap_grid().gage[i][k].index[mm] = lsl;
                            }
                        }
                    }
                }
            } catch (IOException e) {
                statusHandler
                        .error("Failed to read the Precip Neighbors List file: "
                                + hrap_gage_file + ".", e);
                return null;
            }
        }

        return DailyQcUtils.getHrap_grid();
    }

    public static class Lcoord {
        /* x */
        public float lon;

        /* y */
        public float lat;
    }

    public static class Topo {
        public Lcoord coord[][];

        public int value[][];

        public int maxi;

        public int maxj;

        public float max_lat;

        public float max_lon;

        public float total_lat;

        public float total_lon;

        public float delta_lat;

        public float delta_lon;

        public char color[][];
    }
}