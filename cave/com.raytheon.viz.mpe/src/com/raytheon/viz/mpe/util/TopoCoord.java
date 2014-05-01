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

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.Scanner;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.mpe.util.MapPrecipGagesGrid.Lcoord;
import com.raytheon.viz.mpe.util.MapPrecipGagesGrid.Topo;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    	Description
 * ------------ ---------- ----------- 	--------------------------
 * Feb 25, 2009            snaples     	Initial creation
 * Apr 16, 2012			   mgamazaychik	DR9602 - made changes how maxi and maxj are 
 *									   	calculated to make the code consistent 
 *										with A1
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class TopoCoord {

    static int file_found = 0;

    static int first = 1;

    final String geo_data_token = "whfs_geodata_dir";

    float max_lat, max_lon, delta_lat, delta_lon, total_lat, total_lon, lat,
            lon;

    int i, j, ier, maxj, maxi, max_value, k, color, tdi;

    AppsDefaults appsDefaults = AppsDefaults.getInstance();

    Topo topo = DailyQcUtils.topo;

    char[] kbuf;

    public Topo get_topo_coord() {

        if (DailyQcUtils.topo == null) {
            DailyQcUtils.topo = new Topo();
        }
        if (first == 1) {
            // first = 0;

            topo = DailyQcUtils.topo;

            max_value = 0;

            /* Open the topograhy file. */
            StringBuffer topofile = new StringBuffer(
                    appsDefaults.getToken(geo_data_token));

            if (topofile.length() == 0) {
                /* Could not retrieve the whfs geo data directory. */
                return null;
            }

            topofile.append("/topography");
            BufferedReader in = null;
            try {
                in = new BufferedReader(new FileReader(topofile.toString()));

                /*
                 * Read the first record of the topography file. It contains the
                 * header information including the latitude/longitude bounds of
                 * the data in the file and the courseness of the data.
                 */
                String[] tokens;
                String line = in.readLine();
                if (line != null) {
                    tokens = line.split("\\s+", 6);
                } else {
                    tokens = null;
                }
                if (tokens != null && tokens.length == 6) {
                    // ier = sscanf (header_buf, "%f %f %f %f %f %f", &max_lat,
                    // &max_lon, &total_lat,
                    // &total_lon, &delta_lat, &delta_lon);
                    delta_lat = (float) (Float.parseFloat(tokens[4]) / 60.);
                    delta_lon = (float) (Float.parseFloat(tokens[5]) / 60.);
                    max_lat = (Float.parseFloat(tokens[0]));
                    max_lon = (Float.parseFloat(tokens[1]));
                    total_lat = (Float.parseFloat(tokens[2]));
                    total_lon = (Float.parseFloat(tokens[3]));

                    /*
                     * Determine the total number of longitude increments (in
                     * minutes) and the total number of latitude increments.
                     */
                    /*
                     * DR9602 - made changes to make the code consistent with A1
                     */
                    maxi = (int) Math.ceil((total_lon / delta_lon));
                    maxj = (int) Math.ceil((total_lat / delta_lat));

                    /*
                     * Assign the latitude and longitude information read from
                     * the topography file into the topography structure.
                     */
                    topo.maxi = maxi;
                    topo.maxj = maxj;
                    topo.max_lat = max_lat;
                    topo.max_lon = max_lon;
                    topo.total_lat = total_lat;
                    topo.total_lon = total_lon;
                    topo.delta_lat = delta_lat;
                    topo.delta_lon = delta_lon;
                }
                in.close();

            } catch (IOException e) {
                // TODO Auto-generated catch block
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
            /* Dynamically create the two dimensional array. */
            /* Maxi corresponds to longitude. */
            topo.coord = new Lcoord[maxi][maxj];
            topo.value = new int[maxi][maxj];
            topo.color = new char[maxi][maxj];

            BufferedInputStream bin = null;
            try {
                bin = new BufferedInputStream(new FileInputStream(
                        topofile.toString()));
                Scanner s = new Scanner(bin);
                s.nextLine();
                for (j = 0; j < maxj; j++) {
                    int tp;
                    k = 0;
                    for (i = 0; i < maxi; i++) {
                        tp = s.nextInt();
                        topo.value[i][j] = (tp);
                        // s.nextInt();
                        // s.nextInt();
                        // s.nextInt();
                        topo.coord[i][j] = new MapPrecipGagesGrid.Lcoord();
                        lat = lon = topo.coord[i][j].lat = max_lat - j
                                * delta_lat;

                        /*
                         * Be sure to negate the longitude. Map library expects
                         * Western Hemisphere longitudes to be negative.
                         */
                        topo.coord[i][j].lon = (max_lon - i * delta_lon);
                    }
                }
                bin.close();
                s.close();
                file_found = 1;
                for (j = 0; j < maxj - 1; j++) {
                    for (i = 0; i < maxi - 1; i++) {
                        /* Determine the color index of this topo cell. */
                        if (topo.value[i][j] > 0) {

                            if (topo.value[i][j] < 80) {
                                color = 3;
                            } else if (topo.value[i][j] < 160) {
                                color = 10;
                            } else if (topo.value[i][j] < 240) {
                                color = 17;
                            } else if (topo.value[i][j] < 320) {
                                color = 24;
                            } else {
                                color = 31;
                            }

                            /*
                             * Determine the gradient from southwest to
                             * northeast. Tdi will be negative for a downhill
                             * gradient. Tdi will be positive for an uphill
                             * gradient.
                             */
                            tdi = -topo.value[i][j + 1] / 3
                                    + topo.value[i + 1][j] / 3;
                            if (tdi > 3) {
                                tdi = 3;
                            }

                            if (tdi < -3) {
                                tdi = -3;
                            }

                            /*
                             * Adjust the color to give the illusion of
                             * illumination from the southwest.
                             */
                            color = color + tdi;

                            topo.color[i][j] = (char) color;
                        } else {
                            topo.color[i][j] = (char) 0;
                        }

                    }
                }

                if (kbuf != null) {
                    kbuf = null;
                }

            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } finally {
                try {
                    if (bin != null) {
                        bin.close();
                    }
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }

        }
        if (file_found == 1) {
            return topo;
        } else {
            return null;
        }
    }
}
