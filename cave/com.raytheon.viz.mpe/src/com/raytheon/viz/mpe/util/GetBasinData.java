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
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.persistence.Table;

import com.raytheon.uf.common.dataplugin.shef.tables.Linesegs;
import com.raytheon.uf.common.dataplugin.shef.tables.LinesegsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Data;
import com.raytheon.viz.mpe.util.DailyQcUtils.Lcoord;
import com.raytheon.viz.mpe.util.DailyQcUtils.Maps;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 4, 2009            snaples     Initial creation
 * 
 * </pre>
 * 
 * @author snaples
 * @version 1.0
 */

public class GetBasinData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GetBasinData.class);

    private int maxib;

    public boolean get_basin_data(String basin_file, String hrap_file,
            Maps[] mean_areal_precip_global, String[] tag) {
        long start = System.currentTimeMillis();
        System.out.println("Starting to get Basin Data. ");
        int ib, l, numpts, ip, x, y, ip2, ip3, ip4, i;
        int hrap_basin_flag = 1;
        double lat, lon;
        int m;
        int mm;
        int num_points;
        File basin_f = new File(basin_file);
        File hrap_f = new File(hrap_file);
        BufferedReader in = null;
        String ibuf = "";
        ArrayList<Linesegs> pLineSegs = new ArrayList<Linesegs>();
        Linesegs pLineSegNode = new Linesegs();
        DailyQcUtils dc = new DailyQcUtils();

        if (basin_file == null) {
            /* A basin file must be specified. */
            System.out.println("No basin file specified\n");
            return false;
        }

        if (hrap_file == null) {
            System.out.println("No hrap basin file specified\n");
            hrap_basin_flag = 0;
        }
        if (basin_f.lastModified() == 0 || basin_f.length() == 0) {
            if (basin_f.length() == 0) {
                basin_f.delete();
            }
        }
        if (hrap_basin_flag == 1) {
            if (hrap_f.lastModified() == 0 || hrap_f.length() == 0) {
                if (hrap_f.length() == 0) {
                    hrap_f.delete();
                    hrap_basin_flag = 0;
                }
            }
        }
        ib = 0;
        try {
            in = new BufferedReader(new FileReader(basin_f));
            Scanner s = new Scanner(ibuf);
            String regx = "^([0-9A-Z]{3,})\\s*(.*?)\\s+(\\d{1,})";
            Pattern p = Pattern.compile(regx);

            while ((ibuf = in.readLine()) != null) {
                Matcher mt = p.matcher(ibuf);
                if (!mt.find()) {
                    continue;
                }
                mean_areal_precip_global[ib] = dc.new Maps();
                mean_areal_precip_global[ib].hb5 = mt.group(1);
                numpts = Integer.parseInt(mt.group(3).trim());
                mean_areal_precip_global[ib].basin_points = numpts;

                Lcoord[] ppp = new Lcoord[numpts];
                for (int ij = 0; ij < ppp.length; ij++) {
                    ppp[ij] = dc.new Lcoord();
                }

                mean_areal_precip_global[ib].basin = ppp;
                for (l = 0; l < numpts; l++) {
                    ibuf = in.readLine().trim();
                    String[] locs = ibuf.split("\\s+");
                    lat = Double.parseDouble(locs[0].trim());
                    lon = Double.parseDouble(locs[1].trim());

                    /* Ensure that the longitude is negative. */
                    if (lon > 0) {
                        lon = lon * -1;
                    }

                    mean_areal_precip_global[ib].basin[l].lon = (float) lon;
                    mean_areal_precip_global[ib].basin[l].lat = (float) lat;

                }
                ib++;
            }

            // mean_areal_precip_global[ib] = dc.new Maps();
            // mean_areal_precip_global[ib].hb5 = "";
            maxib = ib;
            System.out.println("Size of map array is : " + maxib + " basins");
            s.close();
            in.close();
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block. Please revise as
            // appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return false;
        } catch (IOException e) {
            // TODO Auto-generated catch block. Please revise as
            // appropriate.
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return false;
        } catch (NumberFormatException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
            return false;
        }

        for (ib = 0; ib < maxib; ib++) {
            mean_areal_precip_global[ib].gz = new float[200];
            mean_areal_precip_global[ib].uz = new float[200];
            mean_areal_precip_global[ib].mz = new float[200];
            mean_areal_precip_global[ib].lz = new float[200];
            mean_areal_precip_global[ib].gzc = new float[24];
            mean_areal_precip_global[ib].uzc = new float[24];
            mean_areal_precip_global[ib].mzc = new float[24];
            mean_areal_precip_global[ib].lzc = new float[24];
            mean_areal_precip_global[ib].zgz = new float[200];
            mean_areal_precip_global[ib].zuz = new float[200];
            mean_areal_precip_global[ib].zmz = new float[200];
            mean_areal_precip_global[ib].zlz = new float[200];
            mean_areal_precip_global[ib].tgz = new float[200];
            mean_areal_precip_global[ib].tuz = new float[200];
            mean_areal_precip_global[ib].tmz = new float[200];
            mean_areal_precip_global[ib].tlz = new float[200];
            mean_areal_precip_global[ib].maps_done = new int[200];
            mean_areal_precip_global[ib].tmaps_done = new int[200];
            mean_areal_precip_global[ib].zmaps_done = new int[200];

            // for (m = 0; m < 200; m++) {
            Arrays.fill(mean_areal_precip_global[ib].gz, -1);
            Arrays.fill(mean_areal_precip_global[ib].uz, -1);
            Arrays.fill(mean_areal_precip_global[ib].mz, -1);
            Arrays.fill(mean_areal_precip_global[ib].lz, -1);
            Arrays.fill(mean_areal_precip_global[ib].zgz, -1);
            Arrays.fill(mean_areal_precip_global[ib].zuz, -1);
            Arrays.fill(mean_areal_precip_global[ib].zmz, -1);
            Arrays.fill(mean_areal_precip_global[ib].zlz, -1);
            Arrays.fill(mean_areal_precip_global[ib].tgz, -999);
            Arrays.fill(mean_areal_precip_global[ib].tuz, -999);
            Arrays.fill(mean_areal_precip_global[ib].tmz, -999);
            Arrays.fill(mean_areal_precip_global[ib].tlz, -999);
            Arrays.fill(mean_areal_precip_global[ib].maps_done, -1);
            Arrays.fill(mean_areal_precip_global[ib].tmaps_done, -1);
            Arrays.fill(mean_areal_precip_global[ib].zmaps_done, -1);

            // }
        }
        if (hrap_basin_flag == 1) {
            try {
                System.out.println("Reading basin data from file.");
                long basin_start = System.currentTimeMillis();
                in = new BufferedReader(new FileReader(hrap_f));
                char eb[] = new char[100];
                for (ib = 0; ib < maxib; ib++) {
                    in.read(eb, 0, 80);
                    int p = new String(eb).indexOf('\n');

                    if (p != -1) {
                        p = 0;
                    }

                    String t = new String(eb);
                    Scanner s = new Scanner(t);

                    numpts = s.nextInt();
                    mean_areal_precip_global[ib].hrap_points = numpts;
                    String bchar = "";
                    bchar = s.findInLine("*$");
                    mean_areal_precip_global[ib].bchar = bchar;
                    mean_areal_precip_global[ib].hrap_data = new Hrap_Data[numpts];

                    for (mm = 0; mm < 4; mm++) {
                        mean_areal_precip_global[ib].zones[mm] = -1;
                    }

                    for (l = 0; l < numpts; l++) {

                        for (mm = 0; mm < 4; mm++) {
                            mean_areal_precip_global[ib].hrap_data[l].zone[mm] = -1;
                        }

                        p = in.read(eb, 0, 100);

                        if (p == 0) {
                            break;
                        }

                        t = new String(eb);

                        // ier = sscanf (ibuf, "%d %d %d %d %d %d\n", &x, &y,
                        // &ip, &ip2, &ip3,&ip4);

                        x = s.nextInt();
                        y = s.nextInt();
                        if (s.hasNextInt()) {
                            ip = s.nextInt();

                            if (ip < 0 || ip > 4) {
                                System.out
                                        .println("HRAP error in read_basin_data routine.\n");
                                return false;
                            }
                            ip2 = s.nextInt();
                            ip3 = s.nextInt();
                            ip4 = s.nextInt();

                            mean_areal_precip_global[ib].hrap_data[l].x = x;
                            mean_areal_precip_global[ib].hrap_data[l].y = y;
                            mean_areal_precip_global[ib].hrap_data[l].zone[ip - 1] = 1;
                            mean_areal_precip_global[ib].zones[0] = 1;

                            if (s.hasNextInt()) {
                                if (ip2 < 0 || ip2 > 4) {
                                    System.out
                                            .println("HRAP error in read_basin_data routine.\n");
                                    return false;
                                }

                                mean_areal_precip_global[ib].hrap_data[l].zone[ip2 - 1] = 1;
                                mean_areal_precip_global[ib].zones[ip2 - 1] = 1;

                            }

                            if (s.hasNextInt()) {

                                if (ip3 < 0 || ip3 > 4) {
                                    System.out
                                            .println("HRAP error in read_basin_data routine.\n");
                                    return false;
                                }

                                mean_areal_precip_global[ib].hrap_data[l].zone[ip3 - 1] = 1;
                                mean_areal_precip_global[ib].zones[ip3 - 1] = 1;

                            }

                            if (s.hasNextInt()) {

                                if (ip4 < 0 || ip4 > 4) {
                                    System.out
                                            .println("HRAP error in read_basin_data routine.\n");
                                    return false;
                                }

                                mean_areal_precip_global[ib].hrap_data[l].zone[ip4 - 1] = 1;
                                mean_areal_precip_global[ib].zones[ip4 - 1] = 1;

                            }

                        }
                    }
                    System.out
                            .println("Finished reading basin data from file, elapsed time: "
                                    + (System.currentTimeMillis() - basin_start)
                                    + " ms");
                    s.close();
                    in.close();
                }
            } catch (FileNotFoundException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                return false;
            } catch (IOException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                return false;
            } catch (NumberFormatException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                return false;
            } finally {
                try {
                    if (in != null) {
                        in.close();
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        } else {
            /*
             * Read Linesegs to get the basin definitions. Basins are assumed
             * not to have subareas.
             */
            /*
             * Loop over each basin. Retrieve the lineseg data for it if it is
             * available.
             */
            long linesegs_start = System.currentTimeMillis();
            System.out.println("Starting Line Segs methods. elapsed: "
                    + (linesegs_start - start));
            for (ib = 0; ib < maxib; ib++) {
                /* Build the linesegs where clause. */
                String where_clause = "WHERE area_id = '"
                        + mean_areal_precip_global[ib].hb5.trim()
                        + "' ORDER BY hrap_row ASC, hrap_beg_col ASC ";

                /* Retrieve the linesegs data. */
                pLineSegs = GetLineSegs(where_clause);

                /* Assume a bchar of HZZZ. */
                mean_areal_precip_global[ib].bchar = "HZZZ";

                if (!pLineSegs.isEmpty()) {
                    Iterator<Linesegs> it = pLineSegs.iterator();
                    pLineSegNode = it.next();

                    mean_areal_precip_global[ib].hrap_points = 0;
                    mean_areal_precip_global[ib].hrap_data = new Hrap_Data[0];
                    l = 0;

                    while (it.hasNext()) {
                        num_points = (pLineSegNode.getHrapEndCol() - pLineSegNode
                                .getId().getHrapBegCol()) + 1;
                        mean_areal_precip_global[ib].hrap_points += num_points;
                        if (mean_areal_precip_global[ib].hrap_data.length > 0) {
                            mean_areal_precip_global[ib].hrap_data = (Hrap_Data[]) resizeArray(
                                    mean_areal_precip_global[ib].hrap_data,
                                    mean_areal_precip_global[ib].hrap_points);
                        } else {
                            mean_areal_precip_global[ib].hrap_data = new Hrap_Data[mean_areal_precip_global[ib].hrap_points];
                        }

                        if (mean_areal_precip_global[ib].hrap_data == null) {
                            System.out.println("Could not realloc memory for "
                                    + mean_areal_precip_global[ib].hrap_data
                                    + ".\n");
                            return false;
                        }

                        for (i = pLineSegNode.getId().getHrapBegCol(); i <= pLineSegNode
                                .getHrapEndCol(); ++i) {
                            Hrap_Data d = dc.new Hrap_Data();
                            mean_areal_precip_global[ib].hrap_data[l] = d;
                            mean_areal_precip_global[ib].hrap_data[l].x = i;
                            mean_areal_precip_global[ib].hrap_data[l].y = pLineSegNode
                                    .getId().getHrapRow();
                            mean_areal_precip_global[ib].hrap_data[l].zone[0] = 1;
                            mean_areal_precip_global[ib].hrap_data[l].zone[1] = 0;
                            mean_areal_precip_global[ib].hrap_data[l].zone[2] = 0;
                            mean_areal_precip_global[ib].hrap_data[l].zone[3] = 0;
                            mean_areal_precip_global[ib].zones[0] = 1;
                            mean_areal_precip_global[ib].zones[1] = 0;
                            mean_areal_precip_global[ib].zones[2] = 0;
                            mean_areal_precip_global[ib].zones[3] = 0;

                            ++l;
                        }

                        pLineSegNode = it.next();
                    }

                    pLineSegs = null;

                } else {
                    System.out
                            .println("Could not retrieve LineSeg data for basin "
                                    + mean_areal_precip_global[ib].hb5);
                }

            }
            System.out.println("Finished the Line Segs methods, elapsed time: "
                    + (System.currentTimeMillis() - linesegs_start) + " ms");
        }
        /* initialize basins as always on */
        for (ib = 0; ib < maxib; ib++) {
            mean_areal_precip_global[ib].owner = 9999;
        }
        System.out.println("Finished getting Basin Data, elapsed time: "
                + (System.currentTimeMillis() - start) + " ms");
        DailyQcUtils.setMax_basins(maxib);
        return true;
    }

    public static ArrayList<Linesegs> GetLineSegs(String where) {
        StringBuilder query = new StringBuilder(
                "SELECT area_id, hrap_row, hrap_beg_col, hrap_end_col, area FROM ");
        query.append(Linesegs.class.getAnnotation(Table.class).name());
        query.append(" ");
        query.append(where);

        ArrayList<Linesegs> retVal = new ArrayList<Linesegs>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(
                    query.toString(), "ihfs", QueryLanguage.SQL);

            retVal.ensureCapacity(results.size());
            for (Object[] item : results) {
                Linesegs ln = new Linesegs();
                LinesegsId ld = new LinesegsId();
                ld.setAreaId(item[0].toString());
                ld.setHrapRow(Integer.parseInt(item[1].toString().trim()));
                ld.setHrapBegCol(Integer.parseInt(item[2].toString().trim()));
                ln.setId(ld);
                ln.setHrapEndCol(Integer.parseInt(item[3].toString().trim()));
                ln.setArea(Double.parseDouble(item[4].toString().trim()));

                retVal.add(ln);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return retVal;
    }

    public int get_num_basins() {
        return maxib;
    }

    /**
     * Reallocates an array with a new size, and copies the contents of the old
     * array to the new array.
     * 
     * @param oldArray
     *            the old array, to be reallocated.
     * @param newSize
     *            the new array size.
     * @return A new array with the same contents.
     */
    private static Object resizeArray(Object oldArray, int newSize) {
        int oldSize = java.lang.reflect.Array.getLength(oldArray);
        Class<?> elementType = oldArray.getClass().getComponentType();
        Object newArray = java.lang.reflect.Array.newInstance(elementType,
                newSize);
        int preserveLength = Math.min(oldSize, newSize);
        if (preserveLength > 0) {
            System.arraycopy(oldArray, 0, newArray, 0, preserveLength);
        }
        return newArray;
    }
}
