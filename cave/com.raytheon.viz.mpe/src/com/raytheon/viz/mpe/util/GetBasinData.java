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
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;
import java.util.Set;
import java.util.regex.Pattern;

import javax.persistence.Table;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKBReader;

import com.raytheon.uf.common.dataplugin.shef.tables.Linesegs;
import com.raytheon.uf.common.dataplugin.shef.tables.LinesegsId;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
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
 * 
 * Date          Ticket#  Engineer    De  cription
 * ------------- -------- --------------- --------------------------------------
 * Mar 04, 2009           snaples         Initial creation
 * Aug 12, 2013  16490    snaples         Fixed mapping of hrap grid to basins
 *                                        in get_basin_data
 * Mar 11, 2020  19533    mgaazaychikov   Added reading basin data directly from
 *                                        maps database
 * Apr 14, 2020   7923    randerso        Code cleanup
 * 
 * </pre>
 * 
 * @author snaples
 */

public class GetBasinData {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GetBasinData.class);

    private static final String SSHPBASINS_TABLE = "mapdata.sshpbasins";

    private static final String AREAID_TABLE_NAME = "\"areaId\"";

    private static final String GEOM_TABLE_NAME = "the_geom";

    private static HashMap<String, Geometry> basinData = new HashMap<>();

    private int maxib;

    private DailyQcUtils dqc = DailyQcUtils.getInstance();

    public boolean getBasinData(String hrap_file,
            Maps[] mean_areal_precip_global, String[] tag) {
        long start = System.currentTimeMillis();
        statusHandler.debug("Starting to get Basin Data. ");
        int ib, l, numpts, ip, x, y, ip2, ip3, ip4, i;
        int hrap_basin_flag = 1;
        double lat, lon;
        int mm;
        int num_points;
        File hrap_f = new File(hrap_file);
        List<Linesegs> pLineSegs = new ArrayList<>();
        Linesegs pLineSegNode = new Linesegs();

        if (hrap_file == null) {
            statusHandler.info("No hrap basin file specified");
            hrap_basin_flag = 0;
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
        if (basinData.isEmpty()) {
            readSSHPBasinData();
            if (basinData.isEmpty()) {
                statusHandler.error(
                        "Could not read basins from mapdata.sshpbasins database table");
                return false;
            }
        }

        Set<String> basinIDs = basinData.keySet();
        Iterator<String> basinIDsIterator = basinIDs.iterator();
        while (basinIDsIterator.hasNext()) {
            String basinID = basinIDsIterator.next();
            Coordinate[] basinCoords = basinData.get(basinID).getCoordinates();
            numpts = basinCoords.length;

            mean_areal_precip_global[ib] = dqc.new Maps();
            mean_areal_precip_global[ib].hb5 = basinID;
            mean_areal_precip_global[ib].basin_points = numpts;

            Lcoord[] ppp = new Lcoord[numpts];
            for (int ij = 0; ij < ppp.length; ij++) {
                ppp[ij] = dqc.new Lcoord();
            }

            mean_areal_precip_global[ib].basin = ppp;
            for (l = 0; l < numpts; l++) {
                lat = basinCoords[l].y;
                lon = basinCoords[l].x;

                /* Ensure that the longitude is negative. */
                if (lon > 0) {
                    lon = lon * -1;
                }

                mean_areal_precip_global[ib].basin[l].lon = (float) lon;
                mean_areal_precip_global[ib].basin[l].lat = (float) lat;

            }
            ib++;
        }

        maxib = ib;
        statusHandler.info("Size of map array is : " + maxib + " basins");

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

        }
        if (hrap_basin_flag == 1) {
            try (BufferedReader in = new BufferedReader(
                    new FileReader(hrap_f))) {
                statusHandler.info("Reading hrap basin data from file");
                long basin_start = System.currentTimeMillis();
                String eb = new String();
                for (ib = 0; ib < maxib; ib++) {
                    // (eb, 0, 80);
                    eb = in.readLine();
                    int p = eb.length();

                    if (p != -1) {
                        p = 0;
                    }

                    eb = eb.trim();
                    Scanner s = new Scanner(eb);
                    Pattern pc = Pattern.compile("\\s*[A-Z]+$");

                    numpts = s.nextInt();
                    mean_areal_precip_global[ib].hrap_points = numpts;
                    String bchar = "";
                    bchar = s.findInLine(pc);
                    mean_areal_precip_global[ib].bchar = bchar.trim();
                    mean_areal_precip_global[ib].hrap_data = new Hrap_Data[numpts];

                    for (mm = 0; mm < 4; mm++) {
                        mean_areal_precip_global[ib].zones[mm] = -1;
                    }

                    for (l = 0; l < numpts; l++) {
                        Hrap_Data d = dqc.new Hrap_Data();
                        mean_areal_precip_global[ib].hrap_data[l] = d;

                        for (mm = 0; mm < 4; mm++) {
                            mean_areal_precip_global[ib].hrap_data[l].zone[mm] = -1;
                        }

                        // (eb, 0, 100);
                        eb = in.readLine();
                        eb = eb.trim();
                        p = eb.length();

                        if (p == 0) {
                            break;
                        }

                        s.close();
                        s = new Scanner(eb);
                        x = s.nextInt();
                        y = s.nextInt();
                        if (s.hasNextInt()) {
                            ip = s.nextInt();

                            mean_areal_precip_global[ib].hrap_data[l].x = x;
                            mean_areal_precip_global[ib].hrap_data[l].y = y;
                            mean_areal_precip_global[ib].hrap_data[l].zone[ip
                                    - 1] = 1;
                            mean_areal_precip_global[ib].zones[0] = 1;

                            if (s.hasNextInt()) {
                                ip2 = s.nextInt();
                                if (ip2 < 0 || ip2 > 4) {
                                    statusHandler.error(
                                            "HRAP error in read_basin_data routine.");
                                    s.close();
                                    return false;
                                }

                                mean_areal_precip_global[ib].hrap_data[l].zone[ip2
                                        - 1] = 1;
                                mean_areal_precip_global[ib].zones[ip2 - 1] = 1;

                            }

                            if (s.hasNextInt()) {
                                ip3 = s.nextInt();
                                if (ip3 < 0 || ip3 > 4) {
                                    statusHandler.error(
                                            "HRAP error in read_basin_data routine.");
                                    s.close();
                                    return false;
                                }

                                mean_areal_precip_global[ib].hrap_data[l].zone[ip3
                                        - 1] = 1;
                                mean_areal_precip_global[ib].zones[ip3 - 1] = 1;

                            }

                            if (s.hasNextInt()) {
                                ip4 = s.nextInt();
                                if (ip4 < 0 || ip4 > 4) {
                                    statusHandler.error(
                                            "HRAP error in read_basin_data routine.");
                                    s.close();
                                    return false;
                                }

                                mean_areal_precip_global[ib].hrap_data[l].zone[ip4
                                        - 1] = 1;
                                mean_areal_precip_global[ib].zones[ip4 - 1] = 1;

                            }

                        }
                    }
                    s.close();
                }
                statusHandler.info(
                        "Finished reading basin data from file, elapsed time: "
                                + (System.currentTimeMillis() - basin_start)
                                + " ms");
            } catch (IOException | NumberFormatException e) {
                statusHandler.warn(e.getLocalizedMessage(), e);
                return false;
            }
        } else {
            /*
             * Read Linesegs to get the basin definitions. Basins are assumed
             * not to have subareas.
             *
             * Loop over each basin. Retrieve the lineseg data for it if it is
             * available.
             */
            long linesegs_start = System.currentTimeMillis();
            statusHandler.info("Starting Line Segs methods. elapsed: "
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
                        num_points = (pLineSegNode.getHrapEndCol()
                                - pLineSegNode.getId().getHrapBegCol()) + 1;
                        mean_areal_precip_global[ib].hrap_points += num_points;
                        if (mean_areal_precip_global[ib].hrap_data.length > 0) {
                            mean_areal_precip_global[ib].hrap_data = (Hrap_Data[]) resizeArray(
                                    mean_areal_precip_global[ib].hrap_data,
                                    mean_areal_precip_global[ib].hrap_points);
                        } else {
                            mean_areal_precip_global[ib].hrap_data = new Hrap_Data[mean_areal_precip_global[ib].hrap_points];
                        }

                        if (mean_areal_precip_global[ib].hrap_data == null) {
                            statusHandler.error("Could not realloc memory for "
                                    + mean_areal_precip_global[ib].hrap_data);
                            return false;
                        }

                        for (i = pLineSegNode.getId()
                                .getHrapBegCol(); i <= pLineSegNode
                                        .getHrapEndCol(); ++i) {
                            Hrap_Data d = dqc.new Hrap_Data();
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
                    statusHandler
                            .error("Could not retrieve LineSeg data for basin "
                                    + mean_areal_precip_global[ib].hb5);
                }

            }
            statusHandler.info("Finished the Line Segs methods, elapsed time: "
                    + (System.currentTimeMillis() - linesegs_start) + " ms");
        }
        /* initialize basins as always on */
        for (ib = 0; ib < maxib; ib++) {
            mean_areal_precip_global[ib].owner = 9999;
        }
        statusHandler.info("Finished getting Basin Data, elapsed time: "
                + (System.currentTimeMillis() - start) + " ms");
        dqc.setMax_basins(maxib);
        return true;
    }

    private HashMap<String, Geometry> readSSHPBasinData() {
        Date now = TimeUtil.newGmtCalendar().getTime();
        long start = now.getTime();
        StringBuilder query = new StringBuilder("SELECT ");
        query.append(AREAID_TABLE_NAME).append(", ST_AsBinary(")
                .append(GEOM_TABLE_NAME).append(") as ").append(GEOM_TABLE_NAME)
                .append(" FROM ").append(SSHPBASINS_TABLE).append(";");

        WKBReader wkbReader = new WKBReader();
        try {
            QueryResult geomResults = DirectDbQuery.executeMappedQuery(
                    query.toString(), "maps", QueryLanguage.SQL);
            for (int i = 0; i < geomResults.getResultCount(); ++i) {

                String gid = ((String) geomResults.getRowColumnValue(i, 0));
                Geometry g = null;
                Object obj = geomResults.getRowColumnValue(i, 1);
                if (obj instanceof byte[]) {
                    byte[] wkb = (byte[]) obj;
                    try {
                        g = wkbReader.read(wkb);
                    } catch (ParseException e) {
                        statusHandler.error(
                                "Problems reading data from the database: "
                                        + e.getMessage());
                    }
                } else {
                    statusHandler.error("Expected byte[] received "
                            + obj.getClass().getName() + ": " + obj.toString()
                            + "\n  table=\"" + SSHPBASINS_TABLE + "\"");
                }
                basinData.put(gid, g);
            }
        } catch (VizException e) {
            statusHandler.error("Problem executing db query: " + e.getMessage(),
                    e);
        }
        now = TimeUtil.newGmtCalendar().getTime();
        statusHandler.info(
                "Reading basins in took " + ((now.getTime() - start)) + "ms");
        return basinData;
    }

    public static List<Linesegs> GetLineSegs(String where) {
        StringBuilder query = new StringBuilder(
                "SELECT area_id, hrap_row, hrap_beg_col, hrap_end_col, area FROM ");
        query.append(Linesegs.class.getAnnotation(Table.class).name());
        query.append(" ");
        query.append(where);

        ArrayList<Linesegs> retVal = new ArrayList<>();
        try {
            List<Object[]> results = DirectDbQuery
                    .executeQuery(query.toString(), "ihfs", QueryLanguage.SQL);

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
            statusHandler.error("Problem executing db query: " + e.getMessage(),
                    e);
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
