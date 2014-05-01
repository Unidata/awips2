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
package com.raytheon.viz.hydrocommon.whfslib;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Table;

import com.raytheon.uf.common.dataplugin.shef.tables.Geoarea;
import com.raytheon.uf.common.dataplugin.shef.tables.Linesegs;
import com.raytheon.uf.common.dataplugin.shef.tables.LinesegsId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * GeoUtil class is a singleton class that contains the utility methods for
 * hydrology.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2008  1744       askripsk    Initial creation.
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

public final class GeoUtil {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(GeoUtil.class);

    private final double MAX_DBL = 1.7976931348623157e+308;

    /* Structure for "get_area_linesegs" routine. */
    public static class GeoAreaLineSegs {
        public String area_id; /* The id of the area being processed. */

        public String name; /* The name of the area being processed. */

        public String boundaryType;

        public Double interiorLat;

        public Double interiorLon;

        public int numrows; /*
                             * The number of rows in the geoarea being
                             * processed.
                             */

        public long[] rows; /*
                             * The numbers of the HRAP rows in the area being
                             * processed.
                             */

        public long[] beg_cols; /*
                                 * The beginning HRAP column corresponding to
                                 * each row.
                                 */

        public long[] end_cols; /*
                                 * The ending HRAP column corresponding to each
                                 * row.
                                 */

        public float avg_val; /*
                               * The average precipitation value in the area
                               * being processed.
                               */

        public float max_val; /*
                               * The maximum precipitation value in the area
                               * being processed.
                               */

        public float min_val; /*
                               * The minimum precipitation value in the area
                               * being processed.
                               */

        public float area_covered; /*
                                    * The percentage of the area covered by
                                    * valid precipitation values.
                                    */

    }

    /**
     * The static singleton instance.
     */
    private static GeoUtil instance;

    /**
     * Singleton constructor.
     * 
     * @return the precipitation utility instance variable.
     */
    public static synchronized GeoUtil getInstance() {
        if (instance == null) {
            instance = new GeoUtil();
        }

        return instance;
    }

    /**
     * Private constructor: Use getInstance().
     */
    private GeoUtil() {
    }

    public String cvt_latlon_from_double(double x) {
        String buf;

        long deg, min, sec;

        double temp, tmpx;

        tmpx = x;
        x = Math.abs(x);

        /*
         * Take the whole part of double and use as degrees. Use the fractional
         * part of double as minutes & seconds.
         */
        deg = (long) x; /* keep the integer part as degrees */

        temp = (x - (deg)) * 60.0;
        min = (long) temp; /* compute the minutes */

        temp = Math.floor(((temp - (min)) * 60.0) + 0.5);
        sec = (long) temp; /* compute the seconds */

        /*
         * Check for boundary errors and correct them.
         */
        if (sec == 60) {
            if (min == 59) {
                deg += 1;
                min = 0;
                sec = 0;
            } else if (min < 59) {
                min += 1;
                sec = 0;
            }
        }

        /*
         * store the latlon value.
         */
        if (tmpx >= 0.0) {
            buf = String.format("%02d %02d %02d", deg, min, sec);
        } else {
            buf = String.format("-%02d %02d %02d", deg, min, sec);
        }

        return buf;
    }

    public double cvt_spaced_format(String str, int negate)
            throws NumberFormatException {
        double coord, deg, min, sec;

        if (str == null) {
            return MAX_DBL;
        }

        /*
         * Convert value.
         */
        String[] parts = str.split(" ");
        deg = Double.parseDouble(parts[0]);

        if (parts.length < 2) {
            min = 0.0;
            sec = 0.0;
        } else {
            min = Double.parseDouble(parts[1]);

            if (parts.length < 3) {
                sec = 0.0;
            } else {
                sec = Double.parseDouble(parts[2]);
            }

            min += sec / 60.00;
        }

        coord = (Math.abs(deg) + (min / 60.00));

        if (deg < 0) {
            coord *= -1.0;
        }

        if (negate > 0) {
            coord *= -1.0;
        }

        return coord;
    }

    public List<Geoarea> getGeoArea(String where, String areaType) {
        StringBuilder query = new StringBuilder("Select * FROM ");
        query.append("Geoarea");
        query.append(" ");
        query.append(where);
        query.append(" ORDER BY area_id ");
System.out.println(query.toString());
        ArrayList<Geoarea> retVal = new ArrayList<Geoarea>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(
                    query.toString(), "ihfs", QueryLanguage.SQL);
            if ((results == null) || (results.size() <= 0)) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error querying GeoArea/Linesegs table for " + areaType + ". " +
                        "Check localization and the GeoArea and Linesegs tables.");
            } else {
                retVal.ensureCapacity(results.size());
                Object[] oa = new Object[results.get(0).length];

                for (int i = 0; i < results.size(); i++) {
                    oa = results.get(i);
                    Geoarea geo = new Geoarea();
                    geo.setAreaId((String) oa[0]);
                    geo.setName((String) oa[1]);
                    geo.setBoundaryType((String) oa[2]);
                    geo.setInteriorLat((Double) oa[3]);
                    geo.setInteriorLon((Double) oa[4]);
                    retVal.add(geo);
                }
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return retVal;
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
                ld.setHrapRow(Integer.parseInt(item[1].toString()));
                ld.setHrapBegCol(Integer.parseInt(item[2].toString()));
                ln.setId(ld);
                ln.setHrapEndCol(Integer.parseInt(item[3].toString()));
                ln.setArea(Double.parseDouble(item[4].toString()));

                retVal.add(ln);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return retVal;
    }

    public List<GeoAreaLineSegs> getGeoAreaLinesegs(String boundarytype) {
        String area_type = boundarytype;
        int numrows = 0;
        String where = "";
        ArrayList<Geoarea> geoAreaList = new ArrayList<Geoarea>();
        ArrayList<GeoAreaLineSegs> geoLineSegs = new ArrayList<GeoAreaLineSegs>();

        where = String
                .format("WHERE boundary_type = '%s' AND area_id IN (SELECT area_id FROM linesegs)",
                        area_type);
        geoAreaList = (ArrayList<Geoarea>) getGeoArea(where, area_type);

        if (geoAreaList.size() == 0) {
            System.out.println("In routine \"process_areas:\n"
                    + "Could not retrieve any basin data from "
                    + "the \"GeoArea\" table.\n");
            return geoLineSegs;
        }
        for (Geoarea geoa : geoAreaList) {

            /*
             * read the HRAP bin coords for the area and extract the information
             * from the linesegs table.
             */
            where = String.format(" where area_id = '%s' ", geoa.getAreaId());

            ArrayList<Linesegs> linesegPtr = new ArrayList<Linesegs>();

            linesegPtr = GetLineSegs(where);

            numrows = linesegPtr.size();

            if (linesegPtr.size() == 0) {
                System.out
                        .println(String
                                .format("\nIn routine \"get_area_linesegs\":\n"
                                        + "LineSeg information not available for %s.\n",
                                        geoa.getAreaId()));
                continue;
            }
            GeoAreaLineSegs pNode = new GeoAreaLineSegs();

            pNode.area_id = geoa.getAreaId();
            pNode.name = geoa.getName();
            pNode.boundaryType = geoa.getBoundaryType();
            pNode.interiorLat = geoa.getInteriorLat();
            pNode.interiorLon = geoa.getInteriorLon();
            pNode.numrows = numrows;
            pNode.rows = new long[numrows];
            pNode.beg_cols = new long[numrows];
            pNode.end_cols = new long[numrows];

            for (int i = 0; i < pNode.numrows; ++i) {
                pNode.rows[i] = linesegPtr.get(i).getId().getHrapRow();
                pNode.beg_cols[i] = linesegPtr.get(i).getId().getHrapBegCol();
                pNode.end_cols[i] = linesegPtr.get(i).getHrapEndCol();
            }

            geoLineSegs.add(pNode);
            linesegPtr.clear();
            linesegPtr = null;
        }// end for loop (Geoarea geoa : geoAreaList)
        geoAreaList.clear();
        geoAreaList = null;
        return geoLineSegs;
    }
}
