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
package com.raytheon.viz.volumebrowser.util;

import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

import com.raytheon.uf.viz.points.PointsDataManager;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Static convenience methods for dealing with the points and lines when loading
 * data from the volume browser.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 03, 2015  3861     bsteffen  Extracted from AbstractDataCatalog
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class PointLineUtil {

    /** key representing all latitude and longitude planes **/
    public static final String LAT_LON_KEY = "LatLon";

    /** key representing all point and line planes **/
    public static final String POINT_LINE_KEY = "PointLine";

    public static final Pattern POINT_PATTERN = Pattern.compile("^Point");

    public static Set<String> getPointLineKeys() {
        Set<String> keySet = new HashSet<String>();
        for (String letter : PointsDataManager.getInstance().getPointNames()) {
            keySet.add("Point" + letter);
        }
        for (String letter : ToolsDataManager.getInstance().getBaselineNames()) {
            keySet.add("Line" + letter);
        }
        return keySet;
    }

    public static boolean isLatLon(String plane) {
        return ((plane != null) && (plane.startsWith("Lat")
                || plane.startsWith("Lon") || plane.equals("LATS") || plane
                    .equals("LONS")));
    }

    public static boolean isPointLine(String plane) {
        return ((plane != null) && (plane.startsWith("Line") || plane
                .startsWith("Point")));
    }

    /**
     * Obtain the point from the given catalog entry or from directly from the
     * volume browser if in time series.
     * 
     * @param catalogEntry
     *            the catalogEntry for which to obtain a point
     * @return the coordinates belonging to the point. Null if there is no point
     *         associated with this catalog Entry.
     */
    public static Coordinate getPointCoordinate(IDataCatalogEntry catalogEntry) {

        String pointLetter = getPointLetter(catalogEntry);

        Coordinate c = PointsDataManager.getInstance().getCoordinate(
                pointLetter);
        if (c == null) {
            c = PointsDataManager.getInstance().getCoordinate("A");
        }
        return c;

    }

    public static String getPointLetter(IDataCatalogEntry catalogEntry) {
        String pointLetter = null;
        switch (catalogEntry.getDialogSettings().getViewSelection()) {
        case TIMEHEIGHT:
        case VARVSHGT:
        case CROSSSECTION:
        case SOUNDING:
            pointLetter = POINT_PATTERN.matcher(
                    catalogEntry.getSelectedData().getPlanesKey())
                    .replaceFirst("");
            break;
        case TIMESERIES:
            pointLetter = catalogEntry.getDialogSettings().getPointsSelection()
                    .getName();
            break;
        }
        return pointLetter;
    }

}
