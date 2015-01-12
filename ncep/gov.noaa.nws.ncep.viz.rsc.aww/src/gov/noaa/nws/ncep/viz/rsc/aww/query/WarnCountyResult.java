/**
 * gov.noaa.nws.ncep.viz.rsc.warn.rsc.WarnCountyResult
 * 
 * Date created August 25, 2011
 *
 *  This code is developed by the SIB for use in the AWIPS2 system. 
 */

package gov.noaa.nws.ncep.viz.rsc.aww.query;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
//import java.util.concurrent.*;
//import com.vividsolutions.jts.geom.*;

/**
 * WarnCountyResult: this class handling database query part for the
 * WarnResource.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011-08-25    456        G. Zhang    Initial creation.
 * 2013-08-12   #1028       G. Hull     rm WarnResource, PDOList, IRscDataObj
 * 2013-08-13   #1028       G. Hull  don't add duplicate fips to the query
 * 
 * </pre>
 * 
 * @author gzhang
 * @version 1.0
 */

public class WarnCountyResult {

    private static final double ENV_MIN_X = -180.0;

    private static final double ENV_MAX_X = 180.0;

    private static final double ENV_MIN_Y = -90;

    private static final double ENV_MAX_Y = 90.0;

    private static Logger logger = Logger.getLogger(WarnCountyResult.class
            .getCanonicalName());

    private String geoConstraint;

    private StringBuilder query = new StringBuilder();

    // Don't need the state but keep for now since it shouldn't slow us down
    // much at all.
    // Could also do w/o the countyname if we stick with using the name from the
    // StationsTable.
    private String queryPrefix = "select AsBinary(the_geom), AsBinary(the_geom_0_001), state,countyname,fips from mapdata.county where ";

    private Map<String, ArrayList<ArrayList<Object[]>>> fipsMultiResultMap = new HashMap<String, ArrayList<ArrayList<Object[]>>>();

    private List<String> fipsList = new ArrayList<String>();

    public WarnCountyResult(/* WarnResource wcnRsc, Object[] pdoList */) {
        geoConstraint = String
                .format("the_geom_0_001 && ST_SetSrid('BOX3D(%f %f, %f %f)'::box3d,4326)",
                        ENV_MIN_X, ENV_MIN_Y, ENV_MAX_X, ENV_MAX_Y);
    }

    /**
     * TODO : This needs to be changed to be able to query more fips when new
     * aww records are ingested. Currently there is a bug when auto update is
     * triggered for aww's that have counties that were not in the list of
     * original fips to query.
     * 
     */
    public void buildQueryPart2(List<String> countyFips) {
        if (countyFips == null) {
            return;
        }

        for (String fips : countyFips) {
            if (!fipsList.contains(fips)) {
                fipsList.add(fips);

                query.append(" ( fips ='");
                query.append(fips);
                query.append("' ) OR  ");
            }
        }
    }

    public void populateMap() {

        List<Object[]> results = null;

        try {
            String wholeQuery = queryPrefix + geoConstraint + " AND ("
                    + query.substring(0, query.lastIndexOf("OR")) + " );";

            results = DirectDbQuery.executeQuery(wholeQuery, "maps",
                    QueryLanguage.SQL);

        } catch (Exception e) {
            logger.log(
                    Level.SEVERE,
                    "_____ Exception in query string or result: "
                            + e.getMessage());
            return;
        }

        for (Object[] o : results) {
            if (o == null || o.length != 5 || o[2] == null || o[3] == null
                    || o[4] == null) {
                continue;
            }

            ArrayList<Object[]> obs = new ArrayList<Object[]>();
            obs.add(new Object[] { o[0], o[1] });

            String key = (String) o[4];

            if (fipsMultiResultMap.containsKey(key)) {

                fipsMultiResultMap.get(key).add(obs);
            } else {
                ArrayList<ArrayList<Object[]>> list = new ArrayList<ArrayList<Object[]>>();
                list.add(obs);
                fipsMultiResultMap.put(key, list);
            }

            // fipsResultMap.put((String)o[4], obs);
        }
    }

    /**
     * 2011-09-01: Loiza county in Puerto Rico with fips 72087 has NO record in
     * Raytheon's database: maps mapdata.county table and we need to handle
     * cases like that.
     * 
     * TODO: move this handling to the query place?
     */

    public ArrayList<ArrayList<Object[]>> getStateCountyResult2(String fips) {

        ArrayList<ArrayList<Object[]>> list = fipsMultiResultMap.get(fips);

        if (list == null) {
            // logger.log(Level.WARNING, "_______ No result for fips: "+fips);

            return new ArrayList<ArrayList<Object[]>>();
        }

        return list;
    }
}
