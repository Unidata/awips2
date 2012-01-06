/*
 * Query
 *
 * Date created 06 Feb 2009
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 *
 */
package gov.noaa.nws.ncep.edex.uengine.tasks.query;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.hibernate.criterion.Disjunction;
import org.hibernate.criterion.Expression;

import com.raytheon.edex.db.dao.spatial.ObStationDao;
import com.raytheon.edex.msg.ResponseMessageNull;
import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.pointdata.spatial.ObStation;

/**
 * GempakAreaQuery
 * 
 * Performs conversion of GEMPAK AREA parameter into AWIPS2 database query,
 * executes the query and returns a list of station IDs as a results of the
 * query.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date                 Ticket#         Engineer                Description
 * ------------         ----------      -----------             --------------------------
 * 03/18/2009                           mgamazaychikov          Initial Creation
 * 06/02/2009			92				mgamazaychikov			Added ability to return
 * 																station's WMO Index
 * 
 * </pre>
 * 
 * @author mgamazaychikov
 * @version 1
 */

@SuppressWarnings("deprecation")
public class GempakAreaQuery extends ScriptTask {

    /* Enumeration containing the GEMPAK station header keywords */
    public enum ShdrKey {
        country, elevation, lat, lon, priority, state, wmoindex
    };

    /*
     * A mapping between the enumeration and the representation in the database
     * table
     */
    public static HashMap<String, ShdrKey> shdrMap = new HashMap<String, ShdrKey>();
    static {
        shdrMap.put("COUN", ShdrKey.country);
        shdrMap.put("SELV", ShdrKey.elevation);
        shdrMap.put("SLAT", ShdrKey.lat);
        shdrMap.put("SLON", ShdrKey.lon);
        shdrMap.put("SPRI", ShdrKey.priority);
        shdrMap.put("STAT", ShdrKey.state);
        shdrMap.put("STID", ShdrKey.wmoindex);
    }

    private String[] gArea = null;

    private boolean isState = false;

    private boolean isStation = false;

    private boolean isCountry = false;

    private boolean isBox = false;

    private boolean isDset = false;

    private boolean isStationHdr = false;

    private boolean isWmoIndex = false;

    private boolean isReturnStationICAO = true;

    public GempakAreaQuery() {
    }

    @SuppressWarnings("unchecked")
    @Override
    public List<String> execute() throws Exception {

        ObStationDao obStationDao;
        List<String> resultsBack = new ArrayList();
        List<ObStation> results;

        try {
            /*
             * Borrow the dao object
             */
            obStationDao = new ObStationDao();
            results = new ArrayList<ObStation>();
            if (isBox) {
                /*
                 * Query by Spatial Box (UpperLeftLat/Lon, LowerRightLat/Lon)
                 */
                List<ObStation> results2 = null;
                results2 = obStationDao.queryBySpatialBox(
                        Double.parseDouble(gArea[0]),
                        Double.parseDouble(gArea[1]),
                        Double.parseDouble(gArea[2]),
                        Double.parseDouble(gArea[3]));
                for (int ii = 0; ii < results2.size(); ii++) {
                    if (results2.get(ii) != null) {
                        results.add(results2.get(ii));
                    }
                }
            } else if (isState) {
                /*
                 * Query by state
                 */
                results = obStationDao.queryByState(gArea[0]);
            } else if (isWmoIndex) {
                /*
                 * Query by WMO INdex
                 */
                int wmoIndex = Integer.parseInt(gArea[0]);
                results.add(obStationDao.queryByWmoIndex(wmoIndex));
            } else if (isStation) {
                /*
                 * Query by ICAO
                 */
                for (String gemArea : gArea) {
                    ObStation station = null;
                    station = obStationDao.queryByIcao(gemArea);
                    if (station != null) {
                        if (!results.contains(station))
                            results.add(station);
                    }
                }
            } else if (isCountry) {
                /*
                 * Query by country
                 */
                results = obStationDao.queryByCountry(gArea[0]);
            } else if (isDset) {
                /*
                 * TODO
                 */
                resultsBack.add("GempakAreaQuery returned no results");
                return resultsBack;
            } else if (isStationHdr) {
                /*
                 * Query by Criteria - for station header queries
                 */
                Object[] names = obStationDao.executeSQLQuery(gArea[0]);
                /*
                 * Exit early if no stations are identified
                 */
                if (names.length == 0) {
                    resultsBack.add("GempakAreaQuery returned no results");
                    return resultsBack;
                }

                Disjunction stationEq = Expression.disjunction();
                for (int i = 0; i < names.length; i++) {
                    if (((Object[]) names[i])[0] == null) {

                        stationEq.add(Expression.eq("wmoIndex",
                                ((Object[]) names[i])[1]));
                    } else {
                        stationEq.add(Expression.eq("icao",
                                ((Object[]) names[i])[0].toString()));
                    }

                }
                results = (List<ObStation>) obStationDao
                        .executeCriteriaQuery(stationEq);
            }

        } catch (Exception e) {
            throw new MicroEngineException("Unable to obtain spatial data", e);
        }
        String strres = null;
        StringBuffer resultsBuf = new StringBuffer();

        if (results == null || results.size() == 0) {
            /*
             * Exit early if no results are found
             */
            resultsBack.add("GempakAreaQuery returned no results");
            return resultsBack;

        } else if (results != null) {
            /*
             * Convert the results to string buffer, then to string to prepare
             * for the output
             */
            for (int i = 0; i < results.size(); i++) {
                if (results.get(i).getIcao() != null) {
                    if (isReturnStationICAO) {
                        resultsBuf.append(results.get(i).getIcao());
                    } else {
                        resultsBuf.append(results.get(i).getWmoIndex());
                    }
                    resultsBuf.append(";");
                    resultsBuf.append((int) (results.get(i).getGeometry()
                            .getY() * 100));
                    resultsBuf.append(";");
                    resultsBuf.append((int) (results.get(i).getGeometry()
                            .getX() * 100));
                    resultsBuf.append(";");
                    int stationElev;
                    /*
                     * Enclose the getElevation method with try-catch block to
                     * avoid exception thrown when trying to get elevation that
                     * is missing
                     */
                    try {
                        stationElev = results.get(i).getElevation().intValue();
                    } catch (Exception e) {
                        stationElev = -9999;
                    }
                    resultsBuf.append(stationElev);
                    resultsBuf.append("|");
                }
            }
        }

        strres = resultsBuf.substring(0, resultsBuf.length() - 1);
        resultsBack.add(strres);
        return resultsBack;

    }

    public void setReturnStationWmoIndex() {
        isReturnStationICAO = false;
        return;
    }

    public void setReturnStationICAO() {
        isReturnStationICAO = true;
        return;
    }

    public void setGempakArea(String aValue) {

        String[] inputStringArray = null;
        if (aValue.contains("#")) {
            isBox = true;
            /*
             * AREA = #clat;clon;dlat;dlon
             * 
             * This defines a latitude/longitude range by the center latitude
             * and longitude. The lower left corner is (clat-dlat; clon-dlon);
             * the upper right corner is (clat+dlat; clon+dlon). No corrections
             * are made for the poles or the International Date Line.
             */
            inputStringArray = aValue.substring(1).split(";");
            gArea = new String[inputStringArray.length];

            /*
             * upperLeftLat
             */
            gArea[0] = Double
                    .toString((Double.parseDouble(inputStringArray[0]) + Double
                            .parseDouble(inputStringArray[2])));
            /*
             * upperLeftLon
             */
            gArea[1] = Double
                    .toString((Double.parseDouble(inputStringArray[1]) - Double
                            .parseDouble(inputStringArray[3])));
            /*
             * lowerRightLat
             */
            gArea[2] = Double
                    .toString((Double.parseDouble(inputStringArray[0]) - Double
                            .parseDouble(inputStringArray[2])));
            /*
             * lowerRightLon
             */
            gArea[3] = Double
                    .toString((Double.parseDouble(inputStringArray[1]) + Double
                            .parseDouble(inputStringArray[3])));
            return;
        }
        if (aValue.contains("@") && aValue.length() == 3) {
            isState = true;
            /*
             * @ST This area includes those stations located in the state,
             * province or country defined by ST. Only some countries are
             * recognized (US,CN,MX,CI,BW,AU).
             */
            gArea = new String[1];
            gArea[0] = aValue.substring(1).toUpperCase();
            return;
        }
        if (aValue.contains("@") && aValue.length() > 3
                && !aValue.contains(":c")) {
            isStation = true;
            /*
             * @STN1[;STN2;...;STNn] This area includes the stations listed,
             * where STNi may be a station identifier or a station number.
             */
            inputStringArray = aValue.substring(1).split(";");
            gArea = new String[inputStringArray.length];
            for (int ig = 0; ig < inputStringArray.length; ig++) {
                if (inputStringArray[ig].length() == 3) {
                    gArea[ig] = "K" + inputStringArray[ig].toUpperCase();
                } else if (inputStringArray[ig].length() == 4) {
                    gArea[ig] = inputStringArray[ig].toUpperCase();
                }
            }
            return;
        }
        if (aValue.contains("@") && aValue.contains(":c")) {
            isCountry = true;
            /*
             * @CN:C This area includes those stations located in the country
             * defined by CN.
             */
            gArea = aValue.substring(1).split(":");
            return;
        }
        if (aValue.matches("^[-+]?\\d+(\\.\\d+)?$")) {
            isWmoIndex = true;
            gArea[0] = aValue;
            return;
        }
        if (!aValue.contains("#") && !aValue.contains("@")
                && aValue.contains(";")) {
            isBox = true;
            /*
             * lat1;lon1;lat2;lon2 This defines a latitude/longitude range where
             * (lat1, lon1) is the lower left corner and (lat2, lon2) is the
             * upper right corner. West longitude is negative.
             */
            inputStringArray = aValue.split(";");
            /*
             * upperLeftLat
             */
            gArea = new String[inputStringArray.length];
            gArea[0] = inputStringArray[2];
            /*
             * upperLeftLon
             */
            gArea[1] = inputStringArray[1];
            /*
             * lowerRightLat
             */
            gArea[2] = inputStringArray[0];
            /*
             * lowerRightLon
             */
            gArea[3] = inputStringArray[3];
            return;
        }
        if (!aValue.contains("@") && aValue.contains(":")) {
            isStationHdr = true;
            /*
             * SHDR:iloval:ihivalThis area defines a range of integer values for
             * the station header,SHDR. Valid keywords for SHDR are:
             * 
             * COUN -- country SELV -- elevation (in meters) SLAT -- latitude
             * (in degrees x 100) SLON -- longitude (in degrees x 100, West is
             * negative) SPRI -- priority STAT -- state STID -- character
             * identifier (6 digits for surface--usually the WMO 5-digit number
             * followed by a 0)
             * 
             * where COUN, STAT and STID are not very useful, since the integer
             * representation of characters is system dependent.
             * 
             * For example, SELV:0:2000 specifies stations whose elevations are
             * less than 2000 meters.
             */
            inputStringArray = aValue.split(":");
            StringBuffer sql = new StringBuffer();
            gArea = new String[1];
            /*
             * Construct SQL Query string for geometry (lat, lon) query
             */
            if (translateShdrKeyWord(inputStringArray[0].toUpperCase())
                    .toString().equals("lat")) {
                sql.append("SELECT icao,wmoindex FROM awips.common_obs_spatial "
                        + "WHERE the_geom && GeomFromText('");
                if (inputStringArray.length == 3) {
                    sql.append("POLYGON((");
                    if (inputStringArray[1] == null
                            && inputStringArray[2] != null) {
                        Double lat = Double.parseDouble(inputStringArray[2]);
                        /*
                         * Case SLAT::highlat
                         */
                        sql.append("-180.00 -90.00, -180.00 " + lat
                                + ", 180.00 " + lat
                                + ", 180.00 -90.00, -180.00 -90.00))');");
                    } else if (inputStringArray[1] != null
                            && inputStringArray[2] == null) {
                        Double lat = Double.parseDouble(inputStringArray[2]);
                        /*
                         * Case SLAT:lowlat:
                         */
                        sql.append("-180.00 90.00, -180.00 " + lat
                                + ", 180.00 " + lat
                                + ", 180.00 90.00, -180.00 90.00))');");
                    } else if (inputStringArray[1] != null
                            && inputStringArray[2] != null) {
                        Double lolat = Double.parseDouble(inputStringArray[1]);
                        Double hilat = Double.parseDouble(inputStringArray[2]);
                        /*
                         * Case SLAT:lowlat:highlat
                         */
                        sql.append("-180.00 " + lolat + ", -180.00 " + hilat
                                + ", 180.00 " + hilat + ", 180.00 " + lolat
                                + ", -180.00 " + lolat + "))');");
                    }
                } else if (inputStringArray.length == 2
                        && inputStringArray[1] != null) {
                    /*
                     * Case SLAT:lat
                     */
                    Double lat = Double.parseDouble(inputStringArray[1]);
                    sql.append("LINESTRING(");
                    sql.append("-180.00 " + lat + ", 180.00 " + lat + ")');");
                }
            } else if (translateShdrKeyWord(inputStringArray[0].toUpperCase())
                    .toString().equals("lon")) {
                sql.append("SELECT icao,wmoindex FROM awips.common_obs_spatial "
                        + "WHERE the_geom && GeomFromText('");

                if (inputStringArray.length == 3) {
                    sql.append("POLYGON((");
                    if (inputStringArray[1] == null
                            && inputStringArray[2] != null) {
                        Double lon = Double.parseDouble(inputStringArray[2]);
                        /*
                         * Case SLON::highlon
                         */
                        sql.append("-180.00 -90.00, " + lon + " -90.00, " + lon
                                + " 90.00, -180.00 90.00, -180.00 -90.00))');");
                    } else if (inputStringArray[1] != null
                            && inputStringArray[2] == null) {
                        Double lon = Double.parseDouble(inputStringArray[2]);
                        /*
                         * Case SLAT:lowlon:
                         */
                        sql.append(lon
                                + " -90.00, 180.00 -90.00, 180.00 90.00, "
                                + lon + "90.00, " + lon + " -90.00))');");
                    } else if (inputStringArray[1] != null
                            && inputStringArray[2] != null) {
                        Double lolat = Double.parseDouble(inputStringArray[1]);
                        Double hilat = Double.parseDouble(inputStringArray[2]);
                        /*
                         * Case SLON:lowlat:highlat
                         */
                        sql.append("-180.00 " + lolat + ", -180.00 " + hilat
                                + ", 180.00 " + hilat + ", 180.00 " + lolat
                                + ", -180.00 " + lolat + "))');");
                    }
                } else if (inputStringArray.length == 2
                        && inputStringArray[1] != null) {
                    /*
                     * Case SLON:lon
                     */
                    Double lon = Double.parseDouble(inputStringArray[1]);
                    sql.append("LINESTRING(");
                    sql.append(lon + " -90.00, " + lon + " 90.00)');");
                }
            }
            /*
             * Construct SQL Query string for all other queries
             */
            else {
                sql.append("SELECT icao, wmoindex FROM awips.common_obs_spatial WHERE ");
                sql.append(translateShdrKeyWord(
                        inputStringArray[0].toUpperCase()).toString());
                if (inputStringArray.length == 3) {
                    if (inputStringArray[1] == null
                            && inputStringArray[2] != null) {
                        sql.append(" < '" + inputStringArray[2] + "';");
                    } else if (inputStringArray[1] != null
                            && inputStringArray[2] == null) {
                        sql.append(" > '" + inputStringArray[1] + "';");
                    } else {
                        sql.append(" BETWEEN '" + inputStringArray[1]
                                + "' and '" + inputStringArray[2] + "';");
                    }
                } else if (inputStringArray.length == 2) {
                    sql.append("='" + inputStringArray[1] + "';");
                }
            }

            gArea[0] = sql.toString();
            return;
        }
    }

    public static ShdrKey translateShdrKeyWord(String shdrKeyWord) {
        return shdrMap.get(shdrKeyWord);
    }

    public ResponseMessageNull makeNullResponse() {
        String message = "GempakAreaQuery returned no results";
        return ResponseMessageNull.generateNullResponse(message, null, null);
    }

}