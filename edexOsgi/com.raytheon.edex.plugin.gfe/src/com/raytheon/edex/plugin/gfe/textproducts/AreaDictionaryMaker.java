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
package com.raytheon.edex.plugin.gfe.textproducts;

import java.io.File;
import java.io.PrintWriter;
import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.gfe.python.GfePyIncludeUtil;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

import jep.JepConfig;
import jep.JepException;

/**
 * Code to generate the AreaDictionary for text formatters
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer        Description
 * ------------- -------- --------------- --------------------------------------
 * May 04, 2011           wldougher       Moved from MapManager
 * Oct 08, 2014  4953     randerso        Added hooks for TCVAreaDictionary
 *                                        creation
 * Oct 10, 2014  3685     randerso        Add code to generate the fips2cities
 *                                        and zones2cites python modules from
 *                                        the GIS database tables
 * Dec 08, 2014  4953     randerso        Updated Jep include path to allow use
 *                                        of LocalizationSupport
 * Jul 13, 2015  4500     rjpeter         Fix SQL Injection concerns.
 * Dec 15, 2015  17933    mgamazaychikov  Add four new corners to PART_OF_STATE.
 * Jan 08, 2016  5237     tgurney         Replace calls to deprecated
 *                                        LocalizationFile methods
 * Jul 18, 2016  5747     dgilling        Move edex_static to common_static.
 * Oct 20, 2016  5953     randerso        Generate fips2cities and zones2cities in
 *                                        common_static/configured/gfe/python
 * Feb 20, 2018  6602     dgilling        Update for new text utilities path.
 * Jun 03, 2019  7852     dgilling        Update code for jep 3.8.
 *
 * </pre>
 *
 * @author wldougher
 */

public class AreaDictionaryMaker {
    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AreaDictionaryMaker.class);

    protected static final String FIPS_CITY_QUERY =
    "SELECT name, population,  ST_Y(city.the_geom), ST_X(city.the_geom) "
            + "FROM mapdata.city, mapdata.county "
            + "WHERE county.state = :state AND substring(fips,3,3) = :num "
            + "AND ST_Contains(county.the_geom, city.the_geom) "
            + "ORDER BY city.name;";

    protected static final String ZONES_CITY_QUERY =
    "SELECT city.name, population,  ST_Y(city.the_geom), ST_X(city.the_geom) "
            + "FROM mapdata.city, mapdata.zone "
            + "WHERE zone.state = :state AND zone.zone = :num "
            + "AND ST_Contains(zone.the_geom, city.the_geom) "
            + "ORDER BY city.name;";

    protected static final Map<String, String> PART_OF_STATE;

    static {
        PART_OF_STATE = new HashMap<>(30, 1.0f);
        PART_OF_STATE.put(null, "");
        PART_OF_STATE.put("bb", "big bend");
        PART_OF_STATE.put("c", "");
        PART_OF_STATE.put("cc", "central");
        PART_OF_STATE.put("E", "");
        PART_OF_STATE.put("ea", "east");
        PART_OF_STATE.put("ec", "east central");
        PART_OF_STATE.put("ee", "eastern");
        PART_OF_STATE.put("er", "east central upper");
        PART_OF_STATE.put("eu", "eastern upper");
        PART_OF_STATE.put("M", "");
        PART_OF_STATE.put("mi", "middle");
        PART_OF_STATE.put("nc", "north central");
        PART_OF_STATE.put("ne", "northeast");
        PART_OF_STATE.put("nn", "northern");
        PART_OF_STATE.put("nr", "north central upper");
        PART_OF_STATE.put("nw", "northwest");
        PART_OF_STATE.put("pa", "panhandle");
        PART_OF_STATE.put("pd", "piedmont");
        PART_OF_STATE.put("sc", "south central");
        PART_OF_STATE.put("se", "southeast");
        PART_OF_STATE.put("so", "south");
        PART_OF_STATE.put("sr", "south central upper");
        PART_OF_STATE.put("ss", "southern");
        PART_OF_STATE.put("sw", "southwest");
        PART_OF_STATE.put("up", "upstate");
        PART_OF_STATE.put("wc", "west central");
        PART_OF_STATE.put("wu", "western upper");
        PART_OF_STATE.put("ww", "western");
        PART_OF_STATE.put("ws", "southwest");
        PART_OF_STATE.put("es", "southeast");
        PART_OF_STATE.put("wn", "northwest");
        PART_OF_STATE.put("en", "northeast");
    }

    protected IPathManager pathMgr = PathManagerFactory.getPathManager();

    private Map<String, String> stateDict;

    /**
     * Generate the AreaDictionary.py and CityLocation.py scripts for site,
     * using editAreaAttrs.
     *
     * @param siteID
     *            The site for which the area dictionary file and city location
     *            file should be generated.
     * @param editAreaAttrs
     *            A Map from edit area names to shape file attributes
     */
    public void genAreaDictionary(String siteID,
            Map<String, List<Map<String, Object>>> editAreaAttrs) {
        statusHandler.info("Area Dictionary generation phase");

        if (siteID == null) {
            throw new IllegalArgumentException("site is null");
        }

        if (siteID.isEmpty()) {
            throw new IllegalArgumentException("site is an empty string");
        }

        if (editAreaAttrs == null) {
            throw new IllegalArgumentException("null edit area attributes");
        }

        long t0 = System.currentTimeMillis();
        genStateDict();

        LocalizationContext configuredCtx = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.CONFIGURED);
        configuredCtx.setContextName(siteID);

        List<Map<String, Object>> countyAttrs = editAreaAttrs.get("Counties");
        List<Map<String, Object>> zoneAttrs = editAreaAttrs.get("Zones");

        if (countyAttrs != null) {
            genFips2Cities(configuredCtx, countyAttrs);
        } else {
            statusHandler.warn(
                    "No county edit area attributes found. Not generating fips2cites.");
        }

        if (zoneAttrs != null) {
            genZones2Cities(configuredCtx, zoneAttrs);
        } else {
            statusHandler.warn(
                    "No zone edit area attributes found. Not generating zones2cites.");
        }

        LocalizationContext baseCtx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File scriptFile = pathMgr.getFile(baseCtx, LocalizationUtil.join("gfe",
                "python", "createAreaDictionary.py"));

        File configDir = pathMgr.getFile(configuredCtx,
                LocalizationUtil.join("gfe", "python"));

        String includePath = PyUtil.buildJepIncludePath(true,
                configDir.getPath(),
                GfePyIncludeUtil.getCommonGfeIncludePath());

        Map<String, Object> argMap = new HashMap<>();

        LocalizationContext caveStaticConfig = pathMgr.getContext(
                LocalizationContext.LocalizationType.CAVE_STATIC,
                LocalizationContext.LocalizationLevel.CONFIGURED);
        caveStaticConfig.setContextName(siteID);
        File outputDirFile = pathMgr.getFile(caveStaticConfig,
                GfePyIncludeUtil.TEXT_UTILITIES);
        outputDirFile.mkdir();
        argMap.put("outputDir", outputDirFile.getPath());

        argMap.put("mapDict", editAreaAttrs);

        try (PythonScript pyScript = new PythonScript(
                new JepConfig().setIncludePath(includePath).setClassLoader(
                        getClass().getClassLoader()),
                scriptFile.getPath())) {
            pyScript.execute("createAreaDictionary", argMap);
            // createCityLocation uses script globals modified by
            // createAreaDictionary()
            pyScript.execute("createCityLocation", argMap);

            // check to see if Hazard_TCV was configured for this site
            ILocalizationFile lf = pathMgr.getLocalizationFile(caveStaticConfig,
                    FileUtil.join("gfe", "userPython", "textProducts",
                            "Hazard_TCV.py"));
            if (lf.exists()) {
                argMap.put("siteID", siteID);
                pyScript.execute("createTCVAreaDictionary", argMap);
            }

        } catch (JepException e) {
            statusHandler.error("Error generating area dictionary", e);
        }

        long t1 = System.currentTimeMillis();
        statusHandler
                .info("Area Dictionary generation time: " + (t1 - t0) + " ms");
    }

    private void genFips2Cities(LocalizationContext context,
            List<Map<String, Object>> attributes) {
        genArea2Cities(context, attributes, "fips2cities.py", "fipsdata",
                "FIPS", 'C', FIPS_CITY_QUERY);
    }

    private void genZones2Cities(LocalizationContext context,
            List<Map<String, Object>> attributes) {
        genArea2Cities(context, attributes, "zones2cities.py", "zonedata",
                "Zones", 'Z', ZONES_CITY_QUERY);
    }

    private void genArea2Cities(LocalizationContext context,
            List<Map<String, Object>> attributes, String fileName,
            String dictName, String group, char separator, String cityQuery) {

        ILocalizationFile lf = pathMgr.getLocalizationFile(context,
                FileUtil.join("gfe", "python", fileName));

        try (SaveableOutputStream lfStream = lf.openOutputStream();
                PrintWriter out = new PrintWriter(lfStream)) {
            out.println(dictName + " = {");

            try {
                DecimalFormat df = new DecimalFormat("0.00000");
                StringBuilder sb = new StringBuilder();
                Pattern pattern = Pattern
                        .compile("(\\p{Upper}{2})" + separator + "(\\d{3})");
                CoreDao dao = new CoreDao(DaoConfig.forDatabase("maps"));

                for (Map<String, Object> att : attributes) {
                    String ean = (String) att.get("editarea");
                    if ((ean == null) || ean.isEmpty()) {
                        continue;
                    }

                    Matcher matcher = pattern.matcher(ean);
                    if (!matcher.matches()) {
                        continue;
                    }

                    String state = matcher.group(1);
                    String num = matcher.group(2);

                    String fullStateName = this.stateDict.get(state);
                    String partOfState = PART_OF_STATE.get(att.get("fe_area"));
                    String wfo = (String) att.get("cwa");
                    Map<String, Object> paramMap = new HashMap<>(2, 1);
                    paramMap.put("state", state);
                    paramMap.put("num", num);

                    // retrieve cities for this area
                    QueryResult citiesResult = null;
                    try {
                        citiesResult = dao.executeMappedSQLQuery(cityQuery,
                                paramMap);
                    } catch (Exception e) {
                        statusHandler.error("Error getting cites for " + ean,
                                e);
                    }

                    sb.setLength(0);
                    sb.append("'").append(ean).append("': {");
                    sb.append("'fullStateName': '").append(fullStateName)
                            .append("', ");
                    sb.append("'state': '").append(state).append("', ");

                    sb.append("'cities': [");
                    if ((citiesResult != null)
                            && (citiesResult.getResultCount() > 0)) {
                        for (QueryResultRow city : citiesResult.getRows()) {
                            String name = (String) city.getColumn(0);
                            Object population = city.getColumn(1);
                            Double lat = (Double) city.getColumn(2);
                            Double lon = (Double) city.getColumn(3);

                            if (name.indexOf('\'') >= 0) {
                                sb.append("(\"").append(name).append("\", ");
                            } else {
                                sb.append("('").append(name).append("', ");
                            }
                            if (population == null) {
                                sb.append("None, ");
                            } else {
                                sb.append(population.toString()).append(", ");
                            }
                            sb.append("'").append(df.format(lat)).append("', ");
                            sb.append("'").append(df.format(lon))
                                    .append("'), ");
                        }
                        sb.setLength(sb.length() - 2);
                    }
                    sb.append("], ");

                    sb.append("'partOfState': '").append(partOfState)
                            .append("', ");
                    sb.append("'wfo': '").append(wfo).append("'}, ");
                    out.println(sb.toString());
                }
            } catch (Exception e) {
                statusHandler.error(e.getLocalizedMessage(), e);
            }

            out.println("}");
            out.close();
            lfStream.save();
        } catch (Exception e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        }

    }

    private void genStateDict() {
        try {
            CoreDao dao = new CoreDao(DaoConfig.forDatabase("maps"));
            QueryResult result = dao.executeMappedSQLQuery(
                    "SELECT state, name FROM mapdata.states");
            stateDict = new HashMap<>(result.getResultCount(),
                    1.0f);
            for (QueryResultRow row : result.getRows()) {
                String st = (String) row.getColumn(0);
                String name = (String) row.getColumn(1);
                stateDict.put(st, name);
            }

        } catch (Exception e) {
            statusHandler.error(e.getLocalizedMessage(), e);
        }
    }
}
