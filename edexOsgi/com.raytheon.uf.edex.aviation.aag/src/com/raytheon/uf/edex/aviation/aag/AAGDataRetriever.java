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
package com.raytheon.uf.edex.aviation.aag;

import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.pointdata.PointDataConstants;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.pointdata.PointDataQuery;

import jep.JepConfig;
import jep.JepException;

/**
 * Retrieves AAG data for generating forecasts
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2017  6110       tgurney     Initial creation
 * Mar 6, 2020  DCS21314   mporricelli  Implement use of 1hr lightning
 * </pre>
 *
 * @author tgurney
 */

public class AAGDataRetriever {

    private static final String PLUGIN_NAME = "bufrmosLAMP";

    private static final String LAMP_PARAMS_LIST = "stationId, refTime, "
            + "forecastHr, ceiling_bestCat, windSpeedInflated, "
            + "MaxWindSpeed, precipFreezing, clouds_OV, ceiling_cat1, ceiling_cat2, "
            + "ceiling_cat3, ceiling_cat4, ceiling_cat5, ceiling_cat6, ceiling_cat7, vis_cat5, "
            + "vis_cat4, vis_cat6, vis_cat1, vis_cat3, vis_cat2, QPF6hr_bestCat, tstorm6hr, "
            + "POP_bestCat, dewpoint, vis_bestCat, precipSnow, clouds_SC, temperature, "
            + "clouds_BK, clouds_CL, precipType, obVis_bestCat, clouds_bestCat, POP6hr, "
            + "severe6hr, csevere6hr, POP_hour, windDir, cvis_bestCat, POP_hour_bestCat, "
            + "ltg2hr, ltg_bestCat, tstorm_bestCat, PQPF_6hr, ceiling_cat8, "
            + "c_ceiling_cat1, c_ceiling_cat2, c_ceiling_cat3, c_ceiling_cat4, "
            + "c_ceiling_cat5, c_ceiling_cat6, c_ceiling_cat7, c_ceiling_cat8, "
            + "c_ceiling_bestCat, cvis_cat1, cvis_cat2, cvis_cat3, cvis_cat4, "
            + "cvis_cat5, cvis_cat6, ltg1hr";

    private static final String PYTHON_SCRIPT_PATH = LocalizationUtil
            .join("aviation", "python", "AAGLampDataProcessor.py");

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private PointDataContainer getLampData(String stationId) {
        PointDataContainer pdc = null;
        try {
            Map<String, RequestConstraint> constraintMap = new HashMap<>();
            constraintMap.put(PointDataConstants.LOCATION_STATIONID,
                    new RequestConstraint(stationId));
            constraintMap.put("pluginName", new RequestConstraint(PLUGIN_NAME));
            TimeQueryRequest req = new TimeQueryRequest();
            req.setMaxQuery(true);
            req.setPluginName(PLUGIN_NAME);
            req.setQueryTerms(constraintMap);
            List<?> timeResult = (List<?>) RequestRouter.route(req);
            if (!timeResult.isEmpty()) {
                DataTime[] dataTime = timeResult.toArray(new DataTime[0]);
                PointDataQuery query = new PointDataQuery(PLUGIN_NAME);
                query.requestAllLevels();
                query.addParameter(PointDataConstants.LOCATION_STATIONID,
                        stationId, "=");
                query.addParameter(PluginDataObject.REFTIME_ID,
                        dataTime[0].toString(), "=");
                query.setParameters(LAMP_PARAMS_LIST);
                pdc = query.execute();
            }
        } catch (Exception e) {
            logger.warn("Failed to get bufrmosLAMP point data for " + stationId,
                    e);
        }
        return pdc;
    }

    private PythonScript getPython(String localizationPath)
            throws JepException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext baseCtx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File runner = pathMgr.getFile(baseCtx, localizationPath);
        String filePath = runner.getPath();
        String includePath = PyUtil.buildJepIncludePath(
                runner.getParentFile().getPath(),
                pathMgr.getFile(baseCtx, "python").getPath());
        JepConfig config = new JepConfig().setIncludePath(includePath)
                .setClassLoader(getClass().getClassLoader());
        return new PythonScript(config, filePath);
    }

    @SuppressWarnings("unchecked")
    private List<AAGData> getAAGData(String stationId, PythonScript python) {
        List<AAGData> rval = Collections.emptyList();
        PointDataContainer pdc = getLampData(stationId);
        if (pdc == null) {
            return rval;
        }
        try {
            Map<String, Object> args = new HashMap<>();
            args.put("siteID", stationId);
            args.put("pdc", pdc);
            rval = (List<AAGData>) python.execute("getAAGData", args);
        } catch (JepException e) {
            logger.warn("Failed to process LAMP data for station " + stationId,
                    e);
        }
        return rval;
    }

    /**
     * Get the latest available forecast data for each station
     *
     * @param stationIds
     * @return Map of station id to list of forecast data
     */
    public Map<String, List<AAGData>> getAAGData(List<String> stationIds) {
        logger.info("Got " + stationIds.size() + " station IDs");
        Map<String, List<AAGData>> rval = new HashMap<>();
        try (PythonScript python = getPython(PYTHON_SCRIPT_PATH)) {
            if (!stationIds.isEmpty()) {
                for (String stationId : stationIds) {
                    rval.put(stationId, getAAGData(stationId, python));
                }
                long nonEmptys = rval.values().stream()
                        .filter(e -> !e.isEmpty()).count();
                logger.info("Got data for " + nonEmptys + " stations");
            }
        } catch (JepException e) {
            logger.error("Failed to initialize Python interpreter for "
                    + PYTHON_SCRIPT_PATH, e);
        }
        return rval;
    }
}
