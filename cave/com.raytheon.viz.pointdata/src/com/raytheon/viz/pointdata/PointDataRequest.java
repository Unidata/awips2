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
package com.raytheon.viz.pointdata;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.Validate;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.message.response.ResponseMessageCatalog;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataServerRequest;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Provides a client-side query capability for generalized PointData pattern.
 * 
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 20, 2009            chammack     Initial creation.
 * 5/27/2009    1982       grichard     Updated stationId key const.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class PointDataRequest {

    private static final String PLUGIN = "pointdata";

    private static final String PLUGINNAME_KEY = "pluginName";

    private static final String STATIONID_KEY = "location.stationId";

    private static final String DATATIME_KEY = "dataTime";

    private static final String REQUESTED_PARAMETERS_KEY = "requestedParameters";

    private static final String RESTRICT_LEVEL = "restrictLevel";

    private static final String RESTRICT_PARAMETER = "restrictParameter";

    private static final String ID = "id";

    private PointDataRequest() {

    }

    /**
     * Request point data for a set of stations or points, over a given set of
     * parameters, at a specific point in time.
     * 
     * 
     * The request can be additionally constrained by optional
     * RequestConstraints.
     * 
     * @param dt
     *            the datatime to request the data for (required)
     * @param pluginName
     *            the plugin to use (required)
     * @param parameters
     *            the parameters to request (required)
     * @param stationIds
     *            the station IDs to constrain to (optional)
     * @param constraints
     *            additional constraints (optional)
     * @return
     */
    public static PointDataContainer requestPointData(DataTime dt,
            String pluginName, String[] parameters, String[] stationIds,
            Map<String, RequestConstraint> constraints) throws VizException {
        DataTime[] dts = null;
        if (dt != null) {
            dts = new DataTime[] { dt };
        }
        return requestPointDataInternal(dts, null, pluginName, parameters,
                stationIds, constraints, false, null, null);
    }

    public static PointDataContainer requestPointData(TimeRange tr,
            String pluginName, String[] parameters, String[] stationIds,
            Map<String, RequestConstraint> constraints) throws VizException {
        return requestPointDataInternal(null, tr, pluginName, parameters,
                stationIds, constraints, false, null, null);
    }

    public static PointDataContainer requestPointData(DataTime[] dt,
            String pluginName, String[] parameters, String[] stationIds,
            Map<String, RequestConstraint> constraints) throws VizException {
        return requestPointDataInternal(dt, null, pluginName, parameters,
                stationIds, constraints, true, null, null);
    }

    /**
     * Request all levels for point data for a set of stations or points, over a
     * given set of parameters, at a specific point in time.
     * 
     * The request can be additionally constrained by optional
     * RequestConstraints.
     * 
     * @param dt
     *            the datatime to request the data for (required)
     * @param pluginName
     *            the plugin to use (required)
     * @param parameters
     *            the parameters to request (required)
     * @param stationIds
     *            the station IDs to constrain to (optional)
     * @param constraints
     *            additional constraints (optional)
     * @return
     */
    public static PointDataContainer requestPointDataAllLevels(DataTime dt,
            String pluginName, String[] parameters, String[] stationIds,
            Map<String, RequestConstraint> constraints) throws VizException {
        DataTime[] dts = null;
        if (dt != null) {
            dts = new DataTime[] { dt };
        }
        return requestPointDataInternal(dts, null, pluginName, parameters,
                stationIds, constraints, true, null, null);
    }

    public static String[] getParameterNames(String pluginName,
            Map<String, RequestConstraint> constraints) throws VizException {
        try {
            Validate.notNull(pluginName, "Plugin Name is required");

            Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
            if (constraints != null) {
                rcMap.putAll(constraints);
            }
            rcMap.put(PLUGINNAME_KEY, new RequestConstraint(pluginName));
            rcMap.put("mode", new RequestConstraint("getParameters"));

            PointDataServerRequest request = new PointDataServerRequest(rcMap);

            Object result = ThriftClient.sendRequest(request);

            String[] params = ((ResponseMessageCatalog) result).getValues();

            return params;
        } catch (VizException e) {
            throw new VizException("Unable to retrieve point data catalog", e);
        }
    }

    /**
     * Request all levels for point data for a set of stations or points, over a
     * given set of parameters, at a specific point in time.
     * 
     * The request can be additionally constrained by optional
     * RequestConstraints.
     * 
     * @param dt
     *            the datatime to request the data for (required)
     * @param pluginName
     *            the plugin to use (required)
     * @param parameters
     *            the parameters to request (required)
     * @param stationIds
     *            the station IDs to constrain to (optional)
     * @param constraints
     *            additional constraints (optional)
     * @return
     */
    public static PointDataContainer requestPointDataAtLevel(DataTime dt,
            String pluginName, String[] parameters, String[] stationIds,
            Map<String, RequestConstraint> constraints, String levelParameter,
            double[] levelValues) throws VizException {
        return requestPointDataInternal(new DataTime[] { dt }, null,
                pluginName, parameters, stationIds, constraints, false,
                levelParameter, levelValues);
    }

    private static PointDataContainer requestPointDataInternal(DataTime[] dt,
            TimeRange tr, String pluginName, String[] parameters,
            String[] stationIds, Map<String, RequestConstraint> constraints,
            boolean allLevels, String levelParameter, double[] levelValues)
            throws VizException {
        try {
            Validate.notNull(pluginName, "Plugin Name is required");
            Validate.notNull(parameters,
                    "A parameter array is required (may be empty set)");
            Map<String, RequestConstraint> rcMap = new HashMap<String, RequestConstraint>();
            StringBuffer sb = new StringBuffer();
            boolean first = true;
            for (String s : parameters) {
                if (!first) {
                    sb.append(",");
                }
                sb.append(s);
                first = false;
            }

            rcMap.put(REQUESTED_PARAMETERS_KEY,
                    new RequestConstraint(sb.toString()));

            if (dt != null && dt.length > 0) {
                sb = new StringBuffer();
                first = true;
                for (DataTime d : dt) {
                    if (!first) {
                        sb.append(",");
                    }
                    sb.append(d.toString());
                    first = false;
                }
                RequestConstraint dtConstraint = new RequestConstraint(
                        sb.toString());
                dtConstraint.setConstraintType(ConstraintType.IN);
                rcMap.put(DATATIME_KEY, dtConstraint);
            } else if (tr != null) {
                DataTime start = new DataTime(tr.getStart());
                DataTime end = new DataTime(tr.getEnd());

                RequestConstraint dtConstraint = new RequestConstraint();
                String[] constraintList = { start.toString(), end.toString() };
                dtConstraint.setBetweenValueList(constraintList);
                dtConstraint.setConstraintType(ConstraintType.BETWEEN);
                rcMap.put(DATATIME_KEY, dtConstraint);
            }

            if (stationIds != null) {
                RequestConstraint rc = new RequestConstraint();
                rc.setConstraintValueList(stationIds);

                rcMap.put(STATIONID_KEY, rc);
            }

            rcMap.put(PLUGINNAME_KEY, new RequestConstraint(pluginName));
            rcMap.putAll(constraints);
            String mode = "select";
            if (allLevels) {
                mode = "select2d";
            } else if (levelParameter != null) {
                mode = "selectSpecific";
                rcMap.put(RESTRICT_PARAMETER, new RequestConstraint(
                        levelParameter));
                StringBuffer sb1 = new StringBuffer();
                for (int i = 0; i < levelValues.length; i++) {
                    if (i != 0) {
                        sb1.append(",");
                    }
                    sb1.append(levelValues[i]);
                }
                rcMap.put(RESTRICT_LEVEL, new RequestConstraint(sb1.toString()));
            }

            rcMap.put("mode", new RequestConstraint(mode));

            /*
             * String script = ScriptCreator.createScript(PLUGIN, rcMap, 9999,
             * mode); long t0 = System.currentTimeMillis();
             * 
             * Object[] result = Connector.getInstance().connect(script, null,
             * 60000);
             */

            PointDataServerRequest request = new PointDataServerRequest(rcMap);

            Object result = ThriftClient.sendRequest(request);

            PointDataContainer pdc = (PointDataContainer) result;
            if (pdc != null) {
                pdc.setCurrentSz(pdc.getAllocatedSz());
            }
            return pdc;
        } catch (VizException e) {
            throw new VizException("Unable to retrieve point data", e);
        }
    }
}
