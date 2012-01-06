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

package com.raytheon.uf.edex.pointdata;

import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.pointdata.PointDataContainer;
import com.raytheon.uf.common.pointdata.PointDataServerRequest;
import com.raytheon.uf.common.pointdata.PointDataThriftContainer;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;

/**
 * Handler for PointDataServerRequest
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2011 #8070      ekladstrup  Initial creation
 * Aug 09, 2011 #9696      gzhou       add handle for request from nativeLib
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */
public class PointDataServerRequestHandler implements
        IRequestHandler<PointDataServerRequest> {

    @Override
    public Object handleRequest(PointDataServerRequest request)
            throws Exception {
        // grab constraint map
        Map<String, RequestConstraint> map = request.getRcMap();

        // find pluginName
        String pluginName = null;
        RequestConstraint constraint = map.get("pluginName");
        if (constraint != null) {
            pluginName = constraint.getConstraintValue();
        } else {
            throw new Exception(
                    "The required constraint parameter \"pluginName\" not defined.");
        }
        // String pluginName = map.get("pluginName").getConstraintValue();
        map.remove("pluginName");

        // check if requesting specific levels only
        String levelParameter = null;
        String levelValues = null;
        if (map.containsKey("restrictParameter")) {
            levelParameter = map.get("restrictParameter").getConstraintValue();
            levelValues = map.get("restrictLevel").getConstraintValue();
            map.remove("restrictParameter");
            map.remove("restrictLevel");
        }

        // get mode
        String mode = "";
        constraint = map.get("mode");
        if (constraint != null) {
            mode = constraint.getConstraintValue();
            map.remove("mode");
        }

        // create PointDataQuery
        PointDataQuery query = new PointDataQuery(pluginName);

        // find requested parameters if applicable
        if (map.containsKey("requestedParameters")) {
            String params = map.get("requestedParameters").getConstraintValue();
            map.remove("requestedParameters");
            query.setParameters(params);
        }

        // add all remaining constraints
        for (String key : map.keySet()) {
            RequestConstraint rc = map.get(key);
            String value = rc.getConstraintValue();
            String type = constraintTypeToString(rc.getConstraintType());
            query.addParameter(key, value, type);
        }

        // check if requestAllLevels should be called or only specific levels
        if (mode.equals("select2d")) {
            query.requestAllLevels();
        } else if (mode.equals("selectSpecific")) {
            query.requestSpecificLevel(levelParameter, levelValues);
        }

        // perform action based on mode
        PointDataContainer container = null;
        if (mode.equals("getParameters")) {
            return query.getAvailableParameters();
        } else {
            container = query.execute();
        }

        if (mode.equalsIgnoreCase("convertToThrift")) {
            return PointDataThriftContainer.from(container);
        } else {
            return container;
        }
    }

    /**
     * converts a ConstraintType enum into a string
     * 
     * @param type
     * @return
     */
    private String constraintTypeToString(ConstraintType type) {
        String rval;

        // EQUALS, NOT_EQUALS, GREATER_THAN, GREATER_THAN_EQUALS, LESS_THAN,
        // LESS_THAN_EQUALS, BETWEEN, IN, LIKE, ILIKE, ISNULL
        if (type == ConstraintType.EQUALS) {
            rval = "=";
        } else if (type == ConstraintType.NOT_EQUALS) {
            rval = "!=";
        } else if (type == ConstraintType.GREATER_THAN) {
            rval = ">";
        } else if (type == ConstraintType.GREATER_THAN_EQUALS) {
            rval = ">=";
        } else if (type == ConstraintType.LESS_THAN) {
            rval = "<";
        } else if (type == ConstraintType.LESS_THAN_EQUALS) {
            rval = "<=";
        } else if (type == ConstraintType.BETWEEN) {
            rval = "between";
        } else if (type == ConstraintType.IN) {
            rval = "in";
        } else if (type == ConstraintType.LIKE) {
            rval = "like";
        } else if (type == ConstraintType.ILIKE) {
            rval = "ilike";
        } else {
            rval = "=";
        }

        return rval;
    }

}
