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
package com.raytheon.uf.common.parameter.lookup;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.localization.ParameterLocalizationLookup;
import com.raytheon.uf.common.parameter.localization.ParameterLocalizationLookup.ParameterLocalizationListener;
import com.raytheon.uf.common.parameter.request.GetParameterRequest;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Caching and convenience for finding parameters in the Database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------------------
 * Mar 12, 2012           bsteffen  Initial creation
 * Oct 04, 2016  5890     bsteffen  Extract localization lookup to a new class
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class ParameterLookup {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParameterLookup.class);

    private static final ParameterLookup instance = new ParameterLookup();

    public static ParameterLookup getInstance() {
        return instance;
    }

    private ParameterLocalizationLookup localizationParameters = new ParameterLocalizationLookup();

    /**
     * Maps abbreviation to parameter, this is a mirror of what is in the
     * database.
     */
    private Map<String, Parameter> abbrevToParam;

    private ParameterLookup() {
        initializeMaps();
        DbQueryRequest query = new DbQueryRequest();
        query.setEntityClass(Parameter.class.getName());
        try {
            DbQueryResponse resp = (DbQueryResponse) RequestRouter.route(query);
            for (Map<String, Object> map : resp.getResults()) {
                Parameter param = (Parameter) map.get(null);
                abbrevToParam.put(param.getAbbreviation(), param);
            }
        } catch (Exception e) {
            /*
             * do not rethrow, the lookup is not broken at this point so if the
             * problems persist then more exceptions will come from the actual
             * lookup methods themselves.
             */
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred retrieving parameters from server.", e);
        }
        localizationParameters.addListener(new ParameterLocalizationListener() {

            @Override
            public void parametersChanged() {
                refreshParameterNames();
            }
        });
        refreshParameterNames();
    }

    private void initializeMaps() {
        abbrevToParam = Collections
                .synchronizedMap(new HashMap<String, Parameter>());
    }

    /**
     * Remove any parameters from the cache if the name does not match the name
     * in localization. This assumes that the parameter name in the database may
     * be changed if it doesn't match localization. This class does not actually
     * cause the database to change. The parameter will be reloaded from the
     * database next time it is needed.
     */
    private void refreshParameterNames() {
        for (Parameter parameter : localizationParameters.getAllParameters()) {
            Parameter cachedParameter = abbrevToParam
                    .get(parameter.getAbbreviation());
            if (cachedParameter != null
                    && !cachedParameter.getName().equals(parameter.getName())) {
                abbrevToParam.remove(parameter.getAbbreviation());
            }
        }
    }

    public Parameter getParameter(String abbreviation)
            throws ParameterLookupException {
        Parameter result = abbrevToParam.get(abbreviation);
        if (result != null) {
            return result;
        }
        Map<String, RequestConstraint> constraints = new HashMap<>();
        constraints.put("abbreviation", new RequestConstraint(abbreviation));
        DbQueryRequest query = new DbQueryRequest();
        query.setConstraints(constraints);
        query.setEntityClass(Parameter.class.getName());
        try {
            DbQueryResponse resp = (DbQueryResponse) RequestRouter.route(query);
            if (!resp.getResults().isEmpty()) {
                result = (Parameter) resp.getResults().get(0).get(null);
                if (result != null) {
                    abbrevToParam.put(result.getAbbreviation(), result);
                }
                return result;
            }
        } catch (Exception e) {
            throw new ParameterLookupException(
                    "Error occurred retrieving Parameter information from server.",
                    e);
        }
        return null;
    }

    /**
     * bulk request multiple parameters by abbreviation, for any parameters not
     * in the cache this can be significantly faster than requesting multiple
     * parameters individually.
     * 
     * @param ids
     * @return
     */
    public Map<String, Parameter> getParameters(List<String> abbreviations)
            throws ParameterLookupException {
        RequestConstraint abbreviationConstraint = new RequestConstraint(null,
                ConstraintType.IN);
        Map<String, Parameter> result = new HashMap<>(
                abbreviations.size());
        for (String abbreviation : abbreviations) {
            Parameter param = abbrevToParam.get(abbreviation);
            if (param == null) {
                abbreviationConstraint.addToConstraintValueList(abbreviation);
            }
            result.put(abbreviation, param);
        }
        if (abbreviationConstraint.getConstraintValue() == null) {
            // everything was a cache hit.
            return result;
        }
        Map<String, RequestConstraint> constraints = new HashMap<>();
        constraints.put("abbreviation", abbreviationConstraint);
        DbQueryRequest query = new DbQueryRequest();
        query.setConstraints(constraints);
        query.setEntityClass(Parameter.class.getName());
        try {
            DbQueryResponse resp = (DbQueryResponse) RequestRouter.route(query);
            for (Map<String, Object> thing : resp.getResults()) {
                Parameter respParam = (Parameter) thing.get(null);
                abbrevToParam.put(respParam.getAbbreviation(), respParam);
                result.put(respParam.getAbbreviation(), respParam);
            }
        } catch (Exception e) {
            throw new ParameterLookupException(
                    "Error occurred retrieving Parameter information from server.",
                    e);
        }
        return result;
    }

    public Parameter getParameter(Parameter parameter, boolean create)
            throws ParameterLookupException {
        Parameter p = abbrevToParam.get(parameter.getAbbreviation());
        if (p != null) {
            return p;
        }
        try {
            Parameter result = (Parameter) RequestRouter
                    .route(new GetParameterRequest(parameter, create));
            if (result != null) {
                abbrevToParam.put(result.getAbbreviation(), result);
            }
            return result;
        } catch (Exception e) {
            throw new ParameterLookupException(
                    "Error occurred retrieving Parameter information from server.",
                    e);
        }
    }

}
