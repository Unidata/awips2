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

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.parameter.Parameter;
import com.raytheon.uf.common.parameter.ParameterList;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ParameterLookup {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ParameterLookup.class);

    private static final ParameterLookup instance = new ParameterLookup();

    public static ParameterLookup getInstance() {
        return instance;
    }

    // Maps abbreviation to parameter, this is a mirror of what is in the
    // database.
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
            // do not rethrow, the lookup is not broken at this point so if the
            // problems persist then more exceptions will come from the actual
            // lookup methods themselves.
            statusHandler.handle(Priority.PROBLEM,
                    "Error occurred retrieving parameters from server.", e);
        }
        Unmarshaller unmarshaller = null;
        try {
            JAXBContext context = JAXBContext.newInstance(ParameterList.class);
            unmarshaller = context.createUnmarshaller();
        } catch (JAXBException e) {
            statusHandler
                    .error("Error creating Context for parameter defintions, no parameter defintions will be used.",
                            e);
        }
        if (unmarshaller != null) {
            IPathManager pathMgr = PathManagerFactory.getPathManager();

            LocalizationFile[] files = pathMgr.listStaticFiles("parameter"
                    + IPathManager.SEPARATOR + "definition",
                    new String[] { ".xml" }, true, true);

            for (LocalizationFile file : files) {
                if (file == null || !file.exists()
                        || file.getFile().length() < 0) {
                    continue;
                }
                Object obj = null;
                try {
                    obj = unmarshaller.unmarshal(file.getFile());
                } catch (JAXBException e) {
                    statusHandler.error("Error reading parameter defintions: "
                            + file.getName() + " has been ignored.", e);
                    continue;
                }
                if (obj instanceof ParameterList) {
                    ParameterList list = (ParameterList) obj;
                    if (list.getParameters() != null) {
                        for (Parameter p : list.getParameters()) {
                            getParameter(p, true);
                        }
                    }
                } else {
                    statusHandler.error("Error reading parameter definitions: "
                            + file.getName() + " was a "
                            + obj.getClass().getSimpleName()
                            + " but was expecting "
                            + ParameterList.class.getSimpleName());
                }
            }
        }
    }

    private void initializeMaps() {
        abbrevToParam = Collections
                .synchronizedMap(new HashMap<String, Parameter>());
    }

    public Parameter getParameter(String abbreviation)
            throws ParameterLookupException {
        Parameter result = abbrevToParam.get(abbreviation);
        if (result != null) {
            return result;
        }
        HashMap<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
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
        Map<String, Parameter> result = new HashMap<String, Parameter>(
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
        HashMap<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
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
