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
package com.raytheon.uf.edex.registry.ebxml.services.rest;

import java.math.BigInteger;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ParameterType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryDefinitionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringQueryExpressionType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.services.query.RegistryQueryUtil;
import com.raytheon.uf.edex.registry.ebxml.services.query.RegistryQueryUtil.PARAMETER_DATA_TYPE;

/**
 * 
 * REST Services used to manage registry query definitions
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Nov 07, 2013  1678     bphillip  Initial Creation
 * May 11, 2015  4448     bphillip  Separated EBXML Registry from Data Delivery
 * Aug 25, 2016  5846     rjpeter   Remove InternationalString from DB
 * 
 * </pre>
 * 
 * @author bphillip
 **/
@Path(RegistryQueryDefinitionService.REGISTRY_DEFINE_QUERY_PATH)
@Transactional
public class RegistryQueryDefinitionService {

    /** The path to this set of services */
    protected static final String REGISTRY_DEFINE_QUERY_PATH = "/defineQuery/";

    /** Data access object for registry objects */
    private RegistryObjectDao registryObjectDao;

    /**
     * Gets the valid data types for query parameters
     * 
     * @return The valid parameter data types
     */
    @GET
    @Path("getParameterDataTypes")
    public String getParameterDataTypes() {
        return RegistryQueryUtil
                .formatArrayString(PARAMETER_DATA_TYPE.values());
    }

    /**
     * Gets the valid query languages
     * 
     * @return The valid query languages
     */
    @GET
    @Path("getQueryLanguages")
    public String getQueryLanguages() {
        return RegistryQueryUtil
                .formatArrayString(registryObjectDao
                        .executeHQLQuery(
                                "SELECT obj.code FROM ClassificationNodeType obj where id like 'urn:oasis:names:tc:ebxml-regrep:QueryLanguage:%'")
                        .toArray());
    }

    /**
     * Stores the query definition to the registry
     * 
     * @param params
     *            The query definition tokens
     * @return The status message
     */
    @GET
    @Path("storeQuery/{params}")
    public String storeQuery(@PathParam("params") String params) {
        StringBuilder response = new StringBuilder();
        QueryDefinitionType queryDef = new QueryDefinitionType();
        String[] tokens = params.split(",", -1);
        if (tokens.length < 4) {
            throw new IllegalArgumentException(
                    "Argument to storeQuery must contain at least 4 comma delimited tokens!");
        }
        for (int i = 0; i < tokens.length; i++) {
            tokens[i] = tokens[i].trim();
        }
        String queryName = tokens[0];
        String queryDescription = tokens[1];
        String queryLangCode = tokens[2];
        String queryExpression = tokens[3];
        queryDef.setId(queryName);
        queryDef.setLid(queryName);
        queryDef.setName(queryName);
        queryDef.setDescription(queryDescription);
        queryDef.setObjectType(RegistryObjectTypes.QUERY_DEFINITION);
        queryDef.setStatus(StatusTypes.APPROVED);
        if (!queryExpression.isEmpty()) {
            StringQueryExpressionType expr = new StringQueryExpressionType();
            String queryLanguage = (String) registryObjectDao
                    .executeHQLQuery(
                            "SELECT id FROM ClassificationNodeType obj where obj.code=:code",
                            "code", queryLangCode).get(0);
            expr.setQueryLanguage(queryLanguage);
            expr.setValue(queryExpression);
            queryDef.setQueryExpression(expr);
        }
        for (int i = 4; i < tokens.length && !tokens[4].isEmpty(); i += 6) {
            if ((i + 5) >= tokens.length) {
                throw new IllegalArgumentException(
                        "Incomplete parameter submitted to storeQuery!");
            }
            ParameterType param = new ParameterType();
            param.setParameterName(tokens[i]);
            param.setName(tokens[i]);
            if (!tokens[i + 1].isEmpty()) {
                param.setDescription(tokens[i + 1]);
            }
            if (!tokens[i + 2].isEmpty()) {
                param.setDataType(tokens[i + 2]);
            }
            if (!tokens[i + 3].isEmpty()) {
                param.setDefaultValue(tokens[i + 3]);
            }
            if (!tokens[i + 4].isEmpty()) {
                param.setMinOccurs(new BigInteger(tokens[i + 4]));
            }
            if (!tokens[i + 5].isEmpty()) {
                param.setMaxOccurs(new BigInteger(tokens[i + 5]));
            }
            queryDef.getParameter().add(param);
        }
        registryObjectDao.createOrUpdate(queryDef);

        response.append("Successfully created Query Definition: ").append(
                queryDef.getId());
        return response.toString();
    }

    /**
     * Deletes the selected query from the registry
     * 
     * @param queryId
     *            The ID of the query to delete from the registry
     * @return Status message whether the query was deleted
     */
    @GET
    @Path("deleteQuery/{queryId}")
    public String deleteQuery(@PathParam("queryId") String queryId) {

        if (queryId == null || queryId.isEmpty()) {
            return "A query id must be specified";
        }

        // Check to make sure we're not trying to delete a canonical query
        if (CanonicalQueryTypes.CANONICAL_QUERY_TYPES.contains(queryId)) {
            return "Cannot delete canonical query!";
        }
        RegistryObjectType queryObj = registryObjectDao.getById(queryId);
        if (queryObj == null) {
            return "Query Definition [" + queryId
                    + "] not present in registry!";
        }

        registryObjectDao.delete(queryObj);
        return "Successfully deleted query: " + queryId;
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

}
