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

import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ParameterType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.services.rest.IRegistryDataAccessService;
import com.raytheon.uf.common.registry.services.rest.response.RestCollectionResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.QueryDefinitionDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.services.query.RegistryQueryUtil;

/**
 * 
 * Implementation of the registry data access service interface
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/29/2013    2191        bphillip    Initial implementation
 * 9/20/2013    2385        bphillip    Added subscription backup functions
 * 10/2/2013    2385        bphillip    Fixed subscription backup queries
 * 10/8/2013    1682        bphillip    Added query queries
 * 11/7/2013    1678        bphillip    Added getCustomQueries method
 * Mar 31, 2014 2889        dhladky     Added username for notification center tracking.
 * Apr 12,2014  3012       dhladky     Purge never worked, fixed to make work.
 * 5/11/2015    4448        bphillip    Separated EBXML Registry from Data Delivery
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Transactional
@Path(IRegistryDataAccessService.DATA_ACCESS_PATH_PREFIX)
public class RegistryDataAccessService implements IRegistryDataAccessService {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryDataAccessService.class);

    /** Data access object for registry objects */
    private RegistryObjectDao registryObjectDao;

    /** Data access object for query definitions */
    private QueryDefinitionDao queryDefinitionDao;

    public RegistryDataAccessService() {

    }

    /**
     * @see 
     *      com.raytheon.uf.common.registry.services.rest.IRegistryDataAccessService
     *      .getRegistryObjectIdsOfType(String)
     */
    @Override
    @GET
    @Path("getRegistryObjectIds/{objectType}")
    public RestCollectionResponse<String> getRegistryObjectIdsOfType(
            @PathParam("objectType")
            String objectType) {
        statusHandler.info("Getting registry object ids of type [" + objectType
                + "]...");
        RestCollectionResponse<String> response = new RestCollectionResponse<String>();
        response.setPayload(registryObjectDao
                .getRegistryObjectIdsOfType(objectType));
        return response;
    }

    @Override
    @GET
    @Path("getQueries")
    public String getValidQueries() {
        statusHandler.debug("Getting valid queries...");
        List<String> ids = queryDefinitionDao.getQueryIds();
        StringBuilder builder = new StringBuilder();
        for (String id : ids) {
            builder.append(id).append(StringUtil.NEWLINE);
        }
        return builder.toString();
    }

    @Override
    @GET
    @Path("getCustomQueries")
    public String getCustomQueries() {
        List<String> ids = queryDefinitionDao.getQueryIds();
        ids.removeAll(CanonicalQueryTypes.CANONICAL_QUERY_TYPES);
        return RegistryQueryUtil.formatArrayString(ids.toArray());
    }

    @Override
    @GET
    @Path("getParametersForQuery/{queryId}")
    public String getParametersForQuery(@PathParam("queryId")
    String queryId) {
        statusHandler.debug("Getting query parameters for query: " + queryId
                + "...");
        List<ParameterType> parameters = queryDefinitionDao
                .getParametersForQuery(queryId);
        StringBuilder retVal = new StringBuilder();
        for (ParameterType param : parameters) {
            retVal.append(param.getParameterName()).append(StringUtil.NEWLINE);
            retVal.append(param.getDataType()).append(StringUtil.NEWLINE);
            retVal.append(param.getDefaultValue()).append(StringUtil.NEWLINE);
        }
        return retVal.toString();
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    public void setQueryDefinitionDao(QueryDefinitionDao queryDefinitionDao) {
        this.queryDefinitionDao = queryDefinitionDao;
    }
}
