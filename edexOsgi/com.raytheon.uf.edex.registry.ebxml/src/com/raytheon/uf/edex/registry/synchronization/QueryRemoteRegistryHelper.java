/* This software was developed and / or modified by Raytheon Company,

 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 *
 * Contractor Name:        Raytheon Company
 * Contractor Address:     
 *                           8401 Colesville Road Suite 800
 *                         Silver Spring, MD  20910
 *
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.uf.edex.registry.synchronization;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.QueryLanguages;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

/**
 * A Helper class used to perform query to a remote registry according to
 * specific pre-defined value.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#   Engineer    Description
 * ------------- -------- --------- -----------------------
 * 08-30-2018    7238      skabasele   Initial creation
 *
 * </pre>
 *
 * @author skabasele
 */

public class QueryRemoteRegistryHelper {

    private final SlotType queryLanguageSlot;

    private final SlotType queryExpressionSlot;

    private final QueryRequest queryRequest;

    private final QueryType query;

    private final QueryManager queryManager;

    private final String objectType;

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    /** Query used for synchronizing registries */
    private static final String SYNC_QUERY = "FROM RegistryObjectType obj where obj.id in (%s) order by obj.id asc";

    public QueryRemoteRegistryHelper(final String objectType,
            final QueryManager queryManager) {
        // default values
        this.queryManager = queryManager;
        this.objectType = objectType;
        this.queryLanguageSlot = new SlotType(QueryConstants.QUERY_LANGUAGE,
                new StringValueType(QueryLanguages.HQL));
        this.queryExpressionSlot = new SlotType(QueryConstants.QUERY_EXPRESSION,
                new StringValueType(""));
        this.queryRequest = new QueryRequest();
        this.query = new QueryType();
        this.query.setQueryDefinition(CanonicalQueryTypes.ADHOC_QUERY);
        this.query.getSlot().add(queryLanguageSlot);
        this.query.getSlot().add(queryExpressionSlot);
        this.queryRequest.setQuery(query);
        this.queryRequest.setResponseOption(
                new ResponseOptionType(QueryReturnTypes.REGISTRY_OBJECT, true));
        this.queryRequest
                .setId("Synchronizing object type: " + this.objectType);

    }

    /**
     * Helper method used to build a query string from the provided list
     * 
     * @param regObjectIdList
     * @return
     */
    private StringBuilder buildQueryString(final List<String> regObjectIdList) {
        final StringBuilder builder = new StringBuilder();

        for (int i = 0; i < regObjectIdList.size(); i++) {
            if (i != 0) {
                builder.append(',');
            }

            builder.append('\'').append(regObjectIdList.get(i)).append('\'');
        }

        return builder;
    }

    /**
     * Method used to create a query response for the specified list of objects
     * ids.
     * 
     * @param regObjectIdList
     * @return
     */
    public synchronized QueryResponse getQueryResponse(
            final List<String> regObjectIdList) {

        final StringBuilder builder = buildQueryString(regObjectIdList);
        final StringValueType queryValue = new StringValueType(
                String.format(SYNC_QUERY, builder.toString()));
        queryExpressionSlot.setSlotValue(queryValue);

        // query the registry
        QueryResponse queryResponse = null;
        try {
            queryResponse = queryManager.executeQuery(queryRequest);
        } catch (MsgRegistryException e) {
            logger.error("Query to registry for batch has failed.", e);
            logger.error(" The error occured for the following query string [ "
                    + queryValue + " ]");
        }
        return queryResponse;
    }
}
