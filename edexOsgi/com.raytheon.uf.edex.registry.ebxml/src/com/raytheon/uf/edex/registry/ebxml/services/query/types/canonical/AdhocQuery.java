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
package com.raytheon.uf.edex.registry.ebxml.services.query.types.canonical;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.services.query.adhoc.AdhocQueryExpression;
import com.raytheon.uf.edex.registry.ebxml.services.query.adhoc.AdhocQueryExpressionManager;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.CanonicalEbxmlQuery;

/**
 * The canonical query AdhocQuery allows clients to invoke a client-specified ad
 * hoc query in a client-specified query expression syntax that is supported by
 * the server. This specification does not require a server to support any
 * specific query expression syntax. It is likely that servers may support one
 * or more common syntaxes such as SQL-92, XQuery, XPath, SPARQL, Search-WS, OGC
 * Filter etc.
 * <p>
 * <b>Parameter Summary:</b> <br>
 * · <b><i>queryExpression</i></b> -- Value is a query expression string in the
 * language specified by the
 * <p>
 * · <b><i>queryLanguage</i></b> -- Value is the id of a ClassificationNode
 * within the canonical QueryLanguageScheme ClassificationScheme.
 * 
 * queryLanguage parameter
 * <p>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class AdhocQuery extends CanonicalEbxmlQuery {

    /** The list of valid parameters for this query */
    private static final List<String> QUERY_PARAMETERS = new ArrayList<String>();

    /* Initializes the list of parameters */
    static {
        QUERY_PARAMETERS.add(QueryConstants.QUERY_EXPRESSION);
        QUERY_PARAMETERS.add(QueryConstants.QUERY_LANGUAGE);
    }

    public static final String QUERY_DEFINITION = QUERY_CANONICAL_PREFIX
            + "AdhocQuery";

    @Override
    protected <T extends RegistryObjectType> List<T> query(QueryType queryType,
            QueryResponse queryResponse) throws EbxmlRegistryException {
        RegistryObjectTypeDao registryObjectDao = new RegistryObjectTypeDao();
        QueryParameters parameters = getParameterMap(queryType.getSlot(),
                queryResponse, true);
        // The client did not specify the required parameter
        if (!parameters.containsParameter(QueryConstants.QUERY_EXPRESSION)) {
            throw new EbxmlRegistryException("Canonical query ["
                    + this.getQueryDefinition()
                    + "] is missing required parameter ["
                    + QueryConstants.QUERY_EXPRESSION + "]");
        }
        if (!parameters.containsParameter(QueryConstants.QUERY_LANGUAGE)) {
            throw new EbxmlRegistryException("Canonical query ["
                    + this.getQueryDefinition()
                    + "] is missing required parameter ["
                    + QueryConstants.QUERY_LANGUAGE + "]");
        }

        String queryExpression = null;
        String queryExpressionSlot = parameters
                .getFirstParameter(QueryConstants.QUERY_EXPRESSION);
        String queryLanguage = parameters
                .getFirstParameter(QueryConstants.QUERY_LANGUAGE);

        if (!queryLanguage.equals("HQL")) {
            throw new EbxmlRegistryException(
                    "Adhoc query does not support the [" + queryLanguage
                            + "] query language!");
        }

        AdhocQueryExpression expression = AdhocQueryExpressionManager
                .getInstance().getAdhocQueryExpression(queryExpressionSlot);
        Map<String, Object> paramMap = null;
        if (queryExpressionSlot.equals("SlotQuery")) {
            queryLanguage = "HQL";
            paramMap = new HashMap<String, Object>();
            queryExpression = getSlotQuery(queryType.getSlot(), paramMap);
            return registryObjectDao.executeHQLQuery(queryExpression, true,
                    paramMap);
        } else if (expression == null) {
            paramMap = getQueryParams(queryType.getSlot());
            queryExpression = queryExpressionSlot;
            return filterResults(registryObjectDao.executeHQLQuery(
                    queryExpression, true, paramMap));
        } else {
            queryLanguage = "HQL";
            paramMap = getQueryParams(queryType.getSlot());
            queryExpression = expression.getQueryExpression();
            return filterResults(registryObjectDao.executeHQLQuery(
                    queryExpression, true, paramMap));
        }
    }

    /**
     * Populates a query with the given slot values
     * 
     * @param queryExpression
     *            The prepared query
     * @param slots
     *            The slot values to substitute
     * @return The prepared query
     */
    private Map<String, Object> getQueryParams(Collection<SlotType> slots) {
        Map<String, Object> params = new HashMap<String, Object>();
        String slotName = null;
        String slotValue = null;

        for (SlotType slot : slots) {
            slotName = slot.getName();
            slotValue = slot.getSlotValue().getValue();
            if (!QUERY_PARAMETERS.contains(slotName)) {
                params.put(slotName, slotValue);
            }
        }
        return params;
    }

    /**
     * Populates the slot query
     * 
     * @param slots
     *            The slots containing values to substitute in
     * @return The prepared query
     */
    private String getSlotQuery(Collection<SlotType> slots,
            Map<String, Object> params) {
        String slotName = null;
        String slotValue = null;
        String operand = null;
        boolean firstClause = true;
        StringBuffer hqlQuery = new StringBuffer();
        hqlQuery.append("select obj from ")
                .append(RegistryObjectType.class.getName()).append(" as obj ");

        int i = 0;
        for (SlotType slot : slots) {
            if (!QUERY_PARAMETERS.contains(slot.getName())) {
                hqlQuery.append(" inner join obj.slot as slot").append(i++)
                        .append(" ");
            }
        }

        hqlQuery.append(" where ");

        i = 0;
        for (SlotType slot : slots) {
            slotName = slot.getName();
            slotValue = slot.getSlotValue().getValue();
            operand = null;
            if (!QUERY_PARAMETERS.contains(slotName)) {
                if (!firstClause) {
                    hqlQuery.append(" AND ");
                }
                Set<SlotType> childSlots = slot.getSlot();
                if (childSlots != null && !childSlots.isEmpty()) {
                    // should this loop over all children checking for an
                    // operand child?
                    SlotType operandSlot = childSlots
                            .toArray(new SlotType[childSlots.size()])[0];
                    if (operandSlot.getName().equals("operand")) {
                        operand = operandSlot.getSlotValue().getValue();
                    }
                }
                if (operand == null) {
                    operand = "=";
                }
                String paramHolder = "slotValue_" + i;
                params.put(paramHolder, slotValue);
                hqlQuery.append(" (slot").append(i).append(".name='")
                        .append(slotName).append("' AND slot").append(i)
                        .append(".slotValue.")
                        .append(slot.getSlotValue().getColumnName())
                        .append(" ").append(operand).append(" :")
                        .append(paramHolder).append(") ");
                i++;
                firstClause = false;
            }
        }
        return hqlQuery.toString();
    }

    @Override
    protected List<String> getValidParameters() {
        return QUERY_PARAMETERS;
    }

    @Override
    public String getQueryDefinition() {
        return QUERY_DEFINITION;
    }

}
