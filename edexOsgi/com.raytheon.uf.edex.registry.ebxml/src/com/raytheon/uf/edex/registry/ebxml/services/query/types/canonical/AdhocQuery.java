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
import java.util.List;
import java.util.Set;

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl.RETURN_TYPE;
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
 * 3/18/2013    1802       bphillip    Modified to use transaction boundaries and spring dao injection
 * 4/9/2013     1802       bphillip     Changed abstract method signature, modified return processing, and changed static variables
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Service
@Transactional
public class AdhocQuery extends CanonicalEbxmlQuery {

    /** The list of valid parameters for this query */
    private static final List<String> QUERY_PARAMETERS = new ArrayList<String>();

    /* Initializes the list of parameters */
    static {
        QUERY_PARAMETERS.add(QueryConstants.QUERY_EXPRESSION);
        QUERY_PARAMETERS.add(QueryConstants.QUERY_LANGUAGE);
    }

    @Override
    protected void query(QueryType queryType, QueryResponse queryResponse)
            throws EbxmlRegistryException {
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
        List<Object> results = null;
        if (queryExpressionSlot.equals("SlotQuery")) {
            queryLanguage = "HQL";
            Object[] params = new Object[queryType.getSlot().size() * 2];
            queryExpression = getSlotQuery(queryType.getSlot(), params);
            results = registryObjectDao.executeHQLQuery(queryExpression,
                    maxResults, params);
        } else if (expression == null) {
            Object[] params = getQueryParams(queryType.getSlot());
            queryExpression = queryExpressionSlot;
            results = registryObjectDao.executeHQLQuery(queryExpression,
                    maxResults, params);
        } else {
            queryLanguage = "HQL";
            Object[] params = getQueryParams(queryType.getSlot());
            queryExpression = expression.getQueryExpression();
            results = registryObjectDao.executeHQLQuery(queryExpression,
                    maxResults, params);
        }
        setResponsePayload(queryResponse, results);

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
    private Object[] getQueryParams(Collection<SlotType> slots) {
        List<Object> params = new ArrayList<Object>();
        String slotName = null;
        String slotValue = null;
        for (SlotType slot : slots) {
            slotName = slot.getName();
            slotValue = slot.getSlotValue().getValue();
            if (!QUERY_PARAMETERS.contains(slotName)) {
                params.add(slotName);
                params.add(slotValue);
            }
        }
        return params.toArray(new Object[params.size()]);
    }

    /**
     * Populates the slot query
     * 
     * @param slots
     *            The slots containing values to substitute in
     * @return The prepared query
     */
    private String getSlotQuery(Collection<SlotType> slots, Object[] params) {
        String slotName = null;
        String slotValue = null;
        String operand = null;
        boolean firstClause = true;
        StringBuffer hqlQuery = new StringBuffer();
        if (returnType.equals(RETURN_TYPE.ObjectRef)) {
            hqlQuery.append("select obj.id from ");
        } else {
            hqlQuery.append("select obj from ");
        }
        hqlQuery.append(RegistryObjectType.class.getName()).append(" as obj ");

        int i = 0;
        for (SlotType slot : slots) {
            if (!QUERY_PARAMETERS.contains(slot.getName())) {
                hqlQuery.append(" inner join obj.slot as slot").append(i++)
                        .append(" ");
            }
        }

        hqlQuery.append(" where ");

        int paramIndex = 0;
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
                params[paramIndex++] = paramHolder;
                params[paramIndex++] = slotValue;
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
        return CanonicalQueryTypes.ADHOC_QUERY;
    }

}
