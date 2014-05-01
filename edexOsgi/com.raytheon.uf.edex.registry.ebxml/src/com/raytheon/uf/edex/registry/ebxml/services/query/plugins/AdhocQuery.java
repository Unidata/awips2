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
package com.raytheon.uf.edex.registry.ebxml.services.query.plugins;

import java.util.Map;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * The canonical query AdhocQuery allows clients to invoke a client-specified ad
 * hoc query in a client-specified query expression syntax that is supported by
 * the server. This specification does not require a server to support any
 * specific query expression syntax. It is likely that servers may support one
 * or more common syntaxes such as SQL-92, XQuery, XPath, SPARQL, Search-WS, OGC
 * Filter etc.
 * <p>
 * <b>Parameter Summary:</b> <br>
 * <b><i>queryExpression</i></b> -- Value is a query expression string in the
 * language specified by the
 * <p>
 * <b><i>queryLanguage</i></b> -- Value is the id of a ClassificationNode within
 * the canonical QueryLanguageScheme ClassificationScheme.
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
 * 4/9/2013     1802       bphillip    Changed abstract method signature, modified return processing, and changed static variables
 * Jun 24, 2013 2106       djohnson    Requires a transaction to be open, will not create one.
 * 10/8/2013    1682       bphillip    Refactored querying
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class AdhocQuery extends RegistryQueryPlugin {

    /** Data access object for registry objects */
    private RegistryObjectDao registryObjectDao;

    @Override
    @WebMethod(action = EXECUTE_QUERY_ACTION)
    @WebResult(name = "QueryResponse", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryResponse")
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public QueryResponse executeQuery(
            @WebParam(name = "QueryRequest", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryRequest") QueryRequest queryRequest)
            throws MsgRegistryException {
        QueryType queryType = queryRequest.getQuery();
        String queryLanguage = queryType
                .getSlotValue(QueryConstants.QUERY_LANGUAGE);
        String queryExpression = queryType
                .getSlotValue(QueryConstants.QUERY_EXPRESSION);

        Map<String, Object> queryParams = queryType.getSlotNameValues();
        queryParams.remove(QueryConstants.QUERY_EXPRESSION);
        queryParams.remove(QueryConstants.QUERY_LANGUAGE);
        Object[] parameters = new Object[queryParams.size() * 2];
        int index = 0;
        for (String key : queryParams.keySet()) {
            parameters[index++] = key;
            parameters[index++] = queryParams.get(key);
        }

        if (registryObjectDao.getById(queryLanguage) == null) {
            throw EbxmlExceptionUtil.createQueryExceptionType(
                    "AdhocQuery does not support the following query language: ["
                            + queryLanguage + "]", "");

        }

        if (parameters.length == 0) {
            return createResponse(registryObjectDao
                    .executeHQLQuery(queryExpression));

        } else {
            return createResponse(registryObjectDao.executeHQLQuery(
                    queryExpression, parameters));
        }

    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

}
