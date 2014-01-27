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
package com.raytheon.uf.edex.registry.ebxml.services.query.types;

import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.BooleanValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DateTimeValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import org.junit.Ignore;

import com.raytheon.uf.edex.registry.ebxml.dao.AbstractRegistryTest;

/**
 * 
 * Base test template for testing canonical registry queries
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/8/2013    1682        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Ignore
public class QueryTest extends AbstractRegistryTest {

    protected QueryRequest createQuery(String queryType, Object... params) {
        QueryType query = new QueryType();
        query.setQueryDefinition(queryType);
        for (int i = 0; i < params.length; i += 2) {
            if (params[i + 1] instanceof String) {
                query.getSlot().add(
                        new SlotType((String) params[i], new StringValueType(
                                (String) params[i + 1])));
            } else if (params[i + 1] instanceof Boolean) {
                query.getSlot().add(
                        new SlotType((String) params[i], new BooleanValueType(
                                (Boolean) params[i + 1])));
            } else if (params[i + 1] instanceof XMLGregorianCalendar) {
                query.getSlot().add(
                        new SlotType((String) params[i], new DateTimeValueType(
                                (XMLGregorianCalendar) params[i + 1])));
            }
        }

        QueryRequest request = new QueryRequest();
        request.setId("Query Request ID");
        request.setQuery(query);
        return request;
    }

    protected List<RegistryObjectType> executeQuery(QueryManager queryPlugin,
            QueryRequest request) throws MsgRegistryException {
        return queryPlugin.executeQuery(request).getRegistryObjects();
    }
}
