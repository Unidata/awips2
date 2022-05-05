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

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.QueryLanguages;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.plugins.AdhocQuery;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.QueryTest;

/**
 * 
 * Test for the Canonical AdhocQuery defined by the EBXML 4.0 spec
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
public class AdhocQueryTest extends QueryTest {

    /** The adhoc query object */
    @Autowired
    private AdhocQuery adhocQuery;

    @Before
    public void insertTestObjects() throws MsgRegistryException {
        for (int i = 0; i < 11; i++) {
            RegistryObjectType obj = new RegistryObjectType("Test Object " + i,
                    "");
            submitRegistryObjectToRegistry(obj);
        }
    }

    @Test
    public void adhocQueryWithNoParameters() throws MsgRegistryException {
        QueryRequest request = createQuery(CanonicalQueryTypes.ADHOC_QUERY,
                QueryConstants.QUERY_LANGUAGE, QueryLanguages.HQL,
                QueryConstants.QUERY_EXPRESSION,
                "SELECT obj FROM RegistryObjectType obj WHERE obj.id like 'Test Object %'");
        List<RegistryObjectType> result = executeQuery(adhocQuery, request);
        assertTrue(result.size() == 11);
    }

    @Test
    public void adhocQueryWithParameters() throws MsgRegistryException {
        QueryRequest request = createQuery(CanonicalQueryTypes.ADHOC_QUERY,
                QueryConstants.QUERY_LANGUAGE, QueryLanguages.HQL,
                QueryConstants.QUERY_EXPRESSION,
                "SELECT obj FROM RegistryObjectType obj WHERE obj.id like :id",
                "id", "Test Object 5");
        List<RegistryObjectType> result = executeQuery(adhocQuery, request);
        assertTrue(result.size() == 1);
    }

    @Test
    public void adhocQueryWithUnsupportedQueryLanguage() {
        QueryRequest request = createQuery(CanonicalQueryTypes.ADHOC_QUERY,
                QueryConstants.QUERY_LANGUAGE, "Unsupported Language",
                QueryConstants.QUERY_EXPRESSION,
                "SELECT obj FROM RegistryObjectType obj WHERE obj.id like 'Test Object %'");
        try {
            executeQuery(adhocQuery, request);
        } catch (MsgRegistryException e) {
            assertTrue(e
                    .getMessage()
                    .startsWith(
                            "AdhocQuery does not support the following query language:"));
            return;
        }
        fail();
    }

}
