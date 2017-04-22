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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.InternationalStringType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.NodeTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.plugins.BasicQuery;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.QueryTest;

/**
 * 
 * Test for the Canonical BasicQuery defined by the EBXML 4.0 spec
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
public class BasicQueryTest extends QueryTest {

    private static final String TEST_ID = "Basic Query Test Object ";

    private static final String TEST_OBJECT_TYPE = RegistryObjectTypes.REGISTRY_OBJECT;

    private static final String TEST_OWNER = TEST_ID + " Owner";

    private static final String TEST_STATUS = StatusTypes.REJECTED;

    @Autowired
    private BasicQuery basicQuery;

    @Before
    public void insertTestObjects() throws MsgRegistryException {

        ClassificationType testClassification1 = new ClassificationType();
        testClassification1.setId("Test Classification");
        testClassification1.setLid("Test Classification");
        testClassification1.setObjectType(RegistryObjectTypes.CLASSIFICATION);
        testClassification1.setClassificationNode(TEST_OBJECT_TYPE);

        ClassificationType testClassification2 = new ClassificationType();
        testClassification2.setId("Test Classification 2");
        testClassification2.setLid("Test Classification 2");
        testClassification2.setObjectType(RegistryObjectTypes.CLASSIFICATION);
        testClassification2.setClassificationNode(NodeTypes.UNIQUE_CODE);

        for (int i = 1; i <= 12; i++) {
            RegistryObjectType obj = new RegistryObjectType(TEST_ID + i,
                    TEST_ID + i);
            obj.setDescription(new InternationalStringType(TEST_ID
                    + " Description " + i));
            obj.setName(new InternationalStringType(TEST_ID + " Name " + i));
            obj.setObjectType(TEST_OBJECT_TYPE);
            obj.setStatus(TEST_STATUS);
            obj.setOwner(TEST_OWNER);
            if (i % 2 == 0) {
                obj.getClassification().add(testClassification1);
            }
            if (i % 3 == 0) {
                obj.getClassification().add(testClassification2);
            }

            submitRegistryObjectToRegistry(obj);
        }
    }

    @Test
    public void queryOnDescription() throws MsgRegistryException {
        QueryRequest query = createQuery(CanonicalQueryTypes.BASIC_QUERY,
                "description", TEST_ID + " Description 1");
        List<RegistryObjectType> result = executeQuery(basicQuery, query);
        assertTrue(result.size() == 1);

    }

    @Test
    public void queryOnName() throws MsgRegistryException {
        QueryRequest query = createQuery(CanonicalQueryTypes.BASIC_QUERY,
                "name", TEST_ID + " Name 1");
        List<RegistryObjectType> result = executeQuery(basicQuery, query);
        assertTrue(result.size() == 1);
    }

    @Test
    public void queryOnObjectType() throws MsgRegistryException {
        QueryRequest query = createQuery(
                CanonicalQueryTypes.BASIC_QUERY,
                "objectType",
                "/urn:oasis:names:tc:ebxml-regrep:classificationScheme:ObjectType/RegistryObject");
        List<RegistryObjectType> result = executeQuery(basicQuery, query);
        assertTrue(result.size() == 12);
    }

    @Test
    public void queryOnOwner() throws MsgRegistryException {
        QueryRequest query = createQuery(CanonicalQueryTypes.BASIC_QUERY,
                "owner", TEST_OWNER);
        List<RegistryObjectType> result = executeQuery(basicQuery, query);
        assertTrue(result.size() == 12);
    }

    @Test
    public void queryOnStatus() throws MsgRegistryException {
        QueryRequest query = createQuery(CanonicalQueryTypes.BASIC_QUERY,
                "status", "/Rejected");
        List<RegistryObjectType> result = executeQuery(basicQuery, query);
        assertTrue(result.size() == 12);
    }

    @Test
    public void queryOnClassification() throws MsgRegistryException {
        QueryRequest query = createQuery(CanonicalQueryTypes.BASIC_QUERY,
                QueryConstants.CLASSIFICATIONS,
                "/urn:oasis:names:tc:ebxml-regrep:classificationScheme:NodeType/UniqueCode");
        List<RegistryObjectType> result = executeQuery(basicQuery, query);
        assertTrue(result.size() == 4);

        QueryRequest query2 = createQuery(
                CanonicalQueryTypes.BASIC_QUERY,
                QueryConstants.CLASSIFICATIONS,
                "/urn:oasis:names:tc:ebxml-regrep:classificationScheme:NodeType/UniqueCode",
                QueryConstants.CLASSIFICATIONS,
                "/urn:oasis:names:tc:ebxml-regrep:classificationScheme:ObjectType/RegistryObject");
        List<RegistryObjectType> result2 = executeQuery(basicQuery, query2);
        assertEquals(2, result2.size());
    }

    @Test
    public void queryUsingMatchOnAny() throws MsgRegistryException {
        QueryRequest query = createQuery(CanonicalQueryTypes.BASIC_QUERY,
                QueryConstants.DESCRIPTION, TEST_ID + " Description 1",
                QueryConstants.NAME, TEST_ID + " Name 1",
                QueryConstants.MATCH_ANY, false);
        List<RegistryObjectType> result = executeQuery(basicQuery, query);
        assertTrue(result.size() == 1);

        QueryRequest query2 = createQuery(CanonicalQueryTypes.BASIC_QUERY,
                QueryConstants.DESCRIPTION, TEST_ID + " Description 1",
                QueryConstants.NAME, TEST_ID + " Name 2",
                QueryConstants.MATCH_ANY, true);
        List<RegistryObjectType> result2 = executeQuery(basicQuery, query2);
        assertTrue(result2.size() == 2);
    }
}
