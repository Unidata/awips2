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

import java.util.ArrayList;
import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.raytheon.uf.common.registry.constants.AssociationTypes;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.plugins.FindAssociatedObjects;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.QueryTest;

/**
 * 
 * Test for the Canonical FindAssociatedObjects query defined by the EBXML 4.0
 * spec
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
public class FindAssociatedObjectsTest extends QueryTest {

    @Autowired
    private FindAssociatedObjects findAssociatedObjects;

    private static final String REGISTRY_OBJECT_TYPE_1 = "urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject";

    private static final String REGISTRY_OBJECT_TYPE_2 = "urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:Service";

    @Before
    public void insertTestObjects() throws MsgRegistryException {

        List<RegistryObjectType> objsToSubmit = new ArrayList<RegistryObjectType>();
        for (int i = 0; i < 5; i++) {
            RegistryObjectType obj = new RegistryObjectType("Source Object "
                    + i, "Source Object " + i);

            obj.setObjectType(REGISTRY_OBJECT_TYPE_1);
            objsToSubmit.add(obj);
        }

        for (int i = 1; i < 6; i++) {
            RegistryObjectType obj = new RegistryObjectType("Target Object "
                    + i, "Target Object " + i);
            obj.setObjectType(REGISTRY_OBJECT_TYPE_2);
            objsToSubmit.add(obj);
        }

        for (int i = 0; i < 5; i++) {
            AssociationType association = new AssociationType();
            association.setId("Affiliated With Association " + i);
            association.setLid(association.getId());
            association.setObjectType(RegistryObjectTypes.ASSOCIATION);
            association.setType(AssociationTypes.AFFILIATED_WITH);
            association.setSourceObject("Source Object " + i);
            association.setTargetObject("Target Object " + (i + 1));
            objsToSubmit.add(association);
        }

        for (int i = 0; i < 5; i++) {
            AssociationType association = new AssociationType();
            association.setId("Described By Association " + i);
            association.setLid(association.getId());
            association.setObjectType(RegistryObjectTypes.ASSOCIATION);
            association.setType(AssociationTypes.DESCRIBED_BY);
            association.setSourceObject("Source Object " + i);
            association.setTargetObject("Target Object " + (i + 1));
            objsToSubmit.add(association);
        }
        submitRegistryObjectsToRegistry(objsToSubmit);
    }

    @Test
    public void getAssociatedTargetObjects() throws MsgRegistryException {
        QueryRequest query = createQuery(
                CanonicalQueryTypes.FIND_ASSOCIATIONS,
                QueryConstants.ASSOCIATION_TYPE,
                "/urn:oasis:names:tc:ebxml-regrep:classificationScheme:AssociationType/AffiliatedWith",
                QueryConstants.SOURCE_OBJECT_ID, "Source Object 0");
        List<RegistryObjectType> result = executeQuery(findAssociatedObjects,
                query);
        assertEquals(1, result.size());
        assertEquals("Target Object 1", result.get(0).getId());

        query = createQuery(
                CanonicalQueryTypes.FIND_ASSOCIATIONS,
                QueryConstants.ASSOCIATION_TYPE,
                "/urn:oasis:names:tc:ebxml-regrep:classificationScheme:AssociationType/AffiliatedWith",
                QueryConstants.SOURCE_OBJECT_ID, "Source Object ?");
        result = executeQuery(findAssociatedObjects, query);
        assertEquals(5, result.size());
    }

    @Test
    public void getAssociatedSourceObjects() throws MsgRegistryException {
        QueryRequest query = createQuery(
                CanonicalQueryTypes.FIND_ASSOCIATIONS,
                QueryConstants.ASSOCIATION_TYPE,
                "/urn:oasis:names:tc:ebxml-regrep:classificationScheme:AssociationType/AffiliatedWith",
                QueryConstants.TARGET_OBJECT_ID, "Target Object 1");
        List<RegistryObjectType> result = executeQuery(findAssociatedObjects,
                query);
        assertEquals(1, result.size());
        assertEquals("Source Object 0", result.get(0).getId());

        query = createQuery(
                CanonicalQueryTypes.FIND_ASSOCIATIONS,
                QueryConstants.ASSOCIATION_TYPE,
                "/urn:oasis:names:tc:ebxml-regrep:classificationScheme:AssociationType/AffiliatedWith",
                QueryConstants.TARGET_OBJECT_ID, "Target Object ?");
        result = executeQuery(findAssociatedObjects, query);
        assertEquals(5, result.size());
    }
}
