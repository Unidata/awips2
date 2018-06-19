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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryPackageType;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.plugins.GetChildrenByParentId;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.QueryTest;

/**
 * 
 * Test for the Canonical GetChildrenByParentId query defined by the EBXML 4.0
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
public class GetChildrenByParentIdTest extends QueryTest {

    @Autowired
    private GetChildrenByParentId getChildren;

    @Before
    public void insertTestObjects() throws MsgRegistryException {

        List<RegistryObjectType> objsToSubmit = new ArrayList<RegistryObjectType>();
        List<RegistryObjectType> existingObjs = registryObjectDao
                .executeHQLQuery(
                        "FROM RegistryObjectType obj where obj.objectType!=:theType",
                        "theType", RegistryObjectTypes.CLASSIFICATION_SCHEME);
        for (RegistryObjectType obj : existingObjs) {
            if (obj.getObjectType()
                    .equals(RegistryObjectTypes.REGISTRY_PACKAGE)) {
                System.out.println("FOUND IT!!");
            }
        }
        RegistryPackageType pack = new RegistryPackageType();
        pack.setId("Existing Package");
        pack.setLid("Existing Package");
        pack.setObjectType(RegistryObjectTypes.REGISTRY_PACKAGE);
        RegistryObjectListType objList = new RegistryObjectListType(
                existingObjs);
        pack.setRegistryObjectList(objList);
        objsToSubmit.add(pack);

        RegistryObjectType unpackagedObject = new RegistryObjectType(
                "Unpackaged Object", "Unpackaged Object");
        objsToSubmit.add(unpackagedObject);

        OrganizationType testOrg = new OrganizationType();
        testOrg.setId("Test Org");
        testOrg.setLid("Test Org");
        testOrg.setObjectType(RegistryObjectTypes.ORGANIZATION);

        OrganizationType subOrg = new OrganizationType();
        subOrg.setId("Sub Org");
        subOrg.setLid("Sub Org");
        subOrg.setObjectType(RegistryObjectTypes.ORGANIZATION);
        objsToSubmit.add(subOrg);

        testOrg.getOrganization().add(subOrg);
        objsToSubmit.add(testOrg);

        RegistryPackageType subPack = new RegistryPackageType();
        subPack.setId("Sub Pack");
        subPack.setLid("Sub Pack");
        subPack.setObjectType(RegistryObjectTypes.REGISTRY_PACKAGE);
        RegistryObjectType subPackObj = new RegistryObjectType("FOo", "FOo");
        subPack.setRegistryObjectList(new RegistryObjectListType());
        subPack.getRegistryObjectList().getRegistryObject().add(subPackObj);
        pack.getRegistryObjectList().getRegistryObject().add(subPack);
        objsToSubmit.add(subPack);

        submitRegistryObjectsToRegistry(objsToSubmit);

    }

    @Test
    public void getChildrenUsingDefaults() throws MsgRegistryException {
        QueryRequest request = createQuery(CanonicalQueryTypes.GET_CHILDREN_BY_PARENT_ID);
        List<RegistryObjectType> objs = executeQuery(getChildren, request);
        // Expect 3 results. 1 Auditable event from the insert, the registry
        // package and the unpackaged object
        assertEquals(151, objs.size());
    }

    @Test
    public void testObjectTypeIsNotNullClassification()
            throws MsgRegistryException {
        QueryRequest request = createQuery(
                CanonicalQueryTypes.GET_CHILDREN_BY_PARENT_ID,
                QueryConstants.OBJECT_TYPE, "ClassificationScheme");
        List<RegistryObjectType> objs = executeQuery(getChildren, request);
        assertEquals(24, objs.size());
    }

    @Test
    public void testObjectTypeIsNotNullOrganization()
            throws MsgRegistryException {
        QueryRequest request = createQuery(
                CanonicalQueryTypes.GET_CHILDREN_BY_PARENT_ID,
                QueryConstants.OBJECT_TYPE, "Organization");
        List<RegistryObjectType> objs = executeQuery(getChildren, request);
        assertEquals(1, objs.size());
    }

    @Test
    public void testObjectTypeIsNotNullRegistryPackage()
            throws MsgRegistryException {
        QueryRequest request = createQuery(
                CanonicalQueryTypes.GET_CHILDREN_BY_PARENT_ID,
                QueryConstants.OBJECT_TYPE, "RegistryPackage");
        List<RegistryObjectType> objs = executeQuery(getChildren, request);
        assertEquals(2, objs.size());
    }

    @Test
    public void testParentIdSpecifiedRegistryPackage()
            throws MsgRegistryException {
        QueryRequest request = createQuery(
                CanonicalQueryTypes.GET_CHILDREN_BY_PARENT_ID,
                QueryConstants.PARENT_ID, "Sub Pack");
        List<RegistryObjectType> objs = executeQuery(getChildren, request);
        assertEquals(1, objs.size());
    }

    @Test
    public void testParentIdSpecifiedOrganization() throws MsgRegistryException {
        QueryRequest request = createQuery(
                CanonicalQueryTypes.GET_CHILDREN_BY_PARENT_ID,
                QueryConstants.OBJECT_TYPE, "Organization");
        List<RegistryObjectType> objs = executeQuery(getChildren, request);
        assertEquals(1, objs.size());
    }

    @Test
    public void testParentIdSpecifiedClassificationScheme()
            throws MsgRegistryException {
        QueryRequest request = createQuery(
                CanonicalQueryTypes.GET_CHILDREN_BY_PARENT_ID,
                QueryConstants.OBJECT_TYPE, "ClassificationScheme",
                QueryConstants.PARENT_ID,
                "urn:oasis:names:tc:ebxml-regrep:classificationScheme:ActionType");
        List<RegistryObjectType> objs = executeQuery(getChildren, request);
        assertEquals(16, objs.size());
    }

}
