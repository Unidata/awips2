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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryPackageType;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.plugins.GetRegistryPackagesByMemberId;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.QueryTest;

/**
 * 
 * Test for the Canonical GetRegistryPackagesByMemberId query defined by the
 * EBXML 4.0 spec
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
public class GetRegistryPackagesByMemberIdTest extends QueryTest {

    @Autowired
    private GetRegistryPackagesByMemberId getPackages;

    @Before
    public void insertTestObjects() throws MsgRegistryException {

        List<RegistryObjectType> objsToSubmit = new ArrayList<RegistryObjectType>();

        RegistryObjectType testObj1 = new RegistryObjectType("Test OBJ 1",
                "Test OBJ 1");
        RegistryObjectType testObj2 = new RegistryObjectType("Test OBJ 2",
                "Test OBJ 2");

        RegistryPackageType pack1 = new RegistryPackageType();
        pack1.setId("Package 1");
        pack1.setLid("Package 1");
        pack1.setObjectType(RegistryObjectTypes.REGISTRY_PACKAGE);
        pack1.setRegistryObjectList(new RegistryObjectListType());

        RegistryPackageType pack2 = new RegistryPackageType();
        pack2.setId("Package 2");
        pack2.setLid("Package 2");
        pack2.setObjectType(RegistryObjectTypes.REGISTRY_PACKAGE);
        pack2.setRegistryObjectList(new RegistryObjectListType());

        RegistryPackageType pack3 = new RegistryPackageType();
        pack3.setId("Package 3");
        pack3.setLid("Package 3");
        pack3.setObjectType(RegistryObjectTypes.REGISTRY_PACKAGE);
        pack3.setRegistryObjectList(new RegistryObjectListType());

        pack1.getRegistryObjectList().getRegistryObject().add(testObj1);
        pack2.getRegistryObjectList().getRegistryObject().add(testObj1);

        pack2.getRegistryObjectList().getRegistryObject().add(testObj2);
        pack3.getRegistryObjectList().getRegistryObject().add(testObj2);

        objsToSubmit.add(testObj1);
        objsToSubmit.add(testObj2);
        objsToSubmit.add(pack1);
        objsToSubmit.add(pack2);
        objsToSubmit.add(pack3);

        submitRegistryObjectsToRegistry(objsToSubmit);

    }

    @Test
    public void getPackgesByMemberId() throws MsgRegistryException {
        QueryRequest request = createQuery(
                CanonicalQueryTypes.GET_REGISTRY_PACKAGES_BY_MEMBER_ID,
                QueryConstants.MEMBER_ID, "Test OBJ 1");
        List<RegistryObjectType> result = executeQuery(getPackages, request);
        assertEquals(2, result.size());

        request = createQuery(
                CanonicalQueryTypes.GET_REGISTRY_PACKAGES_BY_MEMBER_ID,
                QueryConstants.MEMBER_ID, "Test OBJ 2");
        result = executeQuery(getPackages, request);
        assertEquals(2, result.size());

    }
}
