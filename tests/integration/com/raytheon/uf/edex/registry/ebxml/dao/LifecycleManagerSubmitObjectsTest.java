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
package com.raytheon.uf.edex.registry.ebxml.dao;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectExistsExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.annotation.DirtiesContext.ClassMode;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.raytheon.uf.common.registry.constants.RegistryResponseStatus;
import com.raytheon.uf.edex.database.dao.DatabaseUtil;

/**
 * Test {@link LifecycleManager} submit objects functionality.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 15, 2013 1693       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { DatabaseUtil.UNIT_TEST_DB_BEANS_XML,
        "/spring/ebxml.xml", "/spring/ebxml-impl.xml",
        "/spring/ebxml-querytypes.xml", "/spring/ebxml-registry-dao.xml",
        "/ebxml/unit-test-ebxml-beans.xml", "/unit-test-localization-beans.xml" })
@DirtiesContext(classMode = ClassMode.AFTER_EACH_TEST_METHOD)
public class LifecycleManagerSubmitObjectsTest extends AbstractRegistryTest {

    /**
     * CreateOnly - If an object does not exist, server MUST create it as a new
     * object. If an object already exists, the server MUST return an
     * ObjectExistsException fault message
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void createOnlySubmitObjectsWithNonExistantObjectSucceeds()
            throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE, Mode.CREATE_ONLY);

        final RegistryResponseType response = lifecycleManager
                .submitObjects(submitObjectsRequest);

        assertThat(response.getStatus(),
                is(equalTo(RegistryResponseStatus.SUCCESS)));
    }

    /**
     * CreateOnly - If an object does not exist, server MUST create it as a new
     * object. If an object already exists, the server MUST return an
     * ObjectExistsException fault message
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void createOnlySubmitObjectsWithExistantObjectFails()
            throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE, Mode.CREATE_ONLY);

        lifecycleManager.submitObjects(submitObjectsRequest);

        try {
            lifecycleManager.submitObjects(submitObjectsRequest);

            fail("Expected a MsgRegistryException to have been thrown!");
        } catch (MsgRegistryException exception) {
            final RegistryExceptionType faultInfo = exception.getFaultInfo();
            assertThat(faultInfo,
                    is(instanceOf(ObjectExistsExceptionType.class)));
        }
    }

    /**
     * CreateOrReplace (default) - If an object does not exist, server MUST
     * create it as a new object. If an object already exists, server MUST
     * replace the existing object with the submitted object
     */
    @Test
    public void createOrReplaceSubmitObjectsWithNonExistantObjectSucceeds()
            throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_REPLACE);

        final RegistryResponseType response = lifecycleManager
                .submitObjects(submitObjectsRequest);

        assertThat(response.getStatus(),
                is(equalTo(RegistryResponseStatus.SUCCESS)));
    }

    /**
     * CreateOrReplace (default) - If an object does not exist, server MUST
     * create it as a new object. If an object already exists, server MUST
     * replace the existing object with the submitted object
     */
    @Test
    public void createOrReplaceSubmitObjectsWithExistantObjectSucceeds()
            throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_REPLACE);

        lifecycleManager.submitObjects(submitObjectsRequest);

        final RegistryResponseType response = lifecycleManager
                .submitObjects(submitObjectsRequest);

        assertThat(response.getStatus(),
                is(equalTo(RegistryResponseStatus.SUCCESS)));
    }

    /**
     * CreateOrReplace (default) - If an object does not exist, server MUST
     * create it as a new object. If an object already exists, server MUST
     * replace the existing object with the submitted object
     */
    @Test
    public void createOrReplaceSubmitObjectsWithExistantObjectReplacesExisting()
            throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_REPLACE);

        lifecycleManager.submitObjects(submitObjectsRequest);
        lifecycleManager.submitObjects(submitObjectsRequest);

        QueryRequest partQueryRequest = createQueryForRegistryObjectByLid(MY_REGISTRY_OBJECT_ID);

        final QueryResponse queryResponse = queryManager
                .executeQuery(partQueryRequest);
        final List<RegistryObjectType> registryObjects = queryResponse
                .getRegistryObjectList().getRegistryObject();
        assertThat(registryObjects, hasSize(1));
    }

    /**
     * CreateOrVersion - If an object does not exist, server MUST create it as a
     * new object. If an object already exists, server MUST not alter the
     * existing object and instead it MUST create a new version of the existing
     * object using the state of the submitted object
     */
    @Test
    public void createOrVersionSubmitObjectsWithNonExistantObjectSucceeds()
            throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_VERSION);

        final RegistryResponseType response = lifecycleManager
                .submitObjects(submitObjectsRequest);

        assertThat(response.getStatus(),
                is(equalTo(RegistryResponseStatus.SUCCESS)));
    }

    /**
     * CreateOrVersion - If an object does not exist, server MUST create it as a
     * new object. If an object already exists, server MUST not alter the
     * existing object and instead it MUST create a new version of the existing
     * object using the state of the submitted object
     */
    @Test
    public void createOrVersionSubmitObjectsWithExistantObjectVersionsExisting()
            throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_VERSION);

        lifecycleManager.submitObjects(submitObjectsRequest);
        lifecycleManager.submitObjects(submitObjectsRequest);

        QueryRequest partQueryRequest = createQueryForRegistryObjectByLid(MY_REGISTRY_OBJECT_ID);

        final QueryResponse queryResponse = queryManager
                .executeQuery(partQueryRequest);
        final List<RegistryObjectType> registryObjects = queryResponse
                .getRegistryObjectList().getRegistryObject();
        assertThat(registryObjects, hasSize(2));
    }

}
