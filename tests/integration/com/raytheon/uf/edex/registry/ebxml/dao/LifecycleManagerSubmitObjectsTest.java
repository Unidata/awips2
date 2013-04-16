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

import java.math.BigInteger;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectExistsExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.annotation.DirtiesContext.ClassMode;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.google.common.collect.Lists;
import com.raytheon.uf.common.registry.constants.RegistryResponseStatus;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.dao.DatabaseUtil;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl.RETURN_TYPE;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

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
        "/ebxml/unit-test-ebxml-beans.xml" })
@DirtiesContext(classMode = ClassMode.AFTER_EACH_TEST_METHOD)
public class LifecycleManagerSubmitObjectsTest {

    private static final String MY_REGISTRY_OBJECT_ID = "myRegistryObjectId";

    private static final String REGISTRY_OBJECT_TYPE = "myRegistryObjectType";

    @Autowired
    private LifecycleManager lifecycleManager;

    private QueryManager queryManager;

    @Before
    public void setUp() {
        this.queryManager = EDEXUtil.getESBComponent(QueryManager.class,
                "queryServiceImpl");
    }

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
                MY_REGISTRY_OBJECT_ID, Mode.CREATE_ONLY);

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
                MY_REGISTRY_OBJECT_ID, Mode.CREATE_ONLY);

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
                MY_REGISTRY_OBJECT_ID, Mode.CREATE_OR_REPLACE);

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
                MY_REGISTRY_OBJECT_ID, Mode.CREATE_OR_REPLACE);

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
                MY_REGISTRY_OBJECT_ID, Mode.CREATE_OR_REPLACE);

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
                MY_REGISTRY_OBJECT_ID, Mode.CREATE_OR_VERSION);

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
                MY_REGISTRY_OBJECT_ID, Mode.CREATE_OR_VERSION);

        lifecycleManager.submitObjects(submitObjectsRequest);
        lifecycleManager.submitObjects(submitObjectsRequest);

        QueryRequest partQueryRequest = createQueryForRegistryObjectByLid(MY_REGISTRY_OBJECT_ID);

        final QueryResponse queryResponse = queryManager
                .executeQuery(partQueryRequest);
        final List<RegistryObjectType> registryObjects = queryResponse
                .getRegistryObjectList().getRegistryObject();
        assertThat(registryObjects, hasSize(2));
    }

    /**
     * Create the submit objects request.
     * 
     * @param registryObjectId
     *            the registry object id
     * @param mode
     * @return
     */
    private SubmitObjectsRequest createSubmitObjectsRequest(
            String registryObjectId, Mode mode) {
        final RegistryObjectType registryObject = new RegistryObjectType();
        registryObject.setId(MY_REGISTRY_OBJECT_ID);
        registryObject.setLid(registryObject.getId());
        registryObject.setObjectType(REGISTRY_OBJECT_TYPE);

        List<RegistryObjectType> registryObjects = Lists.newArrayList();
        registryObjects.add(registryObject);

        RegistryObjectListType registryObjectList = new RegistryObjectListType();
        registryObjectList.setRegistryObject(registryObjects);
        SubmitObjectsRequest submitObjectsRequest = new SubmitObjectsRequest();
        submitObjectsRequest.setCheckReferences(false);
        submitObjectsRequest.setComment("This is a comment.");
        submitObjectsRequest.setId("someId");
        submitObjectsRequest.setMode(mode);
        submitObjectsRequest.setRegistryObjectList(registryObjectList);

        return submitObjectsRequest;
    }

    private QueryRequest createQueryForRegistryObjectByLid(
            String registryObjectId) {
        final ResponseOptionType responseOption = EbxmlObjectUtil.queryObjectFactory
                .createResponseOptionType();
        responseOption.setReturnType(RETURN_TYPE.RegistryObject.toString());
        responseOption.setReturnComposedObjects(false);

        final QueryType queryType = new QueryType();
        queryType
                .setQueryDefinition("urn:oasis:names:tc:ebxml-regrep:query:GetObjectsByLid");
        Set<SlotType> slots = new HashSet<SlotType>();
        final SlotType slot = new SlotType();
        slot.setName(QueryConstants.LID);
        final StringValueType slotValue = new StringValueType();
        slotValue.setStringValue(registryObjectId);
        slot.setSlotValue(slotValue);
        slots.add(slot);
        queryType.setSlot(slots);

        QueryRequest partQueryRequest = new QueryRequest();
        partQueryRequest.setResponseOption(responseOption);
        partQueryRequest.setFederated(false);
        partQueryRequest.setQuery(queryType);
        partQueryRequest.setMatchOlderVersions(true);
        partQueryRequest.setMaxResults(new BigInteger("9999"));
        return partQueryRequest;
    }

}
