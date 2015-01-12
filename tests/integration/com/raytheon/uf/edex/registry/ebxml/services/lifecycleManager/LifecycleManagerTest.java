package com.raytheon.uf.edex.registry.ebxml.services.lifecycleManager;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtrinsicObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import org.junit.Before;
import org.junit.Ignore;

import com.raytheon.uf.edex.registry.ebxml.dao.AbstractRegistryTest;

@Ignore
public abstract class LifecycleManagerTest extends AbstractRegistryTest {

    protected static final String TEST_SLOT_NAME_1 = "Test Slot 1";

    protected static final String OBJECT_TYPE_1 = "Object Type 1";

    protected static final String OBJECT_TYPE_2 = "Object Type 2";

    protected static final String OBJECT_TYPE_3 = "urn:oasis:names:tc:ebxml-regrep:ObjectType:RegistryObject:ExtrinsicObject";

    protected static final String TEST_OBJECT_ID_1 = "Test Object 1";

    protected static final String TEST_OBJECT_ID_2 = "Test Object 2";

    protected static final String TEST_OBJECT_ID_3 = "Test Object 3";

    @Before
    public void insertTestObjects() throws MsgRegistryException {
        // Create test object 1
        RegistryObjectType testObject1 = new RegistryObjectType();
        testObject1.setId(TEST_OBJECT_ID_1);
        testObject1.setLid(TEST_OBJECT_ID_1);
        testObject1.setObjectType(OBJECT_TYPE_1);
        SlotType testObjectSlot1 = new SlotType(TEST_SLOT_NAME_1,
                new StringValueType("Test Slot1_Value"));
        testObject1.getSlot().add(testObjectSlot1);

        // Create test object 2
        RegistryObjectType testObject2 = new RegistryObjectType();
        testObject2.setId(TEST_OBJECT_ID_2);
        testObject2.setLid(TEST_OBJECT_ID_2);
        testObject2.setObjectType(OBJECT_TYPE_2);

        // Create test object 3
        ExtrinsicObjectType testObject3 = new ExtrinsicObjectType();
        testObject3.setId(TEST_OBJECT_ID_3);
        testObject3.setLid(TEST_OBJECT_ID_3);
        testObject3.setObjectType(OBJECT_TYPE_3);
        testObject3.setRepositoryItem(new byte[] { 1, 2, 3, 4 });

        // Submit the test objects
        submitObjects(testObject1, testObject2, testObject3);

        // Check if objects submitted are in the registry
        assertObjectExists(TEST_OBJECT_ID_1);
        assertObjectExists(TEST_OBJECT_ID_2);
    }

    protected RegistryResponseType removeObjects(RemoveObjectsRequest request)
            throws MsgRegistryException {
        return lifecycleManager.removeObjects(request);
    }

    protected RemoveObjectsRequest createRemoveObjectsRequest(
            boolean deleteChildren, boolean checkReferences,
            String deletionScope, QueryType query) {
        return createRemoveObjectsRequest(deleteChildren, checkReferences,
                deletionScope, query, (String[]) null);
    }

    protected RemoveObjectsRequest createRemoveObjectsRequest(
            boolean deleteChildren, boolean checkReferences,
            String deletionScope, QueryType query, String... objsToDelete) {
        RemoveObjectsRequest request = new RemoveObjectsRequest();
        request.setId("Test Remove Request");
        request.setComment("Test Remove Request");
        request.setDeleteChildren(deleteChildren);
        request.setDeletionScope(deletionScope);
        request.setUsername("Some User");
        request.setCheckReferences(checkReferences);
        if (query != null) {
            request.setQuery(query);
        }
        if (objsToDelete != null) {
            if (objsToDelete.length > 0) {
                request.setObjectRefList(new ObjectRefListType());
            }
            for (String objToDelete : objsToDelete) {
                request.getObjectRefList().getObjectRef()
                        .add(new ObjectRefType(objToDelete));
            }
        }

        return request;
    }

    public RegistryResponseType submitObjects(RegistryObjectType... objs)
            throws MsgRegistryException {
        return lifecycleManager.submitObjects(createSubmitObjectsRequest(objs));
    }

    public RegistryResponseType submitObjects(SubmitObjectsRequest request)
            throws MsgRegistryException {
        return lifecycleManager.submitObjects(request);
    }

    protected SubmitObjectsRequest createSubmitObjectsRequest(
            RegistryObjectType... objs) {
        SubmitObjectsRequest submitObjectsRequest = new SubmitObjectsRequest();
        submitObjectsRequest.setCheckReferences(false);
        submitObjectsRequest.setComment("Submit Comment");
        submitObjectsRequest.setId("someId");
        submitObjectsRequest.setMode(Mode.CREATE_ONLY);
        if (objs.length > 0) {
            submitObjectsRequest
                    .setRegistryObjectList(new RegistryObjectListType());
        }
        for (RegistryObjectType obj : objs) {
            submitObjectsRequest.getRegistryObjectList().getRegistryObject()
                    .add(obj);
        }
        return submitObjectsRequest;
    }

}
