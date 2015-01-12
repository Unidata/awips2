package com.raytheon.uf.edex.registry.ebxml.services.lifecycleManager;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateActionType;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateActionType.UPDATE_MODE;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AnyValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryExpressionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringQueryExpressionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.XMLQueryExpressionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectNotFoundExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnsupportedCapabilityExceptionType;

import org.junit.Test;

import com.raytheon.uf.common.registry.constants.QueryLanguages;

public class LifecycleManagerUpdateObjectsTest extends LifecycleManagerTest {

    @Test
    public void updateObjectsObjectNotInRegistry() {
        UpdateObjectsRequest request = createUpdateObjectsRequest(
                Mode.CREATE_OR_REPLACE, false, null, null, "Non-existant ID");
        try {
            update(request);
        } catch (MsgRegistryException e) {
            assertTrue(e.getFaultInfo() instanceof ObjectNotFoundExceptionType);
            return;
        }
        fail("Expected ObjectNotFoundExceptionType");
    }

    @Test
    public void updateObjectsUnsupportedQueryLanguageReceived() {
        UpdateObjectsRequest request = createUpdateObjectsRequest(
                Mode.CREATE_OR_REPLACE, false, null, null, TEST_OBJECT_ID_1);
        UpdateActionType updateAction = createUpdateAction(UPDATE_MODE.Insert,
                new StringQueryExpressionType("Unsupported Language", ""),
                new StringValueType(""));
        request.getUpdateAction().add(updateAction);
        try {
            update(request);
        } catch (MsgRegistryException e) {
            assertTrue(e.getFaultInfo() instanceof UnsupportedCapabilityExceptionType);
            return;
        }
        fail("Expected UnsupportedCapabilityExceptionType");
    }

    @Test
    public void updateObjectsUnsupportedQueryExpressionTypeReceived() {
        UpdateObjectsRequest request = createUpdateObjectsRequest(
                Mode.CREATE_OR_REPLACE, false, null, null, TEST_OBJECT_ID_1);
        UpdateActionType updateAction = createUpdateAction(UPDATE_MODE.Insert,
                new XMLQueryExpressionType("Unsupported Query Expression Type",
                        ""), new StringValueType(""));
        request.getUpdateAction().add(updateAction);
        try {
            update(request);
        } catch (MsgRegistryException e) {
            assertTrue(e.getFaultInfo() instanceof UnsupportedCapabilityExceptionType);
            return;
        }
        fail("Expected UnsupportedCapabilityExceptionType");
    }

    @Test
    public void updateObjectsInsertModeDidntSpecifyValue() {
        UpdateObjectsRequest request = createUpdateObjectsRequest(
                Mode.CREATE_OR_REPLACE, false, null, null, TEST_OBJECT_ID_1);
        UpdateActionType updateAction = createUpdateAction(UPDATE_MODE.Insert,
                new StringQueryExpressionType(QueryLanguages.XPATH, ""), null);
        request.getUpdateAction().add(updateAction);
        try {
            update(request);
        } catch (MsgRegistryException e) {
            assertTrue(e.getFaultInfo() instanceof RegistryExceptionType);
            return;
        }
        fail("Expected RegistryExceptionType");
    }

    @Test
    public void updateObjectsUpdateModeDidntSpecifyValue() {
        UpdateObjectsRequest request = createUpdateObjectsRequest(
                Mode.CREATE_OR_REPLACE, false, null, null, TEST_OBJECT_ID_1);
        UpdateActionType updateAction = createUpdateAction(UPDATE_MODE.Update,
                new StringQueryExpressionType(QueryLanguages.XPATH, ""), null);
        request.getUpdateAction().add(updateAction);
        try {
            update(request);
        } catch (MsgRegistryException e) {
            assertTrue(e.getFaultInfo() instanceof RegistryExceptionType);
            return;
        }
        fail("Expected RegistryExceptionType");
    }

    @Test
    public void updateObjectsDeleteModeSpecifiedValue() {
        UpdateObjectsRequest request = createUpdateObjectsRequest(
                Mode.CREATE_OR_REPLACE, false, null, null, TEST_OBJECT_ID_1);
        UpdateActionType updateAction = createUpdateAction(UPDATE_MODE.Update,
                new StringQueryExpressionType(QueryLanguages.XPATH, ""),
                new StringValueType("Invalid"));
        request.getUpdateAction().add(updateAction);
        try {
            update(request);
        } catch (MsgRegistryException e) {
            assertTrue(e.getFaultInfo() instanceof RegistryExceptionType);
            return;
        }
        fail("Expected RegistryExceptionType");
    }

    @Test
    public void updateObjectsInsertModeTest() throws MsgRegistryException {
        SlotType slotToInsert = new SlotType("testSlot", new StringValueType(
                "Test Value"));
        AnyValueType value = new AnyValueType(slotToInsert);

        UpdateObjectsRequest request = createUpdateObjectsRequest(
                Mode.CREATE_OR_REPLACE, false, null, null, TEST_OBJECT_ID_1);
        UpdateActionType updateAction = createUpdateAction(UPDATE_MODE.Insert,
                new StringQueryExpressionType(QueryLanguages.XPATH,
                        "/rim:RegistryObject"), value);
        request.getUpdateAction().add(updateAction);
        update(request);
        assertSlotExists("testSlot");
        RegistryObjectType obj = registryObjectDao.getById(TEST_OBJECT_ID_1);
        assertNotNull(obj.getSlotByName("testSlot"));
    }

    @Test
    public void updateObjectsUpdateModeTestReplaceAttributeValue()
            throws MsgRegistryException {

        String updatedValue = "Updated Value";

        StringValueType newValue = new StringValueType(updatedValue);

        UpdateObjectsRequest request = createUpdateObjectsRequest(
                Mode.CREATE_OR_REPLACE, false, null, null, TEST_OBJECT_ID_1);
        UpdateActionType updateAction = createUpdateAction(UPDATE_MODE.Update,
                new StringQueryExpressionType(QueryLanguages.XPATH,
                        "/rim:RegistryObject/rim:Slot[@name='"
                                + TEST_SLOT_NAME_1 + "']/@name"), newValue);
        request.getUpdateAction().add(updateAction);
        update(request);
        assertSlotExists(updatedValue);
        RegistryObjectType obj = registryObjectDao.getById(TEST_OBJECT_ID_1);
        SlotType slot = obj.getSlotByName(updatedValue);
        assertNotNull(slot);
    }

    @Test
    public void updateObjectsUpdateModeTestReplaceNodeValue()
            throws MsgRegistryException {
        String updatedStringValue = "Updated String Value";

        SlotType updatedValue = new SlotType(TEST_SLOT_NAME_1,
                new StringValueType(updatedStringValue));
        AnyValueType newValue = new AnyValueType(updatedValue);

        UpdateObjectsRequest request = createUpdateObjectsRequest(
                Mode.CREATE_OR_REPLACE, false, null, null, TEST_OBJECT_ID_1);
        UpdateActionType updateAction = createUpdateAction(UPDATE_MODE.Update,
                new StringQueryExpressionType(QueryLanguages.XPATH,
                        "/rim:RegistryObject/rim:Slot[@name='"
                                + TEST_SLOT_NAME_1 + "']"), newValue);
        request.getUpdateAction().add(updateAction);
        update(request);
        assertSlotExists(TEST_SLOT_NAME_1);
        RegistryObjectType obj = registryObjectDao.getById(TEST_OBJECT_ID_1);
        SlotType slot = obj.getSlotByName(TEST_SLOT_NAME_1);
        assertNotNull(slot);
        assertTrue(slot.getSlotValue().getValue().equals(updatedStringValue));
    }

    @Test
    public void updateObjectsDeleteModeTestDeleteNodeValue()
            throws MsgRegistryException {
        UpdateObjectsRequest request = createUpdateObjectsRequest(
                Mode.CREATE_OR_REPLACE, false, null, null, TEST_OBJECT_ID_1);
        UpdateActionType updateAction = createUpdateAction(UPDATE_MODE.Delete,
                new StringQueryExpressionType(QueryLanguages.XPATH,
                        "/rim:RegistryObject/rim:Slot[@name='"
                                + TEST_SLOT_NAME_1 + "']"), null);
        request.getUpdateAction().add(updateAction);
        update(request);
        assertSlotDoesNotExist(TEST_SLOT_NAME_1);
        RegistryObjectType obj = registryObjectDao.getById(TEST_OBJECT_ID_1);
        SlotType slot = obj.getSlotByName(TEST_SLOT_NAME_1);
        assertNull(slot);
    }

    @Test
    public void updateObjectsDeleteModeTestDeleteAttributeValue()
            throws MsgRegistryException {

        UpdateObjectsRequest request = createUpdateObjectsRequest(
                Mode.CREATE_OR_REPLACE, false, null, null, TEST_OBJECT_ID_1);
        UpdateActionType updateAction = createUpdateAction(UPDATE_MODE.Delete,
                new StringQueryExpressionType(QueryLanguages.XPATH,
                        "/rim:RegistryObject/rim:Slot[@name='"
                                + TEST_SLOT_NAME_1 + "']/@name"), null);
        request.getUpdateAction().add(updateAction);
        update(request);
        assertSlotDoesNotExist(TEST_SLOT_NAME_1);
        RegistryObjectType obj = registryObjectDao.getById(TEST_OBJECT_ID_1);
        SlotType slot = obj.getSlotByName(TEST_SLOT_NAME_1);
        assertNull(slot);
    }

    protected void update(UpdateObjectsRequest request)
            throws MsgRegistryException {
        lifecycleManager.updateObjects(request);
    }

    protected UpdateObjectsRequest createUpdateObjectsRequest(Mode updateMode,
            boolean checkReferences, List<UpdateActionType> updateActions,
            QueryType query, String... objectIds) {
        UpdateObjectsRequest request = new UpdateObjectsRequest();
        request.setId("Test Registry Update");
        request.setComment("Testing Registry Update");
        request.setMode(updateMode);
        request.setUpdateAction(updateActions);
        request.setCheckReferences(checkReferences);
        if (query != null) {
            request.setQuery(query);
        }
        if (objectIds.length > 0) {
            request.setObjectRefList(new ObjectRefListType());
            for (String objId : objectIds) {
                request.getObjectRefList().getObjectRef()
                        .add(new ObjectRefType(objId));
            }
        }
        return request;
    }

    protected UpdateActionType createUpdateAction(UPDATE_MODE updateMode,
            QueryExpressionType queryExpression, ValueType value) {
        UpdateActionType action = new UpdateActionType();
        action.setUpdateMode(updateMode);
        if (queryExpression != null) {
            action.setSelector(queryExpression);
        }
        if (value != null) {
            action.setValueHolder(value);
        }
        return action;
    }
}
