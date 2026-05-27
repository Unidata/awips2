package com.raytheon.uf.edex.registry.ebxml.services.lifecycleManager;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtrinsicObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.ReferencesExistExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

import org.junit.Test;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.DeletionScope;

public class LifecycleManagerRemoveObjectsTest extends LifecycleManagerTest {

    @Test
    public void removeObjectsWithReferencesSpecified()
            throws MsgRegistryException {
        assertSuccessfulResponse(removeObjects(createRemoveObjectsRequest(true,
                false, DeletionScope.DELETE_ALL, null, TEST_OBJECT_ID_1)));
        assertObjectDoesNotExist(TEST_OBJECT_ID_1);
        assertSlotDoesNotExist("Test Slot1");
    }

    @Test
    public void removeObjectsDeleteRepositoryItemOnly()
            throws MsgRegistryException {
        ExtrinsicObjectType obj = (ExtrinsicObjectType) registryObjectDao
                .getById(TEST_OBJECT_ID_3);
        assertNotNull(obj.getRepositoryItem());
        assertSuccessfulResponse(removeObjects(createRemoveObjectsRequest(true,
                false, DeletionScope.DELETE_REPOSITORY_ITEM_ONLY, null,
                TEST_OBJECT_ID_3)));
        assertObjectExists(TEST_OBJECT_ID_3);
        obj = (ExtrinsicObjectType) registryObjectDao.getById(TEST_OBJECT_ID_3);
        assertNull(obj.getRepositoryItem());
    }

    @Test
    public void removeObjectsWithQuerySpecified() throws MsgRegistryException {
        QueryType query = new QueryType();
        query.setQueryDefinition(CanonicalQueryTypes.GET_OBJECT_BY_ID);
        SlotType objectTypeSlot = new SlotType("id", new StringValueType(
                TEST_OBJECT_ID_1));
        query.getSlot().add(objectTypeSlot);
        assertSuccessfulResponse(removeObjects(createRemoveObjectsRequest(true,
                false, DeletionScope.DELETE_ALL, query)));
        assertObjectDoesNotExist(TEST_OBJECT_ID_1);
        assertObjectExists(TEST_OBJECT_ID_2);

    }

    @Test
    public void removeObjectsWithInvalidDeletionScope() {
        try {
            removeObjects(createRemoveObjectsRequest(false, false,
                    "Invalid DeletionScope", null, TEST_OBJECT_ID_1));
        } catch (MsgRegistryException e) {
            RegistryExceptionType exception = e.getFaultInfo();
            assertTrue(exception instanceof QueryExceptionType);
            return;
        }
        fail("Expected QueryExceptionType to be thrown");
    }

    @Test
    public void removeObjectsWithCheckReferencesReferencesExist() {
        try {
            removeObjects(createRemoveObjectsRequest(true, true,
                    DeletionScope.DELETE_ALL, null, TEST_OBJECT_ID_3));
        } catch (MsgRegistryException e) {
            RegistryExceptionType exception = e.getFaultInfo();
            assertTrue(exception instanceof ReferencesExistExceptionType);
            assertObjectExists(TEST_OBJECT_ID_3);
            return;
        }
        fail("Expected ReferencesExistExceptionType to be thrown");
    }

    @Test
    public void removeObjectsWithCheckReferencesReferencesDoNotExist()
            throws MsgRegistryException {

        removeObjects(createRemoveObjectsRequest(true, true,
                DeletionScope.DELETE_ALL, null, TEST_OBJECT_ID_1));
        assertObjectDoesNotExist(TEST_OBJECT_ID_1);

    }
}
