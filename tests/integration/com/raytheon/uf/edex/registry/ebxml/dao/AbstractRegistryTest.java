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
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.transaction.TransactionConfiguration;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Lists;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.util.SpringFiles;
import com.raytheon.uf.common.util.TestUtil;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
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
 * Apr 15, 2013 1914       djohnson     Initial creation
 * Apr 18, 2013 1693       djohnson     Consolidate reusable methods.
 * Apr 23, 2013 1910       djohnson     Allow sub-classes to pass callables and monitor for fault exceptions.
 * Jun 05, 2013 2038       djohnson     Use TestUtil constant for transactionManager.
 * 10/8/2013    1682       bphillip     Added submitRegistryObjectsToRegistry
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { SpringFiles.DATADELIVERY_HANDLERS_XML,
        SpringFiles.DATADELIVERY_HANDLERS_IMPL_XML, SpringFiles.EBXML_XML,
        SpringFiles.EBXML_IMPL_XML, SpringFiles.EBXML_QUERYTYPES_XML,
        SpringFiles.EBXML_REGISTRY_DAO_XML, SpringFiles.EBXML_WEBSERVICES_XML,
        SpringFiles.EBXML_XACML_XML, SpringFiles.EBXML_VALIDATOR_PLUGINS_XML,
        SpringFiles.EBXML_SUBSCRIPTION_XML, SpringFiles.EVENTBUS_COMMON_XML,
        SpringFiles.UNIT_TEST_DB_BEANS_XML,
        SpringFiles.UNIT_TEST_EBXML_BEANS_XML,
        SpringFiles.UNIT_TEST_LOCALIZATION_BEANS_XML })
@TransactionConfiguration(transactionManager = TestUtil.METADATA_TX_MANAGER, defaultRollback = true)
@Transactional
@Ignore
public class AbstractRegistryTest {

    protected static final String MY_REGISTRY_OBJECT_ID = "myRegistryObjectId";

    protected static final String REGISTRY_OBJECT_TYPE = "myRegistryObjectType";

    @Autowired
    protected SlotTypeDao slotDao;

    @Autowired
    protected RegistryObjectDao registryObjectDao;

    @Autowired
    protected LifecycleManager lifecycleManager;

    @Autowired
    @Qualifier("queryServiceImpl")
    protected QueryManager queryManager;

    @BeforeClass
    @AfterClass
    public static void resetDbInitialized() {
        DbInit.INITIALIZED = false;
    }

    /**
     * Submits the registry object to the registry and verifies it was
     * successfully processed.
     * 
     * @param registryObjectId
     *            the registry object id to use
     * @param registryObject
     *            the registry object
     * @throws MsgRegistryException
     */
    protected void submitRegistryObjectToRegistry(
            final RegistryObjectType registryObject)
            throws MsgRegistryException {
        final SubmitObjectsRequest submitRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, MY_REGISTRY_OBJECT_ID,
                Mode.CREATE_OR_REPLACE);
        submitRequest.getRegistryObjects().clear();
        submitRequest.getRegistryObjects().add(registryObject);

        final RegistryResponseType submitResponse = lifecycleManager
                .submitObjects(submitRequest);
        assertSuccessfulResponse(submitResponse);
    }

    /**
     * Submits objects to the registry
     * 
     * @param registryObjects
     *            The registry objects to submit
     * @throws MsgRegistryException
     *             If errors occur during submission
     */
    protected void submitRegistryObjectsToRegistry(
            final Collection<RegistryObjectType> registryObjects)
            throws MsgRegistryException {
        for (RegistryObjectType registryObject : registryObjects) {
            submitRegistryObjectToRegistry(registryObject);
        }
    }

    /**
     * Create the submit objects request.
     * 
     * @param registryObjectId
     *            the registry object id
     * @param mode
     * @return
     */
    protected SubmitObjectsRequest createSubmitObjectsRequest(
            String registryObjectId, String registryObjectType, Mode mode) {
        final RegistryObjectType registryObject = new RegistryObjectType();
        registryObject.setId(MY_REGISTRY_OBJECT_ID);
        registryObject.setLid(registryObject.getId());
        registryObject.setObjectType(registryObjectType);
        registryObject.setName(RegistryUtil
                .getInternationalString(registryObjectId));
        registryObject.setDescription(RegistryUtil
                .getInternationalString(registryObjectId));

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

    protected QueryRequest createQueryForRegistryObjectByLid(
            String registryObjectId) {
        final ResponseOptionType responseOption = EbxmlObjectUtil.queryObjectFactory
                .createResponseOptionType();
        responseOption
                .setReturnType(ResponseOptionType.RETURN_TYPE.RegistryObject
                        .toString());
        responseOption.setReturnComposedObjects(false);

        final QueryType queryType = new QueryType();
        queryType
                .setQueryDefinition("urn:oasis:names:tc:ebxml-regrep:query:GetObjectsByLid");
        List<SlotType> slots = new ArrayList<SlotType>(1);
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

    /**
     * Expect the specified exception to be wrapped in a
     * {@link MsgRegistryException}.
     * 
     * @param <T>
     *            the expected exception type
     * @param submitObjectsRequest
     *            the request
     * @param expectedException
     *            the expected exception class
     */
    protected <T extends RegistryExceptionType> void expectFaultException(
            Callable<?> callable, Class<T> expectedException) {
        try {
            callable.call();
            fail("Expected a MsgRegistryException to have been thrown, wrapping a ["
                    + expectedException.getName() + "]");
        } catch (MsgRegistryException exception) {
            final RegistryExceptionType faultInfo = exception.getFaultInfo();
            assertThat(faultInfo, is(instanceOf(expectedException)));
        } catch (Exception e) {
            throw new RuntimeException("Incorrect exception type was thrown!",
                    e);
        }
    }

    /**
     * Assert a successful response status.
     * 
     * @param response
     *            the response
     */
    protected void assertSuccessfulResponse(RegistryResponseType response) {
        assertResponseStatus("The response did not have a successful status!",
                response, RegistryResponseStatus.SUCCESS);
    }

    /**
     * Assert a successful response status.
     * 
     * @param response
     *            the response
     */
    protected void assertPartialSuccessResponse(RegistryResponseType response) {
        assertResponseStatus(
                "The response did not have a partial success status!",
                response, RegistryResponseStatus.PARTIAL_SUCCESS);
    }

    /**
     * Assert a successful response status.
     * 
     * @param response
     *            the response
     */
    protected void assertFailureResponse(RegistryResponseType response) {
        assertResponseStatus(
                "The response did not have a partial success status!",
                response, RegistryResponseStatus.FAILURE);
    }

    /**
     * Asserts that the object with the given ID exists in the registry
     * 
     * @param objectId
     *            The id of the object to check
     */
    protected void assertObjectExists(String objectId) {
        RegistryObjectType result = registryObjectDao.getById(objectId);
        assertNotNull(result);
        assertThat(result.getId(), is(equalTo(objectId)));
    }

    /**
     * Asserts that the object with the give ID does NOT exist in the registry
     * 
     * @param objectId
     *            The id of the object to check
     */
    protected void assertObjectDoesNotExist(String objectId) {
        RegistryObjectType result = registryObjectDao.getById(objectId);
        assertNull(result);
    }

    protected void assertSlotExists(String slotName) {
        List<SlotType> result = slotDao.executeHQLQuery(
                "from SlotType slot where slot.name=:slotName", "slotName",
                slotName);
        assertNotNull(result);
        assertThat(result.isEmpty(), is(equalTo(false)));
        assertThat(result.get(0).getName(), is(equalTo(slotName)));
    }

    protected void assertSlotDoesNotExist(String slotName) {
        List<SlotType> result = slotDao.executeHQLQuery(
                "from SlotType slot where slot.name=:slotName", "slotName",
                slotName);
        assertNotNull(result);
        assertThat(result.isEmpty(), is(equalTo(true)));
    }

    /**
     * Assert the response has the given status.
     * 
     * @param failMessage
     *            the assertion failure message
     * @param response
     *            the response
     * @param expected
     *            the expected status
     */
    private void assertResponseStatus(String failMessage,
            RegistryResponseType response, RegistryResponseStatus expected) {
        assertThat(failMessage, response.getStatus(), is(equalTo(expected)));
    }

}
