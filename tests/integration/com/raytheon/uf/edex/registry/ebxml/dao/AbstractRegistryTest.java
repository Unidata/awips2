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
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import java.math.BigInteger;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
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

import org.junit.Before;
import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.transaction.TransactionConfiguration;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Lists;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.util.SpringFiles;
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
 * Apr 15, 2013 1914       djohnson     Initial creation
 * Apr 18, 2013 1693       djohnson     Consolidate reusable methods.
 * Apr 23, 2013 1910       djohnson     Allow sub-classes to pass callables and monitor for fault exceptions.
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
        SpringFiles.EBXML_REGISTRY_DAO_XML,
        SpringFiles.EBXML_REGISTRY_ENCODER_XML,
        SpringFiles.EBXML_WEBSERVICES_XML, SpringFiles.EBXML_XACML_XML,
        SpringFiles.EBXML_VALIDATOR_PLUGINS_XML,
        SpringFiles.EBXML_SUBSCRIPTION_XML, SpringFiles.EVENTBUS_COMMON_XML,
        DatabaseUtil.UNIT_TEST_DB_BEANS_XML,
        SpringFiles.UNIT_TEST_EBXML_BEANS_XML,
        SpringFiles.UNIT_TEST_LOCALIZATION_BEANS_XML })
@TransactionConfiguration(transactionManager = "metadataTxManager", defaultRollback = true)
@Transactional
@Ignore
public class AbstractRegistryTest {

    protected static final String MY_REGISTRY_OBJECT_ID = "myRegistryObjectId";

    protected static final String REGISTRY_OBJECT_TYPE = "myRegistryObjectType";

    @Autowired
    protected LifecycleManager lifecycleManager;

    protected QueryManager queryManager;

    @Before
    public void setUp() {
        this.queryManager = EDEXUtil.getESBComponent(QueryManager.class,
                "queryServiceImpl");
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
