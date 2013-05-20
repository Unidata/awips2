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
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Callable;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationTypeFixture;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.InvalidRequestExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectExistsExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnresolvedReferenceExceptionType;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.transaction.TransactionConfiguration;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Sets;
import com.raytheon.uf.common.util.SpringFiles;
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
 * Apr 18, 2013 1693       djohnson     More tests verifying spec compliance..
 * Apr 23, 2013 1910       djohnson     More checkReference tests.
 * May 02, 2013 1910       djohnson     Add validator plugins spring file.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { DatabaseUtil.UNIT_TEST_DB_BEANS_XML,
        SpringFiles.EVENTBUS_COMMON_XML,
        SpringFiles.EBXML_XML, SpringFiles.EBXML_IMPL_XML,
        SpringFiles.EBXML_QUERYTYPES_XML, SpringFiles.EBXML_REGISTRY_DAO_XML,
        SpringFiles.EBXML_WEBSERVICES_XML, SpringFiles.EBXML_XACML_XML,
        SpringFiles.EBXML_VALIDATOR_PLUGINS_XML,
        SpringFiles.EBXML_SUBSCRIPTION_XML,
        SpringFiles.UNIT_TEST_EBXML_BEANS_XML,
        SpringFiles.UNIT_TEST_LOCALIZATION_BEANS_XML })
@TransactionConfiguration(transactionManager = "metadataTxManager", defaultRollback = true)
@Transactional
public class LifecycleManagerSubmitObjectsTest extends AbstractRegistryTest {

    /**
     * CreateOnly - If an object does not exist, server MUST create it as a new
     * object. If an object already exists, the server MUST return an
     * ObjectExistsException fault message
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void createOnlyWithNonExistantObjectSucceeds()
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
    public void createOnlyWithExistantObjectFails() throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE, Mode.CREATE_ONLY);

        lifecycleManager.submitObjects(submitObjectsRequest);

        expectFaultException(submitObjectsRequest,
                ObjectExistsExceptionType.class);
    }

    /**
     * CreateOrReplace (default) - If an object does not exist, server MUST
     * create it as a new object. If an object already exists, server MUST
     * replace the existing object with the submitted object
     */
    @Test
    public void createOrReplaceWithNonExistantObjectSucceeds()
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
     * CreateOrReplace (default)
     */
    @Test
    public void noModeSpecifiedCanReplaceExistantObject()
            throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_REPLACE);

        lifecycleManager.submitObjects(submitObjectsRequest);

        // Null out the mode
        submitObjectsRequest.setMode(null);

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
    public void createOrReplaceWithExistantObjectSucceeds()
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
    public void createOrReplaceWithExistantObjectReplacesExisting()
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
    public void createOrVersionWithNonExistantObjectSucceeds()
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
    public void createOrVersionWithExistantObjectVersionsExisting()
            throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_VERSION);

        lifecycleManager.submitObjects(submitObjectsRequest);
        lifecycleManager.submitObjects(createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_VERSION));

        QueryRequest partQueryRequest = createQueryForRegistryObjectByLid(MY_REGISTRY_OBJECT_ID);

        final QueryResponse queryResponse = queryManager
                .executeQuery(partQueryRequest);
        final List<RegistryObjectType> registryObjects = queryResponse
                .getRegistryObjectList().getRegistryObject();
        assertThat(registryObjects, hasSize(2));
    }

    /**
     * Attribute checkReferences - true - Specifies that a server MUST check
     * submitted objects and make sure that all references via reference
     * attributes and slots to other RegistryObjects are resolvable. If a
     * reference does not resolve then the server MUST return
     * UnresolvedReferenceException
     */
    @Test
    public void checkReferencesTrueWithNonExistantAssociationFails()
            throws MsgRegistryException {

        final ClassificationType classificationType = new ClassificationType();
        classificationType.setId("someClassificationId");

        final Set<ClassificationType> classifications = Sets
                .<ClassificationType> newHashSet();
        classifications.add(classificationType);

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_VERSION);
        submitObjectsRequest.setCheckReferences(true);

        final RegistryObjectType registryObject = submitObjectsRequest
                .getRegistryObjects().iterator().next();
        registryObject.setClassification(classifications);

        expectFaultException(submitObjectsRequest,
                UnresolvedReferenceExceptionType.class);
    }

    /**
     * Attribute checkReferences - false - Specifies that a server MUST NOT
     * check submitted objects to make sure that all references via reference
     * attributes and slots to other RegistryObjects are resolvable. If a
     * reference does not resolve then the server MUST NOT return
     * UnresolvedReferenceException
     */
    @Test
    public void checkReferencesFalseWithNonExistantAssociationSucceeds()
            throws MsgRegistryException {

        final ClassificationType classificationType = new ClassificationType();
        classificationType.setId("someClassificationId");

        final Set<ClassificationType> classifications = Sets
                .<ClassificationType> newHashSet();
        classifications.add(classificationType);

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_VERSION);
        submitObjectsRequest.setCheckReferences(false);

        final RegistryObjectType registryObject = submitObjectsRequest
                .getRegistryObjects().iterator().next();
        registryObject.setClassification(classifications);

        lifecycleManager.submitObjects(submitObjectsRequest);
    }

    /**
     * Attribute checkReferences - true - Specifies that a server MUST check
     * submitted objects and make sure that all references via reference
     * attributes and slots to other RegistryObjects are resolvable. If a
     * reference does not resolve then the server MUST return
     * UnresolvedReferenceException
     */
    @Test
    public void checkReferencesTrueWithNonExistantLocalStaticFails()
            throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_VERSION);
        submitObjectsRequest.setCheckReferences(true);

        final OrganizationType organizationType = OrganizationTypeFixture.INSTANCE
                .get();
        // Local static reference as taken from ebXML 4.0 ebRIM specification
        // section 2.9.3.3
        organizationType.setPrimaryContact("urn:acme:person:Danyal");

        final List<RegistryObjectType> registryObjects = submitObjectsRequest
                .getRegistryObjects();
        registryObjects.clear();
        registryObjects.add(organizationType);

        expectFaultException(submitObjectsRequest,
                UnresolvedReferenceExceptionType.class);
    }

    /**
     * id - MUST be specified by client or else server MUST return
     * InvalidRequestException
     */
    @Test
    public void createOrReplaceWithoutIdFails() throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_REPLACE);
        submitObjectsRequest.getRegistryObjects().iterator().next().setId(null);

        expectFaultException(submitObjectsRequest,
                InvalidRequestExceptionType.class);
    }

    /**
     * id - MUST be specified by client or else server MUST return
     * InvalidRequestException
     */
    @Test
    public void createOrVersionWithoutIdFails() throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_VERSION);
        submitObjectsRequest.getRegistryObjects().iterator().next().setId(null);

        expectFaultException(submitObjectsRequest,
                InvalidRequestExceptionType.class);
    }

    /**
     * id - If unspecified Server MUST generate UUID URN
     */
    @Test
    public void createOnlyWithoutIdCreatesUUID() throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE, Mode.CREATE_ONLY);
        submitObjectsRequest.getRegistryObjects().iterator().next().setId(null);

        final RegistryResponseType response = lifecycleManager
                .submitObjects(submitObjectsRequest);

        final String id = response.getObjectRefList().getObjectRef().iterator()
                .next().getId();

        // Make sure it can be parsed as a UUID
        UUID.fromString(id);
    }

    /**
     * id - If id does not exists, server MUST create new object using that id
     * (create)
     */
    @Test
    public void createOnlyWithIdUsesGivenId() throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE, Mode.CREATE_ONLY);
        submitObjectsRequest.getRegistryObjects().iterator().next().setId(null);

        final RegistryResponseType response = lifecycleManager
                .submitObjects(submitObjectsRequest);

        assertThat(response.getObjectRefList().getObjectRef().iterator().next()
                .getId(), is(submitObjectsRequest.getRegistryObjects()
                .iterator().next().getId()));
    }

    /**
     * lid - MUST be specified by client or else server MUST return
     * InvalidRequestException
     */
    @Test
    public void createOrReplaceWithoutLidFails() throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_REPLACE);
        submitObjectsRequest.getRegistryObjects().iterator().next()
                .setLid(null);

        expectFaultException(submitObjectsRequest,
                InvalidRequestExceptionType.class);
    }

    /**
     * lid - MUST be specified by client or else server MUST return
     * InvalidRequestException
     */
    @Test
    public void createOrVersionWithoutLidFails() throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE,
                Mode.CREATE_OR_VERSION);
        submitObjectsRequest.getRegistryObjects().iterator().next()
                .setLid(null);

        expectFaultException(submitObjectsRequest,
                InvalidRequestExceptionType.class);
    }

    /**
     * lid - MUST be specified by client or else server MUST return
     * InvalidRequestException
     */
    @Test
    public void createOnlyWithoutLidFails() throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE, Mode.CREATE_ONLY);
        submitObjectsRequest.getRegistryObjects().iterator().next()
                .setLid(null);

        expectFaultException(submitObjectsRequest,
                InvalidRequestExceptionType.class);
    }

    /**
     * lid - MUST NOT exist or else server MUST return ObjectExistsException
     */
    @Test
    public void createOrVersionWithExistingLidFails()
            throws MsgRegistryException {

        SubmitObjectsRequest submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE, Mode.CREATE_ONLY);
        submitObjectsRequest.getRegistryObjects().iterator().next().setId(null);
        submitObjectsRequest.getRegistryObjects().iterator().next()
                .setLid("goingToSubmitTheSameLidTwice");

        lifecycleManager.submitObjects(submitObjectsRequest);

        submitObjectsRequest = createSubmitObjectsRequest(
                MY_REGISTRY_OBJECT_ID, REGISTRY_OBJECT_TYPE, Mode.CREATE_ONLY);
        submitObjectsRequest.getRegistryObjects().iterator().next().setId(null);
        submitObjectsRequest.getRegistryObjects().iterator().next()
                .setLid("goingToSubmitTheSameLidTwice");

        expectFaultException(submitObjectsRequest,
                ObjectExistsExceptionType.class);
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
    private <T extends RegistryExceptionType> void expectFaultException(
            final SubmitObjectsRequest submitObjectsRequest,
            Class<T> expectedException) {
        expectFaultException(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                lifecycleManager.submitObjects(submitObjectsRequest);
                return null;
            }
        }, expectedException);
    }
}
