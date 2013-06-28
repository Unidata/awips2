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
package com.raytheon.uf.edex.registry.ebxml.services.validator;

import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertThat;

import java.util.Arrays;
import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationTypeFixture;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnresolvedReferenceExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.raytheon.uf.edex.registry.ebxml.dao.AbstractRegistryTest;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * Test {@link ValidatorImpl}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2013 1910       djohnson     Initial creation
 * Apr 29, 2013 1910       djohnson     Move to integration tests section.
 * May 02, 2013 1910       djohnson     Add validator plugins spring file.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class ValidatorImplTest extends AbstractRegistryTest {

    private static final String LOCAL_STATIC_REFERENCE = "urn:acme:person:Danyal";

    @Autowired
    private ValidatorImpl validator;

    /**
     * Section 5.2.1 Validator Plugin Interface - The server selects the
     * RegistryObjects that are the target of the validateObjects operations
     * using the <spi:Query> and <rim:ObjectRefList> elements. Any objects
     * specified by the OriginalObjects element MUST be ignored by the server.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void clientSpecifyingOriginalObjectsAreIgnored()
            throws MsgRegistryException {

        ValidateObjectsRequest request = new ValidateObjectsRequest();
        request.setObjectRefList(null);
        // This object would fail validation if it were validated
        request.setOriginalObjects(new RegistryObjectListType(
                Arrays.<RegistryObjectType> asList(organizationWithLocalStaticReference())));

        final ValidateObjectsResponse response = validator
                .validateObjects(request);
        assertSuccessfulResponse(response);
    }

    /**
     * Element OriginalObjects - Specifies a collection of RegistryObject
     * instances. A server MUST validate all objects that are contained in this
     * element. This element is typically used when a server initiates the
     * validateObjects protocol during the processing of a submitObjects or
     * updateObjects protocol request or when it is delegating a client
     * initiated validateObjects protocol request to a Validator plugin.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void invalidLocalStaticReferenceReturnsPartialSuccessStatus()
            throws MsgRegistryException {
        final OrganizationType organizationType = organizationWithLocalStaticReference();

        final ValidateObjectsResponse response = validateObject(organizationType);

        assertThat(response.getStatus(),
                is(equalTo(RegistryResponseStatus.PARTIAL_SUCCESS)));
    }

    /**
     * Element OriginalObjects - Specifies a collection of RegistryObject
     * instances. A server MUST validate all objects that are contained in this
     * element. This element is typically used when a server initiates the
     * validateObjects protocol during the processing of a submitObjects or
     * updateObjects protocol request or when it is delegating a client
     * initiated validateObjects protocol request to a Validator plugin.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void validLocalStaticReferenceReturnsSuccessStatus()
            throws MsgRegistryException {
        addReferencedObjectToRegistry();
        
        final OrganizationType organizationType = organizationWithLocalStaticReference();

        final ValidateObjectsResponse response = validateObject(organizationType);

        assertThat(response.getStatus(),
                is(equalTo(RegistryResponseStatus.SUCCESS)));
    }

    /**
     * Element OriginalObjects - Specifies a collection of RegistryObject
     * instances. A server MUST validate all objects that are contained in this
     * element. This element is typically used when a server initiates the
     * validateObjects protocol during the processing of a submitObjects or
     * updateObjects protocol request or when it is delegating a client
     * initiated validateObjects protocol request to a Validator plugin.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void invalidLocalStaticReferenceReturnsUnresolvedReferenceException()
            throws MsgRegistryException {
        final OrganizationType organizationType = organizationWithLocalStaticReference();

        final ValidateObjectsResponse response = validateObject(organizationType);
        final List<RegistryExceptionType> exceptions = response.getException();

        assertThat(exceptions, is(not(empty())));
        assertThat(exceptions.iterator().next(),
                is(instanceOf(UnresolvedReferenceExceptionType.class)));
    }

    /**
     * Element OriginalObjects - Specifies a collection of RegistryObject
     * instances. A server MUST validate all objects that are contained in this
     * element. This element is typically used when a server initiates the
     * validateObjects protocol during the processing of a submitObjects or
     * updateObjects protocol request or when it is delegating a client
     * initiated validateObjects protocol request to a Validator plugin.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void validLocalStaticReferenceDoesNotReturnExceptions()
            throws MsgRegistryException {
        addReferencedObjectToRegistry();

        final OrganizationType organizationType = organizationWithLocalStaticReference();

        final ValidateObjectsResponse response = validateObject(organizationType);

        assertThat(response.getException(), is(empty()));
    }

    /**
     * Element Query - Specifies a query to be invoked. A server MUST validate
     * all objects that match the specified query. This element is typically
     * used when a client initiates the validateObjects protocol.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void invalidLocalStaticReferenceViaObjectQueryReturnsUnresolvedReferenceException()
            throws MsgRegistryException {
        final OrganizationType organizationType = organizationWithLocalStaticReference();

        submitRegistryObjectToRegistry(organizationType);

        final ValidateObjectsResponse validateResponse = validateViaQuery(organizationType);
        final List<RegistryExceptionType> exceptions = validateResponse
                .getException();

        assertThat(exceptions, is(not(empty())));
        assertThat(exceptions.iterator().next(),
                is(instanceOf(UnresolvedReferenceExceptionType.class)));
    }

    /**
     * Element Query - Specifies a query to be invoked. A server MUST validate
     * all objects that match the specified query. This element is typically
     * used when a client initiates the validateObjects protocol.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void validLocalStaticReferenceViaObjectQueryDoesNotReturnExceptions()
            throws MsgRegistryException {
        addReferencedObjectToRegistry();

        final OrganizationType organizationType = organizationWithLocalStaticReference();

        submitRegistryObjectToRegistry(organizationType);

        final ValidateObjectsResponse validateResponse = validateViaQuery(organizationType);

        assertThat(validateResponse.getException(), is(empty()));
    }

    /**
     * Element Query - Specifies a query to be invoked. A server MUST validate
     * all objects that match the specified query. This element is typically
     * used when a client initiates the validateObjects protocol.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void invalidLocalStaticReferenceViaObjectQueryReturnsPartialSuccessStatus()
            throws MsgRegistryException {
        final OrganizationType organizationType = organizationWithLocalStaticReference();

        submitRegistryObjectToRegistry(organizationType);

        final ValidateObjectsResponse validateResponse = validateViaQuery(organizationType);

        assertThat(validateResponse.getStatus(),
                is(equalTo(RegistryResponseStatus.PARTIAL_SUCCESS)));
    }

    /**
     * Element Query - Specifies a query to be invoked. A server MUST validate
     * all objects that match the specified query. This element is typically
     * used when a client initiates the validateObjects protocol.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void validLocalStaticReferenceViaObjectQueryReturnsSuccessStatus()
            throws MsgRegistryException {
        addReferencedObjectToRegistry();

        final OrganizationType organizationType = organizationWithLocalStaticReference();

        submitRegistryObjectToRegistry(organizationType);

        final ValidateObjectsResponse validateResponse = validateViaQuery(organizationType);

        assertThat(validateResponse.getStatus(),
                is(equalTo(RegistryResponseStatus.SUCCESS)));
    }

    // TODO: Add tests for remote and dynamic references

    /**
     * Adds the referenced object to the registry.
     * 
     * @throws MsgRegistryException
     */
    private void addReferencedObjectToRegistry() throws MsgRegistryException {
        final RegistryObjectType registryObjectType = new RegistryObjectType(
                LOCAL_STATIC_REFERENCE, LOCAL_STATIC_REFERENCE);
        submitRegistryObjectToRegistry(registryObjectType);
    }

    /**
     * Returns an {@link OrganizationType} instance with a local static
     * reference.
     * 
     * @return the {@link OrganizationType}
     */
    private OrganizationType organizationWithLocalStaticReference() {
        final OrganizationType organizationType = OrganizationTypeFixture.INSTANCE
                .get();
        // Local static reference as taken from ebXML 4.0 ebRIM specification
        // section 2.9.3.3
        organizationType.setPrimaryContact(LOCAL_STATIC_REFERENCE);
        return organizationType;
    }

    private ValidateObjectsResponse validateViaQuery(
            final OrganizationType organizationType)
            throws MsgRegistryException {
        final QueryType queryType = createQueryForRegistryObjectByLid(
                organizationType.getLid()).getQuery();

        final ValidateObjectsRequest validateRequest = new ValidateObjectsRequest();
        validateRequest.setQuery(queryType);

        final ValidateObjectsResponse validateResponse = validator
                .validateObjects(validateRequest);
        return validateResponse;
    }

    /**
     * Submit the object to the validation service.
     * 
     * @param registryObject
     *            the registry object
     * 
     * @return the validation response
     * @throws MsgRegistryException
     *             on error
     */
    private ValidateObjectsResponse validateObject(
            RegistryObjectType registryObject) throws MsgRegistryException {
        ValidateObjectsRequest request = new ValidateObjectsRequest();
        request.setOriginalObjects(new RegistryObjectListType(Arrays
                .asList(registryObject)));

        return validator.serverValidateObjects(request,
                EbxmlObjectUtil.spiObjectFactory
                        .createValidateObjectsResponse());
    }

}
