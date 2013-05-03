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
package com.raytheon.uf.edex.registry.ebxml.services.validator.plugins;

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.InvalidRequestExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnresolvedReferenceExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidationExceptionType;

import org.junit.Before;
import org.junit.Ignore;

import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryObjectReferenceValidator;
import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryObjectReferenceValidator.ValidateObjectTypeResponse;

/**
 * Base test for {@link ValidatorPlugin}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 25, 2013 1910       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public abstract class AbstractRegistryObjectTypeValidatorTest<REGISTRY_OBJECT extends RegistryObjectType, VALIDATOR extends ValidatorPlugin<REGISTRY_OBJECT>> {

    // Local static reference as taken from ebXML 4.0 ebRIM specification
    // section 2.9.3.3
    protected static final String INVALID_LOCAL_STATIC_REFERENCE = "urn:acme:person:Danyal";

    protected static final String VALID_LOCAL_STATIC_REFERENCE = "urn:acme:person:SomeDude";

    protected final IRegistryObjectReferenceValidator mockReferenceValidator = mock(IRegistryObjectReferenceValidator.class);

    protected final VALIDATOR validator = getValidator(mockReferenceValidator);

    @SuppressWarnings("unchecked")
    @Before
    public void setUp() {
        // By default, all references will resolve as valid
        when(mockReferenceValidator.isValidReference(anyString())).thenReturn(
                true);
        when(mockReferenceValidator.isValidObjectType(anyString(),
                        any(Class.class))).thenReturn(
                ValidateObjectTypeResponse.VALID);
        // The local static reference will resolve as bad whenever it's used
        when(
                mockReferenceValidator
                        .isValidReference(INVALID_LOCAL_STATIC_REFERENCE))
                .thenReturn(false);
        when(mockReferenceValidator.isValidObjectType(
                        eq(INVALID_LOCAL_STATIC_REFERENCE), any(Class.class)))
                .thenReturn(ValidateObjectTypeResponse.DOESNT_EXIST);
    }

    /**
     * Convenience method to perform validation on a registry object, and assert
     * that an {@link UnresolvedReferenceExceptionType} was returned in the
     * response.
     * 
     * @param registryObject
     *            the registry object to validate
     * @throws MsgRegistryException
     */
    protected void expectUnresolvedReferenceExceptionReturned(
            REGISTRY_OBJECT registryObject) throws MsgRegistryException {
        expectExceptionReturned(registryObject,
                UnresolvedReferenceExceptionType.class);
    }

    /**
     * Convenience method to perform validation on a registry object, and assert
     * that an {@link UnresolvedReferenceExceptionType} was returned in the
     * response.
     * 
     * @param registryObject
     *            the registry object to validate
     * @throws MsgRegistryException
     */
    protected void expectInvalidRequestExceptionReturned(
            REGISTRY_OBJECT registryObject) throws MsgRegistryException {
        expectExceptionReturned(registryObject,
                InvalidRequestExceptionType.class);
    }

    /**
     * Convenience method to perform validation on a registry object, and assert
     * that a {@link ValidationExceptionType} was returned in the response.
     * 
     * @param registryObject
     *            the registry object to validate
     * @throws MsgRegistryException
     */
    protected void expectValidationExceptionReturned(
            REGISTRY_OBJECT registryObject) throws MsgRegistryException {
        expectExceptionReturned(registryObject, ValidationExceptionType.class);
    }

    /**
     * Convenience method to perform validation on a registry object, and assert
     * that an {@link UnresolvedReferenceExceptionType} was returned in the
     * response.
     * 
     * @param registryObject
     *            the registry object to validate
     * @throws MsgRegistryException
     */
    private <T extends RegistryExceptionType> void expectExceptionReturned(
            REGISTRY_OBJECT registryObject, Class<T> exceptionType)
            throws MsgRegistryException {

        final ValidateObjectsResponse response = validator
                .validateObjects(createValidationRequest(registryObject));
        assertThat(response.getException(), contains(instanceOf(exceptionType)));
    }

    /**
     * Convenience method to perform validation on a registry object, and assert
     * that a {@link RegistryResponseStatus#PARTIAL_SUCCESS} was returned in the
     * response.
     * 
     * @param registryObject
     *            the registry object to validate
     * @throws MsgRegistryException
     */
    protected void expectPartialSuccessResponseStatus(
            REGISTRY_OBJECT registryObject) throws MsgRegistryException {

        final ValidateObjectsResponse response = validator
                .validateObjects(createValidationRequest(registryObject));
        assertThat(response.getStatus(),
                is(RegistryResponseStatus.PARTIAL_SUCCESS));
    }

    /**
     * Convenience method to perform validation on a registry object, and assert
     * that no {@link RegistryExceptionType}s were returned in the response.
     * 
     * @param registryObject
     *            the registry object to validate
     * @throws MsgRegistryException
     */
    protected void expectNoExceptionsReturned(REGISTRY_OBJECT registryObject)
            throws MsgRegistryException {

        final ValidateObjectsResponse response = validator
                .validateObjects(createValidationRequest(registryObject));
        assertThat(response.getException(), is(empty()));
    }

    /**
     * Convenience method to perform validation on a registry object, and assert
     * that a {@link RegistryResponseStatus#SUCCESS} was returned in the
     * response.
     * 
     * @param registryObject
     *            the registry object to validate
     * @throws MsgRegistryException
     */
    protected void expectSuccessResponseStatus(REGISTRY_OBJECT registryObject)
            throws MsgRegistryException {

        final ValidateObjectsResponse response = validator
                .validateObjects(createValidationRequest(registryObject));
        assertThat(response.getStatus(), is(RegistryResponseStatus.SUCCESS));
    }

    /**
     * Create the validation request.
     * 
     * @param registryObject
     *            the registry object to validate
     * @return the request
     */
    protected ValidateObjectsRequest createValidationRequest(
            REGISTRY_OBJECT registryObject) {
        ValidateObjectsRequest request = new ValidateObjectsRequest();
        request.setOriginalObjects(new RegistryObjectListType(Arrays
                .<RegistryObjectType> asList(registryObject)));
        return request;
    }

    /**
     * Get the validator to test.
     * 
     * @param referenceValidator
     * @return the validator instance
     */
    protected abstract VALIDATOR getValidator(
            IRegistryObjectReferenceValidator referenceValidator);
}
