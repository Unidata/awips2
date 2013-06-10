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

import static org.mockito.Mockito.when;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationTypeFixture;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationNodeType;

import org.junit.Test;

import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryObjectReferenceValidator;
import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryObjectReferenceValidator.ValidateObjectTypeResponse;

/**
 * Test {@link AssociationTypeValidator}.
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

public class AssociationTypeValidatorTest
        extends
        AbstractRegistryObjectTypeValidatorTest<AssociationType, AssociationTypeValidator> {

    private final AssociationType registryObject = AssociationTypeFixture.INSTANCE
            .get();

    /**
     * Attribute sourceObject - Each Association MUST have a sourceObject
     * attribute that references the RegistryObjectType instance that is the
     * source of that Association.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnBadSourceObjectReference()
            throws MsgRegistryException {

        registryObject.setSourceObject(INVALID_LOCAL_STATIC_REFERENCE);

        expectUnresolvedReferenceExceptionReturned(registryObject);
    }

    /**
     * Attribute sourceObject - Each Association MUST have a sourceObject
     * attribute that references the RegistryObjectType instance that is the
     * source of that Association.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnNullSourceObjectReference()
            throws MsgRegistryException {

        registryObject.setSourceObject(null);

        expectValidationExceptionReturned(registryObject);
    }

    /**
     * Attribute sourceObject - Each Association MUST have a sourceObject
     * attribute that references the RegistryObjectType instance that is the
     * source of that Association.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void partialSuccessOnBadSourceObjectReference()
            throws MsgRegistryException {

        registryObject.setSourceObject(INVALID_LOCAL_STATIC_REFERENCE);

        expectPartialSuccessResponseStatus(registryObject);
    }

    /**
     * Attribute targetObject - Each Association MUST have a targetObject
     * attribute that references the RegistryObjectType instance that is the
     * target of that Association.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnBadTargetObjectReference()
            throws MsgRegistryException {

        registryObject.setTargetObject(INVALID_LOCAL_STATIC_REFERENCE);

        expectUnresolvedReferenceExceptionReturned(registryObject);
    }

    /**
     * Attribute targetObject - Each Association MUST have a targetObject
     * attribute that references the RegistryObjectType instance that is the
     * target of that Association.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnNullTargetObjectReference()
            throws MsgRegistryException {

        registryObject.setTargetObject(null);

        expectValidationExceptionReturned(registryObject);
    }

    /**
     * Attribute targetObject - Each Association MUST have a targetObject
     * attribute that references the RegistryObjectType instance that is the
     * target of that Association.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void partialSuccessOnBadTargetObjectReference()
            throws MsgRegistryException {

        registryObject.setTargetObject(INVALID_LOCAL_STATIC_REFERENCE);

        expectPartialSuccessResponseStatus(registryObject);
    }

    /**
     * Attribute type - Each Association MUST have a type attribute that
     * identifies the type of that association.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnBadTypeReference() throws MsgRegistryException {

        registryObject.setType(INVALID_LOCAL_STATIC_REFERENCE);

        expectUnresolvedReferenceExceptionReturned(registryObject);
    }

    /**
     * Attribute type - Each Association MUST have a type attribute that
     * identifies the type of that association.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnNullTypeReference() throws MsgRegistryException {

        registryObject.setType(null);

        expectValidationExceptionReturned(registryObject);
    }

    /**
     * Attribute type - Each Association MUST have a type attribute that
     * identifies the type of that association.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void partialSuccessOnBadTypeReference() throws MsgRegistryException {

        registryObject.setType(INVALID_LOCAL_STATIC_REFERENCE);

        expectPartialSuccessResponseStatus(registryObject);
    }

    /**
     * The value of the type attribute MUST be a reference to a
     * ClassificationNode within the canonical AssociationType
     * ClassificationScheme.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnGoodTypeReferenceToWrongObjectType()
            throws MsgRegistryException {

        when(
                mockReferenceValidator.isValidObjectType(
                        registryObject.getType(), ClassificationNodeType.class))
                .thenReturn(ValidateObjectTypeResponse.WRONG_TYPE);

        expectValidationExceptionReturned(registryObject);
    }

    /**
     * Attribute type - Each Association MUST have a type attribute that
     * identifies the type of that association.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void partialSuccessOnGoodTypeReferenceToWrongObjectType()
            throws MsgRegistryException {

        when(
                mockReferenceValidator.isValidObjectType(
                        registryObject.getType(), ClassificationNodeType.class))
                .thenReturn(ValidateObjectTypeResponse.WRONG_TYPE);

        expectPartialSuccessResponseStatus(registryObject);
    }

    @Test
    public void noExceptionsOnAllGoodReferences() throws MsgRegistryException {
        expectNoExceptionsReturned(registryObject);
    }

    @Test
    public void successOnAllGoodReferences() throws MsgRegistryException {
        expectSuccessResponseStatus(registryObject);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected AssociationTypeValidator getValidator(
            IRegistryObjectReferenceValidator referenceValidator) {
        return new AssociationTypeValidator(referenceValidator);
    }

}
