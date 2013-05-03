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

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExternalIdentifierType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExternalIdentifierTypeFixture;

import org.junit.Test;

import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryObjectReferenceValidator;

/**
 * Test {@link ExternalIdentifierTypeValidator}.
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
public class ExternalIdentifierTypeValidatorTest
        extends
        AbstractRegistryObjectTypeValidatorTest<ExternalIdentifierType, ExternalIdentifierTypeValidator> {

    private final ExternalIdentifierType registryObject = ExternalIdentifierTypeFixture.INSTANCE
            .get();

    /**
     * Attribute identificationScheme - Each ExternalIdentifier instance MUST
     * have an identificationScheme attribute that references a
     * ClassificationScheme. This ClassificationScheme defines the namespace
     * within which an identifier is defined using the value attribute for the
     * RegistryObjectType instance referenced by the RegistryObject attribute.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnBadIdentificationSchemeReference()
            throws MsgRegistryException {

        registryObject.setIdentificationScheme(INVALID_LOCAL_STATIC_REFERENCE);

        expectUnresolvedReferenceExceptionReturned(registryObject);
    }

    /**
     * Attribute identificationScheme - Each ExternalIdentifier instance MUST
     * have an identificationScheme attribute that references a
     * ClassificationScheme. This ClassificationScheme defines the namespace
     * within which an identifier is defined using the value attribute for the
     * RegistryObjectType instance referenced by the RegistryObject attribute.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void partialSuccessOnBadIdentificationSchemeReference()
            throws MsgRegistryException {

        registryObject.setIdentificationScheme(INVALID_LOCAL_STATIC_REFERENCE);

        expectPartialSuccessResponseStatus(registryObject);
    }

    /**
     * Attribute registryObject - Each ExternalIdentifier instance MAY have a
     * registryObject attribute specified. This attribute references the parent
     * RegistryObjectType instance for which this is an ExternalIdentifier.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnBadRegistryObjectReference()
            throws MsgRegistryException {

        registryObject.setRegistryObject(INVALID_LOCAL_STATIC_REFERENCE);

        expectUnresolvedReferenceExceptionReturned(registryObject);
    }

    /**
     * Attribute registryObject - Each ExternalIdentifier instance MAY have a
     * registryObject attribute specified. This attribute references the parent
     * RegistryObjectType instance for which this is an ExternalIdentifier.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void partialSuccessOnBadRegistryObjectReference()
            throws MsgRegistryException {

        registryObject.setRegistryObject(INVALID_LOCAL_STATIC_REFERENCE);

        expectPartialSuccessResponseStatus(registryObject);
    }

    /**
     * Attribute identificationScheme - Each ExternalIdentifier instance MUST
     * have an identificationScheme attribute that references a
     * ClassificationScheme. This ClassificationScheme defines the namespace
     * within which an identifier is defined using the value attribute for the
     * RegistryObjectType instance referenced by the RegistryObject attribute.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnNullIdentificationSchemeReference()
            throws MsgRegistryException {

        registryObject.setIdentificationScheme(null);

        expectValidationExceptionReturned(registryObject);
    }

    /**
     * Attribute identificationScheme - Each ExternalIdentifier instance MUST
     * have an identificationScheme attribute that references a
     * ClassificationScheme. This ClassificationScheme defines the namespace
     * within which an identifier is defined using the value attribute for the
     * RegistryObjectType instance referenced by the RegistryObject attribute.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void partialSuccessOnNullIdentificationSchemeReference()
            throws MsgRegistryException {

        registryObject.setIdentificationScheme(null);

        expectPartialSuccessResponseStatus(registryObject);
    }

    /**
     * Attribute value - Each ExternalIdentifier instance MUST have a value
     * attribute that provides the identifier value for this ExternalIdentifier
     * (e.g., the tax payer id in example above).
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnNullValue() throws MsgRegistryException {

        registryObject.setValue(null);

        expectValidationExceptionReturned(registryObject);
    }

    /**
     * Attribute value - Each ExternalIdentifier instance MUST have a value
     * attribute that provides the identifier value for this ExternalIdentifier
     * (e.g., the tax payer id in example above).
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void partialSuccessOnNullValue() throws MsgRegistryException {

        registryObject.setValue(null);

        expectPartialSuccessResponseStatus(registryObject);
    }

    @Test
    public void noExceptionOnGoodReferences()
            throws MsgRegistryException {

        expectNoExceptionsReturned(registryObject);
    }

    @Test
    public void successOnGoodReferences()
            throws MsgRegistryException {

        expectSuccessResponseStatus(registryObject);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected ExternalIdentifierTypeValidator getValidator(
            IRegistryObjectReferenceValidator referenceValidator) {
        return new ExternalIdentifierTypeValidator(referenceValidator);
    }
}
