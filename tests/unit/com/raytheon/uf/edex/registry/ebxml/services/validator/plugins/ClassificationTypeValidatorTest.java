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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationTypeFixture;

import org.junit.Test;

import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryObjectReferenceValidator;

/**
 * Test {@link ClassificationTypeValidator}.
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
public class ClassificationTypeValidatorTest
        extends
        AbstractRegistryObjectTypeValidatorTest<ClassificationType, ClassificationTypeValidator> {

    private final ClassificationType registryObject = ClassificationTypeFixture.INSTANCE
            .get();

    /**
     * Attribute classificationNode - If the ClassificationType instance
     * represents an internal classification, then the classificationNode
     * attribute is required. The classificationNode value MUST reference a
     * ClassificationNodeType instance.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnBadClassificationNodeReference()
            throws MsgRegistryException {

        registryObject.setClassificationNode(INVALID_LOCAL_STATIC_REFERENCE);

        expectUnresolvedReferenceExceptionReturned(registryObject);
    }

    /**
     * Attribute classificationNode - If the ClassificationType instance
     * represents an internal classification, then the classificationNode
     * attribute is required. The classificationNode value MUST reference a
     * ClassificationNodeType instance.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void partialSuccessOnBadClassificationNodeReference()
            throws MsgRegistryException {

        registryObject.setClassificationNode(INVALID_LOCAL_STATIC_REFERENCE);

        expectPartialSuccessResponseStatus(registryObject);
    }

    /**
     * Attribute nodeRepresentation - If the ClassificationType instance
     * represents an external classification, then the nodeRepresentation
     * attribute is required. It is a representation of a taxonomy value from a
     * classification scheme.
     * 
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnNullNodeRepresentationWhenClassificationSchemeIsNotNull()
            throws MsgRegistryException {

        registryObject.setNodeRepresentation(null);
        registryObject.setClassificationScheme(VALID_LOCAL_STATIC_REFERENCE);
        registryObject.setClassificationNode(null);

        expectValidationExceptionReturned(registryObject);
    }

    /**
     * Attribute nodeRepresentation - If the ClassificationType instance
     * represents an external classification, then the nodeRepresentation
     * attribute is required. It is a representation of a taxonomy value from a
     * classification scheme.
     * 
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void partialSuccessOnNullNodeRepresentationWhenClassificationSchemeIsNotNull()
            throws MsgRegistryException {

        registryObject.setNodeRepresentation(null);
        registryObject.setClassificationScheme(VALID_LOCAL_STATIC_REFERENCE);
        registryObject.setClassificationNode(null);

        expectPartialSuccessResponseStatus(registryObject);
    }

    /**
     * Attribute classifiedObject - For both internal and external
     * classifications, the classifiedObject attribute is required and it
     * references the RegistryObjectType instance that is classified by this
     * Classification.
     * 
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnBadClassifiedObjectReference()
            throws MsgRegistryException {

        registryObject.setClassifiedObject(INVALID_LOCAL_STATIC_REFERENCE);

        expectUnresolvedReferenceExceptionReturned(registryObject);
    }

    /**
     * Attribute classifiedObject - For both internal and external
     * classifications, the classifiedObject attribute is required and it
     * references the RegistryObjectType instance that is classified by this
     * Classification.
     * 
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void partialSuccessOnBadClassifiedObjectReference()
            throws MsgRegistryException {

        registryObject.setClassifiedObject(INVALID_LOCAL_STATIC_REFERENCE);

        expectPartialSuccessResponseStatus(registryObject);
    }

    @Test
    public void exceptionWhenInternalAndExternalFieldsSpecified()
            throws MsgRegistryException {

        registryObject.setClassificationScheme(VALID_LOCAL_STATIC_REFERENCE);
        registryObject.setClassificationNode(VALID_LOCAL_STATIC_REFERENCE);
        registryObject.setNodeRepresentation(VALID_LOCAL_STATIC_REFERENCE);

        expectValidationExceptionReturned(registryObject);
    }

    @Test
    public void partialSuccessWhenInternalAndExternalFieldsSpecified()
            throws MsgRegistryException {

        registryObject.setClassificationScheme(VALID_LOCAL_STATIC_REFERENCE);
        registryObject.setClassificationNode(VALID_LOCAL_STATIC_REFERENCE);
        registryObject.setNodeRepresentation(VALID_LOCAL_STATIC_REFERENCE);

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
    protected ClassificationTypeValidator getValidator(
            IRegistryObjectReferenceValidator referenceValidator) {
        return new ClassificationTypeValidator(referenceValidator);
    }
}
