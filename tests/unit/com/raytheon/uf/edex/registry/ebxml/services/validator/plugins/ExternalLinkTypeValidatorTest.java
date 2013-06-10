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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExternalLinkType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExternalLinkTypeFixture;

import org.junit.Test;

import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryObjectReferenceValidator;

/**
 * Test {@link ExternalLinkTypeValidator}.
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
public class ExternalLinkTypeValidatorTest
        extends
        AbstractRegistryObjectTypeValidatorTest<ExternalLinkType, ExternalLinkTypeValidator> {

    private final ExternalLinkType registryObject = ExternalLinkTypeFixture.INSTANCE
            .get();

    /**
     * Element ExternalRef - Each ExternalLink instance MUST have an ExternalRef
     * sub-element defined. This element provides a URI to the external resource
     * pointed to by this ExternalLink instance.
     * 
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void exceptionOnNullExternalRef() throws MsgRegistryException {

        registryObject.setExternalRef(null);

        expectValidationExceptionReturned(registryObject);
    }

    // TODO: Do we need to test for certain invariants on a link?

    /**
     * Element ExternalRef - Each ExternalLink instance MUST have an ExternalRef
     * sub-element defined. This element provides a URI to the external resource
     * pointed to by this ExternalLink instance.
     * 
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void partialSuccessOnNullExternalRef()
            throws MsgRegistryException {

        registryObject.setExternalRef(null);

        expectPartialSuccessResponseStatus(registryObject);
    }

    /**
     * Attribute registryObject - references the parent RegistryObjectType
     * instance within which the ExtrnalLink - Type instance is composed. The
     * value MUST be provided by client when an ExtrenalLink is submitted
     * separate from its parent object. The value MUST be set by the server if
     * the ExternalLink is submitted as part of the submission of its parent
     * object.
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
     * Attribute registryObject - references the parent RegistryObjectType
     * instance within which the ExtrnalLink - Type instance is composed. The
     * value MUST be provided by client when an ExtrenalLink is submitted
     * separate from its parent object. The value MUST be set by the server if
     * the ExternalLink is submitted as part of the submission of its parent
     * object.
     * 
     * @throws MsgRegistryException
     */
    @Test
    public void partialSuccessOnBadRegistryObjectReference()
            throws MsgRegistryException {

        registryObject.setRegistryObject(INVALID_LOCAL_STATIC_REFERENCE);

        expectPartialSuccessResponseStatus(registryObject);
    }

    @Test
    public void noExceptionOnGoodReferences() throws MsgRegistryException {

        expectNoExceptionsReturned(registryObject);
    }

    @Test
    public void successOnGoodReferences() throws MsgRegistryException {

        expectSuccessResponseStatus(registryObject);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected ExternalLinkTypeValidator getValidator(
            IRegistryObjectReferenceValidator referenceValidator) {
        return new ExternalLinkTypeValidator(referenceValidator);
    }
}
