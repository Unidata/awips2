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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationTypeFixture;

import org.junit.Test;

import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryObjectReferenceValidator;

/**
 * Test {@link OrganizationTypeValidator}.
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
public class OrganizationTypeValidatorTest
        extends
        AbstractRegistryObjectTypeValidatorTest<OrganizationType, OrganizationTypeValidator> {

    private final OrganizationType registryObject = OrganizationTypeFixture.INSTANCE
            .get();

    @Test
    public void exceptionOnBadPrimaryContactReference()
            throws MsgRegistryException {

        registryObject.setPrimaryContact(INVALID_LOCAL_STATIC_REFERENCE);

        expectUnresolvedReferenceExceptionReturned(registryObject);
    }

    @Test
    public void partialSuccessOnBadPrimaryContactReference()
            throws MsgRegistryException {

        registryObject.setPrimaryContact(INVALID_LOCAL_STATIC_REFERENCE);

        expectPartialSuccessResponseStatus(registryObject);
    }

    @Test
    public void noExceptionOnGoodPrimaryContactReference()
            throws MsgRegistryException {

        registryObject.setPrimaryContact(VALID_LOCAL_STATIC_REFERENCE);

        expectNoExceptionsReturned(registryObject);
    }


    @Test
    public void successOnGoodPrimaryContactReference()
            throws MsgRegistryException {

        registryObject.setPrimaryContact(VALID_LOCAL_STATIC_REFERENCE);

        expectSuccessResponseStatus(registryObject);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected OrganizationTypeValidator getValidator(
            IRegistryObjectReferenceValidator referenceValidator) {
        return new OrganizationTypeValidator(referenceValidator);
    }
}
