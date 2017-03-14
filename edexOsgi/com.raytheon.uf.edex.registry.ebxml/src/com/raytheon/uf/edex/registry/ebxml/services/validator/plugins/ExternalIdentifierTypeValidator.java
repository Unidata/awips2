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

import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.Validator;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExternalIdentifierType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryObjectReferenceValidator;

/**
 * {@link Validator} plugin implementation for {@link ExternalIdentifierType}
 * instances.
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

public class ExternalIdentifierTypeValidator extends
        ValidatorPlugin<ExternalIdentifierType> {

    /**
     * Constructor.
     * 
     * @param registryObjectReferenceValidator
     */
    protected ExternalIdentifierTypeValidator(
            IRegistryObjectReferenceValidator registryObjectReferenceValidator) {
        super(registryObjectReferenceValidator);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<ExternalIdentifierType> getRegistryObjectTypeClass() {
        return ExternalIdentifierType.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void validate(ExternalIdentifierType registryObject,
            List<RegistryExceptionType> exceptions) {
        final String registryObjectId = registryObject.getId();
        final String identificationScheme = registryObject
                .getIdentificationScheme();

        validateNotNull(registryObject.getValue(), "value", registryObjectId,
                exceptions);
        validateNotNull(identificationScheme, "identificationScheme",
                registryObjectId, exceptions);

        validateReference(identificationScheme, exceptions);
        validateReference(registryObject.getRegistryObject(), exceptions);
    }

}
