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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryObjectReferenceValidator;

/**
 * {@link Validator} plugin implementation for {@link OrganizationType}
 * instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2013 1910       djohnson     Initial creation
 * May 02, 2013 1910       djohnson     Extracted reusable methods to parent class.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class OrganizationTypeValidator extends
        ValidatorPlugin<OrganizationType> {

    /**
     * Constructor.
     * 
     * @param registryObjectReferenceValidator
     *            the registry object reference validator
     */
    public OrganizationTypeValidator(
            IRegistryObjectReferenceValidator registryObjectReferenceValidator) {
        super(registryObjectReferenceValidator);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<OrganizationType> getRegistryObjectTypeClass() {
        return OrganizationType.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void validate(OrganizationType registryObject,
            List<RegistryExceptionType> exceptions) {

        validateReference(registryObject.getPrimaryContact(), exceptions);

    }

}
