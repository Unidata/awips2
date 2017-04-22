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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationNodeType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryObjectReferenceValidator;

/**
 * {@link Validator} plugin implementation for {@link AssociationType}
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

public class AssociationTypeValidator extends ValidatorPlugin<AssociationType> {

    /**
     * Constructor.
     * 
     * @param registryObjectReferenceValidator
     */
    protected AssociationTypeValidator(
            IRegistryObjectReferenceValidator registryObjectReferenceValidator) {
        super(registryObjectReferenceValidator);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<AssociationType> getRegistryObjectTypeClass() {
        return AssociationType.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void validate(AssociationType registryObject,
            List<RegistryExceptionType> exceptions) {
        final String sourceObject = registryObject.getSourceObject();
        final String targetObject = registryObject.getTargetObject();
        final String type = registryObject.getType();

        final String registryObjectId = registryObject.getId();
        validateNotNull(sourceObject, "sourceObject", registryObjectId,
                exceptions);
        validateNotNull(targetObject, "targetObject", registryObjectId,
                exceptions);
        validateNotNull(type, "type", registryObjectId, exceptions);

        validateReference(sourceObject, exceptions);
        validateReference(targetObject, exceptions);
        validateReferenceOfType(type, ClassificationNodeType.class, exceptions);
    }

}
