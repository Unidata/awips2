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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationNodeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationSchemeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryObjectReferenceValidator;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * {@link Validator} plugin implementation for {@link ClassificationType}
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

public class ClassificationTypeValidator extends
        ValidatorPlugin<ClassificationType> {

    /**
     * Constructor.
     * 
     * @param registryObjectReferenceValidator
     */
    protected ClassificationTypeValidator(
            IRegistryObjectReferenceValidator registryObjectReferenceValidator) {
        super(registryObjectReferenceValidator);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<ClassificationType> getRegistryObjectTypeClass() {
        return ClassificationType.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void validate(ClassificationType registryObject,
            List<RegistryExceptionType> exceptions) {
        final String classificationScheme = registryObject
                .getClassificationScheme();
        final String classificationNode = registryObject
                .getClassificationNode();
        final String classifiedObject = registryObject.getClassifiedObject();
        final String nodeRepresentation = registryObject
                .getNodeRepresentation();

        if ((classificationScheme != null && classificationNode != null)
                || (classificationScheme == null && classificationNode == null)) {
            final String message = "One and only one of classificationNode or classificationScheme must be specified";
            exceptions.add(EbxmlExceptionUtil.createValidationExceptionType(
                    message, message).getFaultInfo());
        }

        final String registryObjectId = registryObject.getId();
        validateReferenceOfType(classificationScheme,
                ClassificationSchemeType.class, exceptions);
        validateReferenceOfType(classificationNode,
                ClassificationNodeType.class, exceptions);

        validateNotNull(classifiedObject, "classifiedObject", registryObjectId,
                exceptions);
        validateReference(classifiedObject, exceptions);

        // External classification type, node representation is required
        if (classificationScheme != null) {
            validateNotNull(nodeRepresentation, "nodeRepresentation",
                    registryObjectId, exceptions);
        }
    }

}
