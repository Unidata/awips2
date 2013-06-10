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
import java.util.Set;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExternalIdentifierType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExternalLinkType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryObjectReferenceValidator;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * Performs basic {@link RegistryObjectType} validation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2013 1910       djohnson     Initial creation
 * May 02, 2013 1910       djohnson     Extract reusable code to parent class.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class RegistryObjectTypeValidator extends
        ValidatorPlugin<RegistryObjectType> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryObjectTypeValidator.class);

    /**
     * Constructor.
     * 
     * @param registryObjectReferenceValidator
     */
    public RegistryObjectTypeValidator(
            IRegistryObjectReferenceValidator registryObjectReferenceValidator) {
        super(registryObjectReferenceValidator);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Class<RegistryObjectType> getRegistryObjectTypeClass() {
        return RegistryObjectType.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void validate(RegistryObjectType registryObject,
            List<RegistryExceptionType> exceptions) {
        resolveReferences(registryObject, registryObject.getId(), exceptions);
    }

    /**
     * Checks the specified object to ensure that all references via references
     * attributes and slots to other RegistryObjects are resolvable
     * 
     * @param object
     *            The object to check
     * @param originalId
     *            A record of the original object's id as this id will not need
     *            to pass the check since it is the id of the object being
     *            submitted
     * @throws MsgRegistryException
     *             If errors occur while querying the registry, or there is an
     *             unresolvable property
     */
    private void resolveReferences(RegistryObjectType object,
            String originalId, List<RegistryExceptionType> exceptions) {
        final String objectId = object.getId();
        statusHandler.info("Checking references for object with id ["
                + objectId + "]...");
        Set<ClassificationType> classifications = object.getClassification();
        if (classifications != null) {
            for (ClassificationType classification : classifications) {
                resolveReferences(classification, originalId, exceptions);
            }
        }
        Set<ExternalIdentifierType> externIdents = object
                .getExternalIdentifier();
        if (externIdents != null) {
            for (ExternalIdentifierType externIdent : externIdents) {
                resolveReferences(externIdent, originalId, exceptions);
            }
        }
        Set<ExternalLinkType> externLinks = object.getExternalLink();
        if (externLinks != null) {
            for (ExternalLinkType externLink : externLinks) {
                resolveReferences(externLink, originalId, exceptions);
            }
        }

        if (!objectId.equals(originalId)) {
            boolean objectReferenceValid = registryObjectReferenceValidator
                    .isValidReference(objectId);

            if (!objectReferenceValid) {
                exceptions.add(EbxmlExceptionUtil
                        .createUnresolvedReferenceException(object.getClass(),
                                objectId, statusHandler));
            } else {
                statusHandler
                        .info("References successfully resolve for object with id ["
                                + objectId + "]");

            }
        }
    }

}
