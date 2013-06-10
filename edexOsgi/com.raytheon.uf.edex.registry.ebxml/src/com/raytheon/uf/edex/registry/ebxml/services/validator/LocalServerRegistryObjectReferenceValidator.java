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
package com.raytheon.uf.edex.registry.ebxml.services.validator;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;

/**
 * {@link IRegistryObjectReferenceValidator} implementation when checking
 * references on the same server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2013 1910       djohnson     Initial creation
 * May 02, 2013 1910       djohnson     Add ability to validate registry object type.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Transactional
public class LocalServerRegistryObjectReferenceValidator implements
        IRegistryObjectReferenceValidator {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LocalServerRegistryObjectReferenceValidator.class);

    /** The registry object data access object */
    private RegistryObjectDao registryObjectDao;

    /**
     * Constructor.
     */
    public LocalServerRegistryObjectReferenceValidator() {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isValidReference(final String referenceId) {
        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler
                    .debug("Validating reference id [" + referenceId + "]");
        }
        return registryObjectDao.getById(referenceId) != null;
    }

    /**
     * @param registryObjectDao
     *            the registryObjectDao to set
     */
    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ValidateObjectTypeResponse isValidObjectType(String reference,
            Class<? extends RegistryObjectType> expectedType) {
        final RegistryObjectType registryObject = registryObjectDao
                .getById(reference);
        if (registryObject == null) {
            return ValidateObjectTypeResponse.DOESNT_EXIST;
        } else if (!expectedType.isAssignableFrom(registryObject.getClass())) {
            return ValidateObjectTypeResponse.WRONG_TYPE;
        }

        return ValidateObjectTypeResponse.VALID;
    }
}
