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
package com.raytheon.uf.edex.registry.ebxml.services.validator.types;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidationExceptionType;

import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.Property;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.constants.ErrorSeverity;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryValidator;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * Validator implementation for validation of VersionInfo objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/13/12      184        bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class VersionInfoTypeValidator implements IRegistryValidator {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VersionInfoTypeValidator.class);

    private RegistryObjectTypeDao registryObjectDao = new RegistryObjectTypeDao();

    @Override
    public void validate(Object object, ValidateObjectsResponse response) {
        VersionInfoType versionInfo = (VersionInfoType) object;

        DetachedCriteria criteria = DetachedCriteria
                .forClass(VersionInfoType.class);
        criteria.add(Property.forName("versionName").eq(
                versionInfo.getVersionName()));
        criteria.add(Property.forName("userVersionName").eq(
                versionInfo.getUserVersionName()));
        List<VersionInfoType> result = null;
        try {
            result = registryObjectDao.executeCriteriaQuery(criteria);
        } catch (EbxmlRegistryException e) {
            response.getException()
                    .add(EbxmlExceptionUtil
                            .createRegistryException(
                                    ValidationExceptionType.class,
                                    "",
                                    "Validation Failed: Error verifying version number",
                                    "There was an unexpected error encountered while querying the registry to find if a VersionInfoType object was already present",
                                    ErrorSeverity.ERROR, e, statusHandler));
        }
        if (result == null || result.isEmpty()) {
            try {
                registryObjectDao.saveOrUpdate(versionInfo);
            } catch (EbxmlRegistryException e) {
                response.getException()
                        .add(EbxmlExceptionUtil
                                .createRegistryException(
                                        ValidationExceptionType.class,
                                        "",
                                        "Validation Failed: Error verifying version number",
                                        "There was an unexpected error encountered while saving a new VersionInfoType object to the registry",
                                        ErrorSeverity.ERROR, e, statusHandler));
            }
        }
    }

}
