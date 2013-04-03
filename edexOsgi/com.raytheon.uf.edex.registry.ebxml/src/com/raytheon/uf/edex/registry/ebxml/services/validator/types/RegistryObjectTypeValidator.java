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

import java.util.Set;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.IdentifiableType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;

import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryValidator;
import com.raytheon.uf.edex.registry.ebxml.services.validator.ValidatorTypeManager;

/**
 * Validator implementation for validation of RegistryObjectType objects
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
public class RegistryObjectTypeValidator implements IRegistryValidator {

    @Override
    public void validate(Object object, ValidateObjectsResponse response) {
        ValidatorTypeManager.getInstance().getValidator(IdentifiableType.class)
                .validate(object, response);

        RegistryObjectType regObj = (RegistryObjectType) object;
        verifyVersionInfo(regObj, response);
        verifyClassifications(regObj, response);
    }

    private void verifyVersionInfo(RegistryObjectType regObj,
            ValidateObjectsResponse response) {
        VersionInfoType versionInfo = regObj.getVersionInfo();
        if (versionInfo == null) {
            return;
        }
        ValidatorTypeManager.getInstance()
                .validateObject(versionInfo, response);

    }

    private void verifyClassifications(RegistryObjectType regObj,
            ValidateObjectsResponse response) {
        Set<ClassificationType> classifications = regObj.getClassification();
        if (classifications == null) {
            return;
        }

        for (ClassificationType classType : classifications) {
            ValidatorTypeManager.getInstance().validateObject(classType,
                    response);
        }
    }
}
