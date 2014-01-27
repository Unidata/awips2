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
package com.raytheon.uf.edex.datadelivery.registry.availability;

import java.util.Calendar;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DateTimeValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;

import org.springframework.transaction.support.TransactionTemplate;

import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.RunnableWithTransaction;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * 
 * Periodically writes a record to the database as long as this registry is
 * connected to the federation. This class is primarily used to determine if,
 * upon startup, this registry must synchronize with the federation to get the
 * state of the registry up to date
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/29/2013    2191        bphillip    Initial implementation
 * 12/2/2013    1829        bphillip    Uses correct getter for getting date time value
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class FederatedRegistryMonitor extends RunnableWithTransaction {

    /** The logger instance */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FederatedRegistryMonitor.class);

    /** ID of the object to write to the registry */
    private static final String REGISTRY_AVAILABLE_ID = "RegistryAvailability";

    /** Data access object for registry objects */
    private RegistryObjectDao registryObjectDao;

    public FederatedRegistryMonitor() {
        super();
    }

    public FederatedRegistryMonitor(TransactionTemplate txTemplate,
            RegistryObjectDao registryObjectDao) {
        super(txTemplate);
        this.registryObjectDao = registryObjectDao;
    }

    public long getLastKnownUptime() {
        RegistryObjectType regObj = registryObjectDao
                .getById(REGISTRY_AVAILABLE_ID);
        Calendar cal = null;
        if (regObj == null) {
            cal = TimeUtil.newCalendar();
            cal.setTimeInMillis(0);
        } else {
            DateTimeValueType value = (DateTimeValueType) regObj.getSlotByName(
                    REGISTRY_AVAILABLE_ID).getSlotValue();
            cal = value.getDateTimeValue().toGregorianCalendar();
        }
        return cal.getTimeInMillis();
    }

    @Override
    public void runWithTransaction() {
        try {
            RegistryObjectType regObj = registryObjectDao
                    .getById(REGISTRY_AVAILABLE_ID);
            if (regObj == null) {
                statusHandler
                        .info("Availability object not found in registry. Creating new entry.");
                SlotType slot = new SlotType();
                slot.setName(REGISTRY_AVAILABLE_ID);
                DateTimeValueType value = new DateTimeValueType();
                value.setDateTimeValue(EbxmlObjectUtil
                        .getCurrentTimeAsXMLGregorianCalendar());
                slot.setSlotValue(value);
                regObj = new RegistryObjectType();
                regObj.setId(REGISTRY_AVAILABLE_ID);
                regObj.setLid(REGISTRY_AVAILABLE_ID);
                regObj.setOwner(RegistryUtil.DEFAULT_OWNER);
                regObj.setVersionInfo(new VersionInfoType());
                regObj.setStatus(StatusTypes.APPROVED);
                regObj.getSlot().add(slot);
                registryObjectDao.create(regObj);
            } else {
                DateTimeValueType dateTime = (DateTimeValueType) regObj
                        .getSlotByName(REGISTRY_AVAILABLE_ID).getSlotValue();
                dateTime.setDateTimeValue(EbxmlObjectUtil
                        .getCurrentTimeAsXMLGregorianCalendar());
                registryObjectDao.update(regObj);
            }

        } catch (EbxmlRegistryException e) {
            statusHandler.error("Error updating federated time!", e);
        }
    }
}
