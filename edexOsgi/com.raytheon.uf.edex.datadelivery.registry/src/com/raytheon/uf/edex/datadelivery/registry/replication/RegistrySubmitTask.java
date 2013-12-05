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
package com.raytheon.uf.edex.datadelivery.registry.replication;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import org.springframework.transaction.support.TransactionTemplate;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.RunnableWithTransaction;
import com.raytheon.uf.edex.datadelivery.registry.web.DataDeliveryRESTServices;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * 
 * A task to submit an object to the registry asynchonously
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/29/2013    2191        bphillip    Initial implementation
 * 10/30/2013   1538        bphillip    Updated to use non-static rest client
 * 12/2/2013    1829        bphillip    Changed method of adding slots to object
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistrySubmitTask extends RunnableWithTransaction {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistrySubmitTask.class);

    /** The Registry Object data access object */
    private RegistryObjectDao dao;

    /** The id of the registry object this task is submitting */
    private String objectId;

    /** The URL of the remote server to get the object from */
    private String remoteURL;

    private DataDeliveryRESTServices restClient;

    public RegistrySubmitTask(TransactionTemplate txTemplate,
            RegistryObjectDao dao, String objectId, String remoteURL,
            DataDeliveryRESTServices restClient) {
        super(txTemplate);
        this.dao = dao;
        this.objectId = objectId;
        this.remoteURL = remoteURL;
        this.restClient = restClient;

    }

    @Override
    public void runWithTransaction() {
        try {
            RegistryObjectType objectToSubmit = restClient.getRegistryObject(
                    remoteURL, escapeObjectId(objectId));

            if (objectToSubmit.getSlotByName(EbxmlObjectUtil.HOME_SLOT_NAME) == null) {
                objectToSubmit.getSlot().add(
                        new SlotType(EbxmlObjectUtil.HOME_SLOT_NAME,
                                new StringValueType(remoteURL)));
            }

            RegistryObjectType existingObject = dao.getById(objectId);
            if (existingObject == null) {
                dao.create(objectToSubmit);
            } else {
                dao.merge(objectToSubmit, existingObject);
            }

        } catch (Exception e) {
            statusHandler.error("Error retrieving remote object: " + objectId,
                    e);
            return;
        }
    }

    private String escapeObjectId(String objectId) {
        return objectId.replaceAll(":", "%3A").replaceAll("\\/", "%2F");
    }
}
