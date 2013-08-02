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

import org.springframework.transaction.support.TransactionTemplate;

import com.raytheon.uf.edex.database.RunnableWithTransaction;
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
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistrySubmitTask extends RunnableWithTransaction {

    /** The Registry Object data access object */
    private RegistryObjectDao dao;

    /** The id of the registry object this task is submitting */
    private RegistryObjectType objectToSubmit;

    public RegistrySubmitTask(TransactionTemplate txTemplate,
            RegistryObjectDao dao, RegistryObjectType objectToSubmit,
            String retrievedFrom) {
        super(txTemplate);
        this.dao = dao;
        this.objectToSubmit = objectToSubmit;
        if (this.objectToSubmit.getSlotByName(EbxmlObjectUtil.HOME_SLOT_NAME) == null) {
            this.objectToSubmit.addSlot(EbxmlObjectUtil.HOME_SLOT_NAME,
                    retrievedFrom);
        }
    }

    @Override
    public void runWithTransaction() {
        RegistryObjectType existingObject = dao.getById(objectToSubmit.getId());
        if (existingObject == null) {
            dao.create(objectToSubmit);
        } else {
            dao.merge(objectToSubmit, existingObject);
        }
    }

}
