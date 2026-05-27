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
package com.raytheon.uf.edex.registry.federation;

import org.springframework.transaction.support.TransactionTemplate;

import com.raytheon.uf.edex.database.RunnableWithTransaction;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;

/**
 * 
 * A task to remove an object from the registry asynchonously
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/29/2013    2191        bphillip    Initial implementation
 * 5/11/2015    4448        bphillip    Separated EBXML Registry from Data Delivery
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class RegistryRemoveTask extends RunnableWithTransaction {

    /** Data Access object for registry objects */
    private RegistryObjectDao dao;

    /** The id of the registry object this task is removing */
    private String idToRemove;

    /**
     * Creates a new AsyncRemove object
     * 
     * @param txTemplate
     *            The Hibernate transaction template
     * @param lcm
     *            The lifecycle manager
     * @param idToRemove
     *            The ide of the registry object to remove
     */
    public RegistryRemoveTask(TransactionTemplate txTemplate,
            RegistryObjectDao dao, String idToRemove) {
        super(txTemplate);
        this.dao = dao;
    }

    @Override
    public void runWithTransaction() {
        dao.delete(dao.getById(idToRemove));
    }

}
