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
package com.raytheon.uf.edex.database;

import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallbackWithoutResult;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * 
 * Implementation of a runnable who's run method is encapsulated in a Hibernate
 * transaction. This class is used for executing asynchronous actions that
 * require a Hibernate transaction.
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
public abstract class RunnableWithTransaction implements Runnable {

    /** The Hibernate transaction template used to get a transaction */
    private TransactionTemplate txTemplate;

    /**
     * Creates a new RunnableWithTransaction object
     */
    public RunnableWithTransaction() {

    }

    /**
     * Creates a new RunnableWithTransaction object
     * 
     * @param txTemplate
     *            The transaction template to use
     */
    public RunnableWithTransaction(TransactionTemplate txTemplate) {
        this.txTemplate = txTemplate;
    }

    public void run() {
        txTemplate.execute(new TransactionCallbackWithoutResult() {
            @Override
            protected void doInTransactionWithoutResult(TransactionStatus status) {
                runWithTransaction();
            }
        });
    }

    /**
     * This method is executed transactionally
     */
    public abstract void runWithTransaction();

    public void setTxTemplate(TransactionTemplate txTemplate) {
        this.txTemplate = txTemplate;
    }

}
