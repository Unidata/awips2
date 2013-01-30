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
package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;

/**
 * Completes retrievals.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IRetrievalResponseCompleter {

    /**
     * Interface for a retrieval response status object.
     */
    interface IRetrievalResponseStatus {

        /**
         * Check whether the retrieval succeeded.
         * 
         * @return the succeeded flag
         */
        boolean isSucceeded();
    }

    // Null object implementation
    IRetrievalResponseCompleter NULL = new IRetrievalResponseCompleter() {
        @Override
        public void completeRetrieval(RetrievalRequestRecord retrieval,
                IRetrievalResponseStatus status) {
        }
    };

    /**
     * Complete the specified retrieval.
     * 
     * @param retrieval
     *            the retrieval
     * @param succeeded
     *            true if the retrieval succeeded
     */
    void completeRetrieval(RetrievalRequestRecord retrieval,
            IRetrievalResponseStatus status);

}