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
package com.raytheon.uf.edex.database.processor;

/**
 * Interface for working with a batched set of results inside a database
 * session. Process will be called for each row.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 9, 2013  2555       rjpeter     Initial creation.
 * Jan 23, 2014 2555       rjpeter     Updated to be a row at a time using ScrollableResults.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public interface IDatabaseProcessor<T> {
    /**
     * Perform any processing on this row.
     * 
     * @param row
     * @return True if should continue processing, false otherwise.
     */
    public boolean process(T row);

    /**
     * Perform any post processing if necessary.
     */
    public void finish();

    /**
     * Get the batch size of the query.
     * 
     * @return
     */
    public int getBatchSize();

    /**
     * Set the batch size of the query.
     * 
     * @param batchSize
     */
    public void setBatchSize(int batchSize);
}
