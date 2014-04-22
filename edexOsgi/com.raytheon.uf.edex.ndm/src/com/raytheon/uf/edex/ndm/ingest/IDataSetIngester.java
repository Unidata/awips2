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
package com.raytheon.uf.edex.ndm.ingest;


/**
 * NDM Dataset ingester interface.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2011            jkorman     Initial creation
 * Mar 06, 2014   2876     mpduff      New NDM plugin.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public interface IDataSetIngester {

    /**
     * Register a filename to be processed and the subscription listener that
     * will process the file.
     * 
     * @param filename
     * @param listener
     * @return
     */
    INationalDatasetSubscriber registerListener(String filename,
            INationalDatasetSubscriber listener);

    /**
     * Remove a subscription listener for a given file.
     * 
     * @param filename
     * @param listener
     * @return
     */
    INationalDatasetSubscriber removeListener(String filename,
            INationalDatasetSubscriber listener);

}
