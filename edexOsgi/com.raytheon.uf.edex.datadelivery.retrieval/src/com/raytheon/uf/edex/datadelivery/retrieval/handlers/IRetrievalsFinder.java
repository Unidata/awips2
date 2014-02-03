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



/**
 * Responsible for finding the {@link RetrievalResponseXml} that should be
 * processed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2013 1543       djohnson     Initial creation
 * Jan 30, 2014 2686       dhladky      refactor of retrieval.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public interface IRetrievalsFinder {
    /**
     * Process the requests{@link RetrievalResponseXml}
     * @param RetrievalRequestWrapper
     * @return the {@link RetrievalResponseXml}
     * @throws Exception
     */
    RetrievalResponseXml processRequest(RetrievalRequestWrapper rrw)
            throws Exception;
}
