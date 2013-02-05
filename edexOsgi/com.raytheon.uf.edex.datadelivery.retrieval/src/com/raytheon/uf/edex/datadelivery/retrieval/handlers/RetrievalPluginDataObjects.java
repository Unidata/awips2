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

import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;

/**
 * Associates plugin data objects with a retrieval.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@DynamicSerialize
public class RetrievalPluginDataObjects {

    @DynamicSerializeElement
    private RetrievalRequestRecord requestRecord;

    @DynamicSerializeElement
    private List<RetrievalAttributePluginDataObjects> retrievalAttributePluginDataObjects;

    /**
     * Constructor.
     */
    public RetrievalPluginDataObjects() {
    }

    /**
     * Constructor.
     * 
     * @param requestRecord
     * @param retrievalAttributePluginDataObjects
     */
    public RetrievalPluginDataObjects(
            RetrievalRequestRecord requestRecord,
            List<RetrievalAttributePluginDataObjects> retrievalAttributePluginDataObjects) {
        this.requestRecord = requestRecord;
        this.retrievalAttributePluginDataObjects = retrievalAttributePluginDataObjects;
    }

    /**
     * @return the requestRecord
     */
    public RetrievalRequestRecord getRequestRecord() {
        return requestRecord;
    }

    /**
     * @param requestRecord
     *            the requestRecord to set
     */
    public void setRequestRecord(RetrievalRequestRecord requestRecord) {
        this.requestRecord = requestRecord;
    }

    /**
     * @return the retrievalAttributePluginDataObjects
     */
    public List<RetrievalAttributePluginDataObjects> getRetrievalAttributePluginDataObjects() {
        return retrievalAttributePluginDataObjects;
    }

    /**
     * @param retrievalAttributePluginDataObjects
     *            the retrievalAttributePluginDataObjects to set
     */
    public void setRetrievalAttributePluginDataObjects(
            List<RetrievalAttributePluginDataObjects> retrievalAttributePluginDataObjects) {
        this.retrievalAttributePluginDataObjects = retrievalAttributePluginDataObjects;
    }
}
