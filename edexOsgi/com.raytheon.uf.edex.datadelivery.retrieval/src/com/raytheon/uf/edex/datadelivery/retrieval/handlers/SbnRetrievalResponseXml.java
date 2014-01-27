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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecordPK;

/**
 * Associates plugin data objects with their retrieval request. Specifically for
 * SBN retrievals passed to clients from the central registry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2013 2506       bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SbnRetrievalResponseXml extends RetrievalResponseXml {

    @XmlElement
    @DynamicSerializeElement
    private RetrievalRequestRecord retrievalRequestRecord;

    /**
     * Constructor.
     */
    public SbnRetrievalResponseXml() {
    }

    /**
     * Constructor.
     * 
     * @param requestRecord
     * @param retrievalAttributePluginDataObjects
     */
    public SbnRetrievalResponseXml(RetrievalRequestRecordPK requestRecord,
            List<RetrievalResponseWrapper> retrievalAttributePluginDataObjects,
            RetrievalRequestRecord retrievalRequestRecord) {
        super(requestRecord, retrievalAttributePluginDataObjects);
        this.retrievalRequestRecord = retrievalRequestRecord;
    }

    /**
     * Constructor
     * 
     * @param request
     * @param retrievalPluginDataObjects
     */
    SbnRetrievalResponseXml(RetrievalRequestRecord request,
            RetrievalResponseXml retrievalPluginDataObjects) {
        super(retrievalPluginDataObjects);
        this.retrievalRequestRecord = request;
    }

    /**
     * @return the retrievalRequestRecord
     */
    public RetrievalRequestRecord getRetrievalRequestRecord() {
        return retrievalRequestRecord;
    }

    /**
     * @param retrievalRequestRecord
     *            the retrievalRequestRecord to set
     */
    public void setRetrieval(RetrievalRequestRecord retrievalRequestRecord) {
        this.retrievalRequestRecord = retrievalRequestRecord;
    }

}
