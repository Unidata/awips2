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
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecordPK;

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
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class RetrievalResponseXml {

    @XmlElement
    @DynamicSerializeElement
    private RetrievalRequestRecordPK requestRecord;

    @XmlElements(@XmlElement(name = "retrievalResponseWrapper"))
    @DynamicSerializeElement
    private List<RetrievalResponseWrapper> retrievalAttributePluginDataObjects;

    @XmlAttribute
    @DynamicSerializeElement
    private boolean success;

    /**
     * Constructor.
     */
    public RetrievalResponseXml() {
    }

    /**
     * Constructor.
     * 
     * @param requestRecord
     * @param retrievalAttributePluginDataObjects
     */
    public RetrievalResponseXml(RetrievalRequestRecordPK requestRecord,
            List<RetrievalResponseWrapper> retrievalAttributePluginDataObjects) {
        this.requestRecord = requestRecord;
        this.retrievalAttributePluginDataObjects = retrievalAttributePluginDataObjects;
    }

    /**
     * @return the requestRecord
     */
    public RetrievalRequestRecordPK getRequestRecord() {
        return requestRecord;
    }

    /**
     * @param requestRecord
     *            the requestRecord to set
     */
    public void setRequestRecord(RetrievalRequestRecordPK requestRecord) {
        this.requestRecord = requestRecord;
    }

    /**
     * @return the retrievalAttributePluginDataObjects
     */
    public List<RetrievalResponseWrapper> getRetrievalAttributePluginDataObjects() {
        return retrievalAttributePluginDataObjects;
    }

    /**
     * @param retrievalAttributePluginDataObjects
     *            the retrievalAttributePluginDataObjects to set
     */
    public void setRetrievalAttributePluginDataObjects(
            List<RetrievalResponseWrapper> retrievalAttributePluginDataObjects) {
        this.retrievalAttributePluginDataObjects = retrievalAttributePluginDataObjects;
    }

    /**
     * @return
     */
    public boolean isSuccess() {
        return success;
    }

    /**
     * 
     * @param successful
     */
    public void setSuccess(boolean successful) {
        this.success = successful;
    }

    /**
     * Prepares the object for serialization.
     */
    public void prepareForSerialization() {
        for (RetrievalResponseWrapper attribute : getRetrievalAttributePluginDataObjects()) {
            // Null out the attribute since we can retrieve it from the database
            // on the receiving side
            attribute.getRetrievalResponse().setAttribute(null);
        }
    }
}
