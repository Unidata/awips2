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
package com.raytheon.uf.common.stats;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Event for file ingest statistics (processing time and processing latency.)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2012  #1292     bgonzale     Initial creation
 * Feb 12, 2013  #1615     bgonzale     Changed pluginName to dataType.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
@DynamicSerialize
public class ProcessEvent extends StatisticsEvent {

    private static final long serialVersionUID = 1L;

    private static final Map<String, String> FIELD_UNIT_MAP;
    static {
        Map<String, String> m = new HashMap<String, String>();
        m.put("processingLatency", "ms");
        m.put("processingTime", "ms");
        FIELD_UNIT_MAP = Collections.unmodifiableMap(m);
    }

    @DynamicSerializeElement
    private String message;

    @DynamicSerializeElement
    private String dataType;

    @DynamicSerializeElement
    private String fileName;

    /*
     * Processing time in milliseconds
     */
    @DynamicSerializeElement
    private long processingTime;

    /*
     * Processing latency in milliseconds
     */
    @DynamicSerializeElement
    private long processingLatency;

    public ProcessEvent() {
    }

    @Override
    protected Map<String, String> getFieldUnitMap() {
        return FIELD_UNIT_MAP;
    }

    /**
     * @return the fileName
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * @return the dataType
     */
    public String getDataType() {
        return dataType;
    }

    /**
     * @return the processingLatency in milliseconds
     */
    public long getProcessingLatency() {
        return processingLatency;
    }

    /**
     * @return the processingTime in milliseconds
     */
    public long getProcessingTime() {
        return processingTime;
    }

    /**
     * @param fileName
     *            the fileName to set
     */
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    /**
     * @param message
     *            the message to set
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * @param dataType
     *            the dataType to set
     */
    public void setDataType(String dataType) {
        this.dataType = dataType;
    }

    /**
     * @param processingLatency
     *            the processingLatency in milliseconds to set
     */
    public void setProcessingLatency(long processingLatency) {
        this.processingLatency = processingLatency;
    }

    /**
     * @param processingTime
     *            the processingTime in milliseconds to set
     */
    public void setProcessingTime(long processingTime) {
        this.processingTime = processingTime;
    }

    @Override
    public String toString() {
        return super.toString() + " : " + getMessage();
    }

    @Override
    public void finalizeEvent() {
        // not implemented
    }

}