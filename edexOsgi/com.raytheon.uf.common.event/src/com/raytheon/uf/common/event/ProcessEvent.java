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
package com.raytheon.uf.common.event;



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
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
@DynamicSerialize
public class ProcessEvent extends Event {

    private static final long serialVersionUID = 1L;

    @DynamicSerializeElement
    private String message;

    @DynamicSerializeElement
    private String pluginName;

    @DynamicSerializeElement
    private String fileName;

    @DynamicSerializeElement
    private double processingTimeMilliseconds;

    @DynamicSerializeElement
    private double processingLatencyMilliseconds;

    public ProcessEvent() {
    }

    @Override
    public String toString() {
        return super.toString() + " : " + getMessage();
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }

    /**
     * @param message
     *            the message to set
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * @param pluginName
     *            the pluginName to set
     */
    public void setPluginName(String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * @return the fileName
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * @param fileName
     *            the fileName to set
     */
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    /**
     * @return the processingTimeMilliseconds
     */
    public double getProcessingTimeMilliseconds() {
        return processingTimeMilliseconds;
    }

    /**
     * @param processingTimeMilliseconds
     *            the processingTimeMilliseconds to set
     */
    public void setProcessingTimeMilliseconds(double processingTimeMilliseconds) {
        this.processingTimeMilliseconds = processingTimeMilliseconds;
    }

    /**
     * @return the processingLatencyMilliseconds
     */
    public double getProcessingLatencyMilliseconds() {
        return processingLatencyMilliseconds;
    }

    /**
     * @param processingLatencyMilliseconds
     *            the processingLatencyMilliseconds to set
     */
    public void setProcessingLatencyMilliseconds(
            double processingLatencyMilliseconds) {
        this.processingLatencyMilliseconds = processingLatencyMilliseconds;
    }

}
