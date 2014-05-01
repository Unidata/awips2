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

package com.raytheon.uf.common.alertviz;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/19/2010   5849       cjeanbap    Initial creation
 *                                     
 * </pre>
 * 
 * @author cjeanbap
 * @version 1.0
 */
@DynamicSerialize
public class AlertVizRequest implements IServerRequest {

    @DynamicSerializeElement
    private String message;
    
    @DynamicSerializeElement
    private String machine;
    
    @DynamicSerializeElement
    private String priority;
    
    @DynamicSerializeElement
    private String sourceKey;
    
    @DynamicSerializeElement
    private String category;
    
    @DynamicSerializeElement
    private String audioFile = null;
    
    public AlertVizRequest() {
    }

    public AlertVizRequest(String message, String priority, String sourceKey,
            String category, String audioFile) {
        super();
        this.message = message;
        this.priority = priority;
        this.sourceKey = sourceKey;
        this.category = category;
        this.audioFile = audioFile;
    }

    public String getMachine() {
        return machine;
    }

    public void setMachine(String machine) {
        this.machine = machine;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public String getPriority() {
        return priority;
    }

    public void setPriority(String priority) {
        this.priority = priority;
    }

    public String getSourceKey() {
        return sourceKey;
    }

    public void setSourceKey(String sourceKey) {
        this.sourceKey = sourceKey;
    }

    public String getCategory() {
        return category;
    }

    public void setCategory(String category) {
        this.category = category;
    }

    public String getAudioFile() {
        return audioFile;
    }

    public void setAudioFile(String audioFile) {
        this.audioFile = audioFile;
    }
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        
        sb.append(machine) .append(" | ").append(priority).append(" | ").
            append(category).append(" | ").append(sourceKey);
        
        return sb.toString();
    }
}
