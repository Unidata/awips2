package com.raytheon.uf.common.dataplugin.text;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@DynamicSerialize
public class RemoteRetrievalResponse {
    @DynamicSerializeElement
    private boolean ok;
    
    @DynamicSerializeElement
    private String statusType;
    
    @DynamicSerializeElement
    private String statusMessage;

    public RemoteRetrievalResponse() {
        
    }
    
    public RemoteRetrievalResponse(boolean ok, String statusType, String statusMessage) {
        this.ok = ok;
        this.statusType = statusType;
        this.statusMessage = statusMessage;
    }

    /**
     * @return the ok
     */
    public boolean isOk() {
        return ok;
    }

    /**
     * @param ok the ok to set
     */
    public void setOk(boolean ok) {
        this.ok = ok;
    }

    /**
     * @return the statusType
     */
    public String getStatusType() {
        return statusType;
    }

    /**
     * @param statusType the statusType to set
     */
    public void setStatusType(String statusType) {
        this.statusType = statusType;
    }

    /**
     * @return the statusMessage
     */
    public String getStatusMessage() {
        return statusMessage;
    }

    /**
     * @param statusMessage the statusMessage to set
     */
    public void setStatusMessage(String statusMessage) {
        this.statusMessage = statusMessage;
    }
}
