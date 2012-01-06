package com.raytheon.uf.common.activetable;

import java.util.List;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

@DynamicSerialize
public class UpdateActiveTableResponse implements ISerializableObject {

    @DynamicSerializeElement
    private List<String> sourceInfo;

    @DynamicSerializeElement
    private String message;

    /**
     * @param sourceInfo
     */
    public void setSourceInfo(List<String> sourceInfo) {
        this.sourceInfo = sourceInfo;
    }

    /**
     * @return
     */
    public List<String> getSourceInfo() {
        return sourceInfo;
    }

    /**
     * @param message
     *            the message to set
     */
    public void setMessage(String message) {
        this.message = message;
    }

    /**
     * @return the message
     */
    public String getMessage() {
        return message;
    }
}
